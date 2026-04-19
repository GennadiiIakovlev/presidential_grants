pacman::p_load(rvest, httr, dplyr, purrr, readr, stringr, stringi, progress)

# ---------------------------
# Paths (EDIT THESE TWO)
# ---------------------------
base_url   <- "https://xn--80afcdbalict6afooklqi5o.xn--p1ai"

# your FINAL dataset (where hrefs already exist)
input_file <- "data/data_large/presi_variables_imputed.csv"

# new output dataset (separate file)
output_file <- "data/reparsed_budget_period.csv"

# Aggression dial (set c(0,0) to hammer; may trigger rate limits)
sleep_range <- c(0, 0)

# Write in batches to avoid memory bloat + reduce IO overhead
batch_size <- 200

# ---------------------------
# Helpers
# ---------------------------
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a)) a else b

str_squish2 <- function(x) stringr::str_squish(stringi::stri_trans_nfkc(x))

normalize_label <- function(x) {
  x <- str_squish2(x)
  # site uses Latin 'C' sometimes in "Cофинансирование" -> normalize to Cyrillic 'С'
  x <- stringr::str_replace_all(x, "C", "С")
  stringr::str_to_lower(x)
}

parse_money_ru <- function(x) {
  # "1 515 679,40 ₽" -> 1515679.40
  x <- str_squish2(x)
  x <- stringr::str_replace_all(x, "\u00A0", " ")               # NBSP -> space
  x <- stringr::str_replace_all(x, "[₽рРуб\\.]", " ")           # currency junk
  readr::parse_number(x, locale = readr::locale(decimal_mark = ",", grouping_mark = " "))
}

parse_dmy_ru <- function(x) {
  if (is.na(x) || !nzchar(x)) return(as.Date(NA))
  as.Date(x, format = "%d.%m.%Y")
}

append_csv <- function(df, path) {
  write.table(
    df, file = path, sep = ",",
    row.names = FALSE,
    col.names = !file.exists(path),
    append = file.exists(path),
    quote = TRUE,
    fileEncoding = "UTF-8"
  )
}

make_url <- function(href) {
  href <- str_squish2(href)
  if (stringr::str_detect(href, "^https?://")) return(href)
  paste0(base_url, href)
}

fetch_html <- function(url, tries = 3) {
  # slightly more robust than rvest::read_html(url) for flaky connections
  ua <- httr::user_agent("Mozilla/5.0 (compatible; presigranti-scraper/1.0)")
  for (i in seq_len(tries)) {
    resp <- tryCatch(
      httr::GET(url, ua, httr::timeout(60)),
      error = function(e) NULL
    )
    if (!is.null(resp) && httr::status_code(resp) == 200) {
      txt <- httr::content(resp, as = "text", encoding = "UTF-8")
      return(rvest::read_html(txt))
    }
    # if blocked / rate limited, backoff a bit
    Sys.sleep(min(5, i * 1.5))
  }
  NULL
}

# ---------------------------
# Load ONLY hrefs from FINAL dataset (memory-light)
# ---------------------------
# readr col_select works in recent readr; for max compatibility use cols_only:
hrefs <- readr::read_csv(
  input_file,
  col_types = readr::cols_only(href = readr::col_character()),
  show_col_types = FALSE
) %>%
  dplyr::filter(!is.na(href), href != "", href != "NA") %>%
  dplyr::distinct(href) %>%
  dplyr::pull(href)

# skip already parsed hrefs (so you can rerun safely)
if (file.exists(output_file)) {
  done <- tryCatch(
    readr::read_csv(output_file, col_types = readr::cols_only(href = readr::col_character()),
                    show_col_types = FALSE) %>%
      dplyr::distinct(href) %>%
      dplyr::pull(href),
    error = function(e) character()
  )
  hrefs <- setdiff(hrefs, done)
}

message("Hrefs to parse: ", length(hrefs))

# ---------------------------
# Core parser for one href
# ---------------------------
parse_application_budget_period <- function(href) {
  url <- make_url(href)
  
  doc <- fetch_html(url, tries = 3)
  if (is.null(doc)) {
    return(tibble::tibble(
      href = href,
      cofinancing = NA_real_,
      total_expenses = NA_real_,
      implementation_period = NA_character_,
      implementation_start = NA_character_,
      implementation_end = NA_character_,
      parsed_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    ))
  }
  
  # ---- Budget block (circle) ----
  budget_tbl <- doc %>%
    rvest::html_elements(".circle-bar__info-item") %>%
    purrr::map_dfr(function(node) {
      title <- node %>% rvest::html_element(".circle-bar__info-item-title")  %>% rvest::html_text(trim = TRUE)
      num   <- node %>% rvest::html_element(".circle-bar__info-item-number") %>% rvest::html_text(trim = TRUE)
      tibble::tibble(
        label = normalize_label(title %||% ""),
        value_num = parse_money_ru(num %||% "")
      )
    })
  
  cofinancing <- budget_tbl %>%
    dplyr::filter(stringr::str_detect(label, "софинансирование")) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::pull(value_num)
  if (length(cofinancing) == 0) cofinancing <- NA_real_
  
  total_expenses <- budget_tbl %>%
    dplyr::filter(stringr::str_detect(label, "общая сумма расходов")) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::pull(value_num)
  if (length(total_expenses) == 0) total_expenses <- NA_real_
  
  # ---- Implementation period ----
  period_tbl <- doc %>%
    rvest::html_elements(".winner-info__list-item") %>%
    purrr::map_dfr(function(node) {
      title <- node %>% rvest::html_element(".winner-info__list-item-title") %>% rvest::html_text(trim = TRUE)
      txt   <- node %>% rvest::html_element(".winner-info__list-item-text")  %>% rvest::html_text(trim = TRUE)
      tibble::tibble(
        title = normalize_label(title %||% ""),
        text_raw = str_squish2(txt %||% "")
      )
    })
  
  implementation_period <- period_tbl %>%
    dplyr::filter(title == normalize_label("Сроки реализации")) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::pull(text_raw)
  if (length(implementation_period) == 0 || is.na(implementation_period) || !nzchar(implementation_period)) {
    implementation_period <- NA_character_
  }
  
  dates <- if (!is.na(implementation_period)) {
    stringr::str_extract_all(implementation_period, "\\d{2}\\.\\d{2}\\.\\d{4}")[[1]]
  } else character()
  
  start_date <- if (length(dates) >= 1) dates[1] else NA_character_
  end_date   <- if (length(dates) >= 2) dates[2] else NA_character_
  
  tibble::tibble(
    href = href,
    cofinancing = cofinancing,
    total_expenses = total_expenses,
    implementation_period = implementation_period,
    implementation_start = ifelse(is.na(start_date), NA_character_, format(parse_dmy_ru(start_date), "%Y-%m-%d")),
    implementation_end   = ifelse(is.na(end_date), NA_character_, format(parse_dmy_ru(end_date), "%Y-%m-%d")),
    parsed_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
}

# ---------------------------
# Run (batch append)
# ---------------------------
pb <- progress::progress_bar$new(
  format = "  :current/:total [:bar] :percent eta: :eta",
  total = length(hrefs),
  clear = FALSE, width = 70
)

buf <- vector("list", 0L)

for (i in seq_along(hrefs)) {
  href <- hrefs[i]
  pb$tick()
  
  res <- parse_application_budget_period(href)
  buf[[length(buf) + 1L]] <- res
  
  # flush batch
  if (length(buf) >= batch_size) {
    append_csv(dplyr::bind_rows(buf), output_file)
    buf <- vector("list", 0L)
    gc()
  }
  
  if (sleep_range[2] > 0) Sys.sleep(stats::runif(1, sleep_range[1], sleep_range[2]))
}

# flush leftovers
if (length(buf) > 0) {
  append_csv(dplyr::bind_rows(buf), output_file)
  gc()
}

message("DONE. Output: ", output_file)

