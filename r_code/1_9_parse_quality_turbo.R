# ============================================================
# TURBO re-parse (Apple Silicon / macOS)
# - Reads ONLY href + project_status from parent dataset
# - Parses ONLY: "Рейтинг заявки" (winner-info__list-item)
# - Processes ONLY rows with project_status == 1
# - Appends to a NEW CSV
# - Per-chunk backups that do NOT overwrite across restarts
# ============================================================

pacman::p_load(
  dplyr, stringr, stringi, readr, purrr,
  curl,
  future, furrr,
  data.table, tibble
)

# ---------------------------
# CONFIG (EDIT PATHS)
# ---------------------------
base_url    <- "https://xn--80afcdbalict6afooklqi5o.xn--p1ai"

# Parent dataset MUST have: href, project_status
input_file  <- "data/data_large/presi_variables_imputed.csv"

# NEW output dataset
output_file <- "data/data_large/application_rating.csv"

# Chunk backup folder
backup_dir  <- "data/data_large/backups_rating"
dir.create(backup_dir, recursive = TRUE, showWarnings = FALSE)

# ---------------------------
# SPEED / ROBUSTNESS KNOBS
# ---------------------------
workers <- 6
chunk_size <- 500
max_retries <- 2
jitter_sleep <- c(0.0001, 0.01)
furrr_scheduling <- 1
options(future.globals.maxSize = 2 * 1024^3)

# ---------------------------
# HELPERS
# ---------------------------
`%+%` <- function(a, b) paste0(a, b)

str_squish2 <- function(x) stringr::str_squish(stringi::stri_trans_nfkc(x))

make_url <- function(href) {
  href <- str_squish2(href)
  if (stringr::str_detect(href, "^https?://")) return(href)
  if (!stringr::str_detect(href, "^/")) href <- paste0("/", href)
  paste0(base_url, href)
}

append_csv_fast <- function(df, path) {
  data.table::fwrite(df, path, append = file.exists(path), col.names = !file.exists(path))
}

get_next_chunk_index <- function(backup_dir) {
  files <- list.files(backup_dir, pattern = "^reparsed_rating_chunk_\\d{5}\\.csv$", full.names = FALSE)
  if (length(files) == 0) return(1L)
  nums <- suppressWarnings(as.integer(stringr::str_match(files, "chunk_(\\d{5})\\.csv$")[,2]))
  nums <- nums[!is.na(nums)]
  if (length(nums) == 0) return(1L)
  max(nums) + 1L
}

# ---------------------------
# RAW -> TEXT SAFE DECODER
# ---------------------------
raw_to_text_safe <- function(raw_vec) {
  if (is.null(raw_vec) || length(raw_vec) == 0) return(NA_character_)
  
  if (length(raw_vec) >= 2 &&
      identical(raw_vec[1], as.raw(0x1f)) &&
      identical(raw_vec[2], as.raw(0x8b))) {
    raw_vec <- tryCatch(memDecompress(raw_vec, type = "gzip"), error = function(e) raw_vec)
  }
  
  raw_vec <- raw_vec[raw_vec != as.raw(0x00)]
  if (length(raw_vec) == 0) return(NA_character_)
  
  txt <- tryCatch(rawToChar(raw_vec), error = function(e) NA_character_)
  if (is.na(txt)) {
    pieces <- tryCatch(rawToChar(raw_vec, multiple = TRUE), error = function(e) character())
    if (length(pieces) == 0) return(NA_character_)
    txt <- paste0(pieces, collapse = "")
  }
  
  if (length(txt) != 1) txt <- paste0(txt, collapse = "")
  txt
}

# ---------------------------
# FAST FETCH (curl)
# ---------------------------
fetch_html <- function(url, max_retries = 2) {
  h <- curl::new_handle()
  cookie_path <- file.path(tempdir(), "presi_cookiejar.txt")
  
  curl::handle_setopt(
    h,
    followlocation = 1L,
    maxredirs = 8L,
    timeout = 35L,
    connecttimeout = 10L,
    cookiejar = cookie_path,
    cookiefile = cookie_path
  )
  
  curl::handle_setheaders(
    h,
    "User-Agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.2 Safari/605.1.15",
    "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
    "Accept-Language" = "ru-RU,ru;q=0.9,en-US;q=0.7,en;q=0.6",
    "Upgrade-Insecure-Requests" = "1",
    "Referer" = base_url
  )
  
  last_status <- NA_integer_
  last_err <- NA_character_
  
  for (i in seq_len(max_retries)) {
    out <- tryCatch(
      curl::curl_fetch_memory(url, handle = h),
      error = function(e) {
        last_err <<- conditionMessage(e)
        return(NULL)
      }
    )
    
    if (is.null(out)) {
      Sys.sleep(min(6, 0.5 * 2^(i - 1)))
      next
    }
    
    last_status <- out$status_code
    
    if (!is.null(out$content) && last_status == 200) {
      html <- raw_to_text_safe(out$content)
      return(list(html = html, status = last_status, error = NA_character_))
    }
    
    if (last_status %in% c(403, 429, 500, 502, 503, 504)) {
      Sys.sleep(min(15, 1.2 * 2^(i - 1)))
      next
    } else {
      break
    }
  }
  
  list(html = NA_character_, status = last_status, error = last_err)
}

# ---------------------------
# PARSE "Рейтинг заявки"
# ---------------------------
parse_rating_ru <- function(x) {
  if (is.na(x) || !nzchar(x)) return(NA_real_)
  x <- str_squish2(x)
  x <- stringr::str_replace_all(x, "\u00A0", " ")
  x <- stringr::str_replace_all(x, "&nbsp;", " ")
  readr::parse_number(x, locale = readr::locale(decimal_mark = ",", grouping_mark = " "))
}

extract_fields <- function(html) {
  tryCatch({
    if (is.na(html) || !nzchar(html)) return(list(application_rating = NA_real_))
    
    x <- stringi::stri_trans_nfkc(html)
    x <- stringr::str_replace_all(x, "&nbsp;", " ")
    x <- stringr::str_squish(x)
    
    rating_raw <- stringr::str_match(
      x,
      "(?is)<li[^>]*\\bwinner-info__list-item\\b[^>]*>.*?" %+%
        "<span[^>]*\\bwinner-info__list-item-title\\b[^>]*>\\s*Рейтинг\\s+заявки\\s*</span>.*?" %+%
        "<span[^>]*\\bwinner-info__list-item-text\\b[^>]*>(.*?)</span>"
    )[,2]
    
    if (is.na(rating_raw)) return(list(application_rating = NA_real_))
    
    rating_clean <- rating_raw |>
      stringr::str_remove_all("<[^>]+>") |>
      str_squish2()
    
    list(application_rating = parse_rating_ru(rating_clean))
  }, error = function(e) {
    list(application_rating = NA_real_)
  })
}

parse_one <- function(href) {
  Sys.sleep(stats::runif(1, jitter_sleep[1], jitter_sleep[2]))
  
  url <- make_url(href)
  fetched <- fetch_html(url, max_retries = max_retries)
  fields <- extract_fields(fetched$html)
  
  tibble(
    href = href,
    application_rating = fields$application_rating,
    status_code = fetched$status,
    fetch_error = fetched$error,
    parsed_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
}

# ============================================================
# RUN
# ============================================================

if (!file.exists(input_file)) stop("input_file not found: ", input_file)

parent_df <- readr::read_csv(
  input_file,
  col_types = cols(
    .default = col_skip(),
    href = col_character(),
    project_status = col_double()
  ),
  show_col_types = FALSE
)

hrefs <- parent_df %>%
  filter(!is.na(href), href != "", href != "NA") %>%
  filter(!is.na(project_status), project_status == 1) %>%
  distinct(href) %>%
  pull(href)

if (file.exists(output_file)) {
  prev <- tryCatch(
    data.table::fread(
      output_file,
      showProgress = FALSE,
      select = c("href", "status_code", "application_rating")
    ),
    error = function(e) NULL
  )
  
  if (!is.null(prev) && "href" %in% names(prev)) {
    done <- prev[(status_code == 200) & (!is.na(application_rating)), unique(href)]
    hrefs <- setdiff(hrefs, done)
  }
}

message("Hrefs to parse (remaining, project_status==1): ", length(hrefs))
message("Workers: ", workers,
        " | chunk_size: ", chunk_size,
        " | retries: ", max_retries,
        " | jitter: ", paste0(jitter_sleep, collapse = "-"),
        " | scheduling: ", furrr_scheduling)

if (length(hrefs) == 0) {
  message("Nothing to do. Output already complete: ", output_file)
  quit(save = "no")
}

chunk_start <- get_next_chunk_index(backup_dir)
message("Backup chunk numbering will start at: ", sprintf("%05d", chunk_start))

chunks <- split(hrefs, ceiling(seq_along(hrefs) / chunk_size))

future::plan(future::multisession, workers = workers)
furrr_opts <- furrr::furrr_options(seed = TRUE, scheduling = furrr_scheduling)

for (i in seq_along(chunks)) {
  ch <- chunks[[i]]
  message("\n--- Chunk ", i, "/", length(chunks), " (n=", length(ch), ") ---")
  
  res_list <- furrr::future_map(ch, parse_one, .options = furrr_opts)
  res <- data.table::rbindlist(res_list, use.names = TRUE, fill = TRUE)
  
  append_csv_fast(res, output_file)
  
  chunk_idx <- chunk_start + i - 1L
  backup_path <- file.path(backup_dir, sprintf("reparsed_rating_chunk_%05d.csv", chunk_idx))
  data.table::fwrite(res, backup_path)
  
  ok200 <- sum(res$status_code == 200, na.rm = TRUE)
  blocked403 <- sum(res$status_code == 403, na.rm = TRUE)
  blocked429 <- sum(res$status_code == 429, na.rm = TRUE)
  any_rating <- sum(!is.na(res$application_rating))
  
  message("Chunk stats: 200=", ok200, "/", nrow(res),
          " | 403=", blocked403,
          " | 429=", blocked429,
          " | any_rating=", any_rating,
          " | backup saved: ", backup_path)
  
  if (ok200 == 0 && (blocked403 + blocked429) >= floor(0.9 * nrow(res))) {
    message("Looks blocked. Cooling down 60s...")
    Sys.sleep(60)
  }
  
  gc()
}

future::plan(future::sequential)
message("\nDONE.\nMain output: ", output_file, "\nBackups in: ", backup_dir)