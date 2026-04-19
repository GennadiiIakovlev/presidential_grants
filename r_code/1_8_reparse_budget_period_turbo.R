# ============================================================
# TURBO re-parse (Apple Silicon / macOS)
# - Reads ONLY href from your final dataset
# - Parallel chunked parse with low overhead
# - Appends to a NEW CSV
# - Per-chunk backups that do NOT overwrite across restarts
# ============================================================

pacman::p_load(
  dplyr, stringr, stringi, readr, purrr,
  curl,
  future, furrr,
  data.table
)

# ---------------------------
# CONFIG (EDIT PATHS)
# ---------------------------
base_url    <- "https://xn--80afcdbalict6afooklqi5o.xn--p1ai"

# FINAL dataset with hrefs (only href is read from it)
input_file  <- "data/data_large/presi_variables_imputed.csv"

# NEW output dataset (separate file)
output_file <- "data/data_large/reparsed_budget_period.csv"

# Chunk backup folder
backup_dir  <- "data/data_large/reparsed_backups"
dir.create(backup_dir, recursive = TRUE, showWarnings = FALSE)

# ---------------------------
# SPEED / ROBUSTNESS KNOBS
# ---------------------------
cores <- parallel::detectCores(logical = TRUE)

# If you are getting 403/429: DO NOT raise workers; lower it.
workers <- 6
chunk_size <- 500
max_retries <- 2

# Random jitter per request (reduces WAF patterns)
jitter_sleep <- c(0.001, 0.10)

# Furrr scheduling: 1 = less bursty; Inf = fastest but more bursty
furrr_scheduling <- 1

options(future.globals.maxSize = 2 * 1024^3)

# ---------------------------
# HELPERS
# ---------------------------
`%+%` <- function(a, b) paste0(a, b)

str_squish2 <- function(x) stringr::str_squish(stringi::stri_trans_nfkc(x))
parse_money_ru <- function(x) {
  if (is.na(x) || !nzchar(x)) return(NA_real_)
  x <- str_squish2(x)
  x <- stringr::str_replace_all(x, "\u00A0", " ")
  x <- stringr::str_replace_all(x, "&nbsp;", " ")
  
  # remove currency letters/symbols and dots
  x <- stringr::str_replace_all(x, "[₽рРуб\\.]", " ")
  
  readr::parse_number(
    x,
    locale = readr::locale(decimal_mark = ",", grouping_mark = " ")
  )
}
parse_dmy_ru <- function(x) {
  if (is.na(x) || !nzchar(x)) return(as.Date(NA))
  as.Date(x, format = "%d.%m.%Y")
}

make_url <- function(href) {
  href <- str_squish2(href)
  if (stringr::str_detect(href, "^https?://")) return(href)
  if (!stringr::str_detect(href, "^/")) href <- paste0("/", href)
  paste0(base_url, href)
}

append_csv_fast <- function(df, path) {
  data.table::fwrite(df, path, append = file.exists(path), col.names = !file.exists(path))
}

# Continue backup chunk numbering across restarts
get_next_chunk_index <- function(backup_dir) {
  files <- list.files(backup_dir, pattern = "^reparsed_budget_period_chunk_\\d{5}\\.csv$", full.names = FALSE)
  if (length(files) == 0) return(1L)
  nums <- suppressWarnings(as.integer(stringr::str_match(files, "chunk_(\\d{5})\\.csv$")[,2]))
  nums <- nums[!is.na(nums)]
  if (length(nums) == 0) return(1L)
  max(nums) + 1L
}

# ---------------------------
# RAW -> TEXT SAFE DECODER
# - fixes: "embedded nul in string"
# - also ungzips if payload is gzipped (rare, but happens)
# - guarantees length-1 character output
# ---------------------------
raw_to_text_safe <- function(raw_vec) {
  if (is.null(raw_vec) || length(raw_vec) == 0) return(NA_character_)
  
  # If gzipped (magic bytes 1F 8B), try to decompress
  if (length(raw_vec) >= 2 && identical(raw_vec[1], as.raw(0x1f)) && identical(raw_vec[2], as.raw(0x8b))) {
    raw_vec <- tryCatch(memDecompress(raw_vec, type = "gzip"), error = function(e) raw_vec)
  }
  
  # Strip NUL bytes that break rawToChar()
  raw_vec <- raw_vec[raw_vec != as.raw(0x00)]
  if (length(raw_vec) == 0) return(NA_character_)
  
  txt <- tryCatch(rawToChar(raw_vec), error = function(e) NA_character_)
  if (is.na(txt)) {
    pieces <- tryCatch(rawToChar(raw_vec, multiple = TRUE), error = function(e) character())
    if (length(pieces) == 0) return(NA_character_)
    txt <- paste0(pieces, collapse = "")
  }
  
  # Force scalar
  if (length(txt) != 1) txt <- paste0(txt, collapse = "")
  txt
}

# ---------------------------
# FAST FETCH (curl)
# - cookies + real Safari UA
# - follow redirects
# - backoff on 403/429/5xx
# - NUL-safe decoding
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
# FAST EXTRACT (regex)
# Handles nested <span class="rubl">₽</span> inside number span
# ---------------------------
extract_fields <- function(html) {
  # hard fail-safe: NEVER crash caller
  tryCatch({
    if (is.na(html) || !nzchar(html)) {
      return(list(
        sum_requested = NA_real_,
        cofinancing = NA_real_,
        total_expenses = NA_real_,
        implementation_period = NA_character_,
        implementation_start = as.Date(NA),
        implementation_end = as.Date(NA)
      ))
    }
    
    x <- stringi::stri_trans_nfkc(html)
    x <- stringr::str_replace_all(x, "&nbsp;", " ")
    x <- stringr::str_squish(x)
    
    # --- MONEY ---
    m <- stringr::str_match_all(
      x,
      "(?is)<li[^>]*\\bcircle-bar__info-item\\b[^>]*>.*?" %+%
        "<span[^>]*\\bcircle-bar__info-item-title\\b[^>]*>(.*?)</span>.*?" %+%
        "<span[^>]*\\bcircle-bar__info-item-number\\b[^>]*>(.*?)</span>"
    )[[1]]
    
    sum_requested  <- NA_real_
    cofinancing    <- NA_real_
    total_expenses <- NA_real_
    
    if (nrow(m) > 0) {
      titles_raw <- m[,2]
      nums_raw   <- m[,3]
      
      titles <- titles_raw |>
        stringr::str_remove_all("<[^>]+>") |>
        str_squish2() |>
        (\(z) stringr::str_replace_all(z, "C", "С"))() |>
        stringr::str_to_lower()
      
      nums_clean <- nums_raw |>
        stringr::str_remove_all("<[^>]+>") |>
        str_squish2()
      
      vals <- purrr::map_dbl(nums_clean, parse_money_ru)
      
      idx_req <- which(stringr::str_detect(titles, "запраш"))[1]
      idx_cof <- which(stringr::str_detect(titles, "софинанс"))[1]
      idx_tot <- which(stringr::str_detect(titles, "общая сумма расходов"))[1]
      
      if (!is.na(idx_req)) sum_requested  <- vals[idx_req]
      if (!is.na(idx_cof)) cofinancing    <- vals[idx_cof]
      if (!is.na(idx_tot)) total_expenses <- vals[idx_tot]
      
      # Fallback by position
      if (is.na(sum_requested)  && length(vals) >= 1) sum_requested  <- vals[1]
      if (is.na(cofinancing)    && length(vals) >= 2) cofinancing    <- vals[2]
      if (is.na(total_expenses) && length(vals) >= 3) total_expenses <- vals[3]
      
      # Logical fallback
      if (is.na(total_expenses) && !is.na(sum_requested) && !is.na(cofinancing)) {
        total_expenses <- sum_requested + cofinancing
      }
    }
    
    # --- PERIOD ---
    period_raw <- stringr::str_match(
      x,
      "(?is)Сроки реализации\\s*</span>.*?<span[^>]*winner-info__list-item-text[^>]*>\\s*([^<]+?)\\s*<"
    )[,2]
    
    period_raw <- ifelse(is.na(period_raw), NA_character_, str_squish2(period_raw))
    
    dates <- if (!is.na(period_raw)) {
      stringr::str_extract_all(period_raw, "\\d{2}\\.\\d{2}\\.\\d{4}")[[1]]
    } else character()
    
    start_d <- if (length(dates) >= 1) parse_dmy_ru(dates[1]) else as.Date(NA)
    end_d   <- if (length(dates) >= 2) parse_dmy_ru(dates[2]) else as.Date(NA)
    
    list(
      sum_requested = sum_requested,
      cofinancing = cofinancing,
      total_expenses = total_expenses,
      implementation_period = period_raw,
      implementation_start = start_d,
      implementation_end = end_d
    )
  }, error = function(e) {
    list(
      sum_requested = NA_real_,
      cofinancing = NA_real_,
      total_expenses = NA_real_,
      implementation_period = NA_character_,
      implementation_start = as.Date(NA),
      implementation_end = as.Date(NA)
    )
  })
}

# ---------------------------
# One href
# ---------------------------
parse_one <- function(href) {
  Sys.sleep(stats::runif(1, jitter_sleep[1], jitter_sleep[2]))
  
  url <- make_url(href)
  fetched <- fetch_html(url, max_retries = max_retries)
  fields <- extract_fields(fetched$html)
  
  tibble(
    href = href,
    sum_requested = fields$sum_requested,
    cofinancing = fields$cofinancing,
    total_expenses = fields$total_expenses,
    implementation_period = fields$implementation_period,
    implementation_start = as.character(fields$implementation_start),
    implementation_end   = as.character(fields$implementation_end),
    status_code = fetched$status,
    fetch_error = fetched$error,
    parsed_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
}

# ============================================================
# RUN
# ============================================================

if (!file.exists(input_file)) stop("input_file not found: ", input_file)

# Read ONLY href (memory light)
hrefs <- readr::read_csv(
  input_file,
  col_types = cols_only(href = col_character()),
  show_col_types = FALSE
) %>%
  distinct(href) %>%
  filter(!is.na(href), href != "", href != "NA") %>%
  pull(href)

# RESUME: skip only successfully parsed hrefs (status 200 + any money present)
# (fread only needed columns, so it stays fast even if output is big)
if (file.exists(output_file)) {
  prev <- tryCatch(
    data.table::fread(
      output_file,
      showProgress = FALSE,
      select = c("href", "status_code", "sum_requested", "cofinancing", "total_expenses")
    ),
    error = function(e) NULL
  )
  
  if (!is.null(prev) && "href" %in% names(prev)) {
    done <- prev[
      (status_code == 200) &
        (!is.na(sum_requested) | !is.na(cofinancing) | !is.na(total_expenses)),
      unique(href)
    ]
    hrefs <- setdiff(hrefs, done)
  }
}

message("Hrefs to parse (remaining): ", length(hrefs))
message("Workers: ", workers,
        " | chunk_size: ", chunk_size,
        " | retries: ", max_retries,
        " | jitter: ", paste0(jitter_sleep, collapse = "-"),
        " | scheduling: ", furrr_scheduling)

if (length(hrefs) == 0) {
  message("Nothing to do. Output already complete: ", output_file)
  quit(save = "no")
}

# Determine next chunk index for backups (prevents overwrites across restarts)
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
  
  # Append main output
  append_csv_fast(res, output_file)
  
  # Backup filename index continues from existing files
  chunk_idx <- chunk_start + i - 1L
  backup_path <- file.path(backup_dir, sprintf("reparsed_budget_period_chunk_%05d.csv", chunk_idx))
  data.table::fwrite(res, backup_path)
  
  ok200 <- sum(res$status_code == 200, na.rm = TRUE)
  blocked403 <- sum(res$status_code == 403, na.rm = TRUE)
  blocked429 <- sum(res$status_code == 429, na.rm = TRUE)
  any_money <- sum(!is.na(res$sum_requested) | !is.na(res$cofinancing) | !is.na(res$total_expenses))
  
  message("Chunk stats: 200=", ok200, "/", nrow(res),
          " | 403=", blocked403,
          " | 429=", blocked429,
          " | any_money=", any_money,
          " | backup saved: ", backup_path)
  
  # If the whole chunk is blocked, pause hard (helps if WAF is cooling down)
  if (ok200 == 0 && (blocked403 + blocked429) >= floor(0.9 * nrow(res))) {
    message("Looks blocked. Cooling down 60s...")
    Sys.sleep(60)
  }
  
  gc()
}

future::plan(future::sequential)
message("\nDONE.\nMain output: ", output_file, "\nBackups in: ", backup_dir)