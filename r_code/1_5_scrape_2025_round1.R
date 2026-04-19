suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(tibble)
  library(rvest)
  library(xml2)
})

`%||%` <- function(a, b) if (is.null(a)) b else a
is_empty <- function(x) is.na(x) | str_trim(x) == ""

# ----------------------------
# Config
# ----------------------------
base_url <- "https://xn--80afcdbalict6afooklqi5o.xn--p1ai"

projects_file <- "data/data_large/all_projects_data_backup.csv"
out_file      <- "data/data_large/application_results_2025_r1.csv"
log_file      <- "data/data_large/scrape_status_2025_r1.log"

sleep_seconds <- 0.00000000000000025
max_retries   <- 4
base_backoff  <- 0.00007
flush_every   <- 5

# ----------------------------
# Helpers
# ----------------------------
log_msg <- function(...) {
  msg <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", paste(..., collapse = " "))
  cat(msg, "\n")
  cat(msg, "\n", file = log_file, append = TRUE)
}

safe_write_csv <- function(df, path) {
  tmp <- paste0(path, ".tmp")
  write_csv(df, tmp)
  file.rename(tmp, path)
}

pick_latest_sibling <- function(canonical_path) {
  dir  <- dirname(canonical_path)
  base <- tools::file_path_sans_ext(basename(canonical_path))
  ext  <- tools::file_ext(canonical_path)
  
  if (!dir.exists(dir)) return(canonical_path)
  
  candidates <- list.files(
    dir,
    pattern = paste0("^", base, ".*\\.", ext, "$"),
    full.names = TRUE
  )
  
  if (length(candidates) == 0) return(canonical_path)
  
  info <- file.info(candidates)
  rownames(info)[which.max(info$mtime)]
}

pick_resume_file <- function(canonical_path) {
  if (file.exists(canonical_path)) return(canonical_path)
  pick_latest_sibling(canonical_path)
}

fetch_html <- function(url) {
  attempt <- 0
  while (attempt < max_retries) {
    attempt <- attempt + 1
    out <- tryCatch(read_html(url), error = function(e) e)
    if (!inherits(out, "error")) return(out)
    
    wait <- base_backoff * (2^(attempt - 1))
    log_msg("ERROR fetching:", url, "| attempt", attempt, "| waiting", sprintf("%.2fs", wait))
    Sys.sleep(wait)
  }
  NULL
}

parse_number_rub <- function(x) {
  if (is.na(x)) return(NA_real_)
  readr::parse_number(
    x,
    locale = readr::locale(grouping_mark = " ", decimal_mark = ",")
  )
}

# force stable types (especially fetched_at as character)
normalize_results_types <- function(df) {
  df %>%
    mutate(
      href = as.character(href),
      url = as.character(url),
      status_text = as.character(status_text),
      grant_amount_rub = suppressWarnings(as.double(grant_amount_rub)),
      fetched_at = as.character(fetched_at),
      ok = as.logical(ok)
    )
}

# robust upsert (NO old[idx,] <- ...)
upsert_by_href <- function(old, batch) {
  old   <- normalize_results_types(old)
  batch <- normalize_results_types(batch)
  
  batch <- batch %>%
    arrange(href, fetched_at) %>%
    group_by(href) %>%
    slice_tail(n = 1) %>%
    ungroup()
  
  old_no <- old %>% filter(!href %in% batch$href)
  bind_rows(old_no, batch)
}

# ----------------------------
# Exact selectors
# ----------------------------
extract_status_text <- function(html) {
  node <- html %>% html_element("p.winner-info__status span")
  if (length(node) == 0 || inherits(node, "xml_missing")) return(NA_character_)
  txt <- node %>% html_text(trim = TRUE) %>% str_squish()
  if (is_empty(txt)) NA_character_ else txt
}

extract_grant_amount_rub <- function(html) {
  titles <- html %>%
    html_elements(".circle-bar__info-item-title") %>%
    html_text(trim = TRUE) %>%
    str_replace_all("\u00A0", " ") %>%
    str_squish()
  
  nums <- html %>%
    html_elements(".circle-bar__info-item-number") %>%
    html_text(trim = TRUE) %>%
    str_replace_all("\u00A0", " ") %>%
    str_squish()
  
  if (length(titles) == 0 || length(nums) == 0) return(NA_real_)
  
  n <- min(length(titles), length(nums))
  titles <- titles[seq_len(n)]
  nums   <- nums[seq_len(n)]
  
  key <- str_to_lower(titles)
  idx <- which(str_detect(key, "размер\\s+гранта"))
  if (length(idx) == 0) return(NA_real_)
  
  parse_number_rub(nums[idx[1]])
}

parse_application_outcome <- function(href) {
  url <- paste0(base_url, href)
  html <- fetch_html(url)
  
  fetched_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  
  if (is.null(html)) {
    return(tibble(
      href = href,
      url = url,
      status_text = NA_character_,
      grant_amount_rub = NA_real_,
      fetched_at = fetched_at,
      ok = FALSE
    ))
  }
  
  status <- extract_status_text(html)
  amount <- extract_grant_amount_rub(html)
  
  tibble(
    href = href,
    url = url,
    status_text = status,
    grant_amount_rub = amount,
    fetched_at = fetched_at,
    ok = !is_empty(status)
  )
}

# ----------------------------
# Start from last backup (input + resume)
# ----------------------------
projects_file_use <- pick_latest_sibling(projects_file)
if (!file.exists(projects_file_use)) stop("Missing input file: ", projects_file_use)
log_msg("Using projects file:", projects_file_use)

resume_file <- pick_resume_file(out_file)
log_msg("Resuming from:", resume_file, "| Writing to:", out_file)

projects <- read_csv(projects_file_use, show_col_types = FALSE, progress = FALSE)

hrefs_2025_r1 <- projects %>%
  mutate(contest_l = str_to_lower(contest)) %>%
  filter(str_detect(contest_l, "\\b2025\\b"),
         str_detect(contest_l, "перв")) %>%
  filter(!is.na(href), str_trim(href) != "") %>%
  transmute(href = str_trim(href)) %>%
  pull(href) %>%
  unique()

# READ RESUME FILE WITH EXPLICIT TYPES (prevents fetched_at becoming datetime)
old <- if (file.exists(resume_file)) {
  read_csv(
    resume_file,
    col_types = cols(
      href = col_character(),
      url = col_character(),
      status_text = col_character(),
      grant_amount_rub = col_double(),
      fetched_at = col_character(),   # <- critical
      ok = col_logical()
    ),
    show_col_types = FALSE,
    progress = FALSE
  )
} else {
  tibble(href=character(), url=character(), status_text=character(),
         grant_amount_rub=double(), fetched_at=character(), ok=logical())
}

needed <- c("href","url","status_text","grant_amount_rub","fetched_at","ok")
for (nm in needed) if (!nm %in% names(old)) old[[nm]] <- NA

old <- normalize_results_types(old) %>% distinct(href, .keep_all = TRUE)

# ----------------------------
# Resume order: start AFTER last href in resume file (wrap), then fix empties last
# ----------------------------
last_href <- if (nrow(old) > 0) tail(old$href[!is_empty(old$href)], 1) else NA_character_
start_pos <- match(last_href, hrefs_2025_r1)

ordered_hrefs <- if (!is.na(start_pos)) {
  if (start_pos < length(hrefs_2025_r1)) {
    c(hrefs_2025_r1[(start_pos + 1):length(hrefs_2025_r1)],
      hrefs_2025_r1[1:start_pos])
  } else {
    hrefs_2025_r1
  }
} else {
  hrefs_2025_r1
}

todo_new <- ordered_hrefs[!ordered_hrefs %in% old$href]
todo_fix <- old %>% filter(is_empty(status_text)) %>% pull(href) %>% unique()
todo <- union(todo_new, todo_fix)

log_msg("Resume after:", last_href %||% "<none>",
        "| Found hrefs 2025 r1:", length(hrefs_2025_r1),
        "| Already have:", nrow(old),
        "| New left:", length(todo_new),
        "| Need fix:", length(todo_fix),
        "| Total to do:", length(todo))

if (length(todo) == 0) {
  log_msg("Nothing to do. Exiting.")
  quit(save="no")
}

# ----------------------------
# Run + upsert (NO [<- anywhere)
# ----------------------------
buffer <- list()

flush_buffer <- function() {
  if (length(buffer) == 0) return(invisible(NULL))
  
  batch <- bind_rows(buffer)
  old <<- upsert_by_href(old, batch)
  
  safe_write_csv(old, out_file)
  log_msg("Wrote", nrow(old), "rows total to", out_file)
  
  buffer <<- list()
  invisible(NULL)
}

for (i in seq_along(todo)) {
  href <- todo[[i]]
  log_msg(sprintf("[%d/%d] %s", i, length(todo), href))
  
  Sys.sleep(sleep_seconds)
  buffer[[length(buffer) + 1]] <- parse_application_outcome(href)
  
  if (length(buffer) >= flush_every) flush_buffer()
}

flush_buffer()
log_msg("Done.")