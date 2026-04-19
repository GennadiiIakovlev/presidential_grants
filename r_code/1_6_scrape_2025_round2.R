suppressPackageStartupMessages({
  library(rvest)
  library(dplyr)
  library(stringr)
  library(purrr)
  library(readr)
  library(tibble)
})

# ----------------------------
# Config
# ----------------------------
base_url <- "https://xn--80afcdbalict6afooklqi5o.xn--p1ai"

# ONLY this range
page_start <- 457
page_end   <- 987

# separate outputs (won't touch your master)
projects_file_raw <- "data/data_large/cards_pages_0457_0987_raw.csv"
projects_file_2026 <- "data/data_large/cards_pages_0457_0987_second_contest_2025.csv"
log_file <- "data/data_large/parse_pages_0457_0987.log"

sleep_seconds <- 0.05
max_retries   <- 4
base_backoff  <- 0.5

target_regex  <- "второй\\s+конкурс\\s+2025"

dir.create(dirname(projects_file_raw),  recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(projects_file_2026), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(log_file),           recursive = TRUE, showWarnings = FALSE)

# ----------------------------
# Helpers
# ----------------------------
log_msg <- function(...) {
  msg <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", paste(..., collapse=" "))
  cat(msg, "\n")
  cat(msg, "\n", file = log_file, append = TRUE)
}

make_unique_path <- function(path) {
  if (!file.exists(path)) return(path)
  ext  <- tools::file_ext(path)
  base <- tools::file_path_sans_ext(path)
  i <- 1
  repeat {
    candidate <- sprintf("%s_%03d.%s", base, i, ext)
    if (!file.exists(candidate)) return(candidate)
    i <- i + 1
  }
}

backup_if_exists <- function(path, tag = "backup") {
  if (!file.exists(path)) return(invisible(NULL))
  
  ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
  dir <- dirname(path)
  base <- tools::file_path_sans_ext(basename(path))
  ext <- tools::file_ext(path)
  
  backup_name <- file.path(dir, sprintf("%s_%s_%s.%s", base, tag, ts, ext))
  backup_name <- make_unique_path(backup_name)
  
  ok <- file.copy(path, backup_name, overwrite = FALSE)
  if (isTRUE(ok)) {
    log_msg("Backed up existing file:", path, "->", backup_name)
    return(invisible(backup_name))
  } else {
    log_msg("WARNING: Backup failed for:", path)
    return(invisible(NULL))
  }
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

# safer extraction (won't blow up if selector is missing)
safe_text <- function(node, css) {
  el <- tryCatch(html_element(node, css), error = function(e) NULL)
  if (is.null(el) || length(el) == 0) return(NA_character_)
  txt <- tryCatch(html_text(el, trim = TRUE), error = function(e) NA_character_)
  if (length(txt) == 0) NA_character_ else txt
}

parse_cards_page <- function(page_number) {
  url <- paste0(base_url, "/public/application/cards?page=", page_number)
  
  html <- fetch_html(url)
  if (is.null(html)) return(tibble())
  
  cards <- html %>% html_elements("a[href*='public/application/item']")
  if (length(cards) == 0) return(tibble())
  
  cards %>%
    map_dfr(~{
      href <- tryCatch(html_attr(.x, "href"), error = function(e) NA_character_)
      tibble(
        href = href,
        full_url = ifelse(!is.na(href) & !str_detect(href, "^https?://"),
                          paste0(base_url, href), href),
        title = safe_text(.x, ".projects__title"),
        contest = safe_text(.x, ".contest"),
        direction = safe_text(.x, ".direction"),
        project_price = safe_text(.x, ".projects__price"),
        fond_price = safe_text(.x, ".projects__price--fond"),
        city = safe_text(.x, ".projects__descr div:first-child"),
        project_status = safe_text(.x, ".projects__type span"),
        application = safe_text(.x, ".projects__str-no-wrap"),
        page_number = page_number
      )
    }) %>%
    mutate(across(where(is.character), ~na_if(str_squish(.x), "")))
}

# UPSERT by href
upsert_by_href <- function(old_df, new_df) {
  old_df %>%
    anti_join(new_df %>% distinct(href), by = "href") %>%
    bind_rows(new_df)
}

# ----------------------------
# Main
# ----------------------------
old_raw <- if (file.exists(projects_file_raw)) {
  read_csv(projects_file_raw, show_col_types = FALSE, progress = FALSE)
} else {
  tibble(
    href=character(), full_url=character(), title=character(), contest=character(),
    direction=character(), project_price=character(), fond_price=character(),
    city=character(), project_status=character(), application=character(),
    page_number=integer()
  )
}

old_raw <- old_raw %>%
  filter(!is.na(href), str_trim(href) != "") %>%
  distinct(href, .keep_all = TRUE)

log_msg("Starting range scan:", page_start, "to", page_end)
log_msg("Existing RAW rows:", nrow(old_raw))

all_rows <- list()

for (p in page_start:page_end) {
  Sys.sleep(sleep_seconds)
  log_msg("Page", p)
  
  df <- parse_cards_page(p)
  
  # KEY PATCH: do NOT break on 0 rows
  if (nrow(df) == 0) {
    log_msg("  WARNING: 0 rows (fetch/parse miss) -> continuing")
    next
  }
  
  all_rows[[length(all_rows) + 1]] <- df
  log_msg("  Rows:", nrow(df))
}

new_raw <- bind_rows(all_rows) %>%
  filter(!is.na(href), str_trim(href) != "") %>%
  distinct(href, .keep_all = TRUE)

log_msg("New RAW rows parsed:", nrow(new_raw))

# KEY PATCH: never overwrite with empty result
if (nrow(new_raw) == 0) {
  log_msg("ABORT: parsed 0 rows total; will NOT overwrite anything.")
} else {
  updated_raw <- upsert_by_href(old_raw, new_raw) %>%
    distinct(href, .keep_all = TRUE)
  
  backup_if_exists(projects_file_raw, tag = "backup")
  write_csv(updated_raw, projects_file_raw)
  log_msg("Wrote RAW:", nrow(updated_raw), "rows to", projects_file_raw)
  
  # optional filtered file for 2026 second contest
  hits_2026 <- updated_raw %>%
    mutate(contest_l = str_to_lower(str_squish(contest))) %>%
    filter(!is.na(contest_l), str_detect(contest_l, target_regex)) %>%
    select(-contest_l) %>%
    distinct(href, .keep_all = TRUE)
  
  write_csv(hits_2026, projects_file_2026)
  log_msg("Wrote 2026-2 filtered:", nrow(hits_2026), "rows to", projects_file_2026)
}

