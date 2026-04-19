###############################################################################
# Restore 1,646 winners dropped due to "Получено ..." money parsing
# and drop 8 outlier losers.
#
# Context
# - Current main dataset excludes special contests and is missing 1,654
#   non-special applications vs the 19122025 full backup.
# - 1,646 of those are WINNERS where cards `fond_price` starts with
#   "Получено ..." (text prefix), which turned into NA under strict parsing.
# - The remaining 8 are LOSERS with extreme / broken card amounts.
#
# This script:
# - Restores the 1,646 winners by robustly parsing the numeric amount inside
#   the "Получено ..." `fond_price` string and recomputing money fields.
# - Ensures the 8 loser outliers remain excluded.
# - Overwrites `data/data_large/presi_variables_imputed.csv` (with a safety copy).
###############################################################################

pacman::p_load(dplyr, readr, stringr, stringi, lubridate)

paths <- list(
  current = "data/data_large/presi_variables_imputed.csv",
  backup_full = "data/data_large/presi_variables_imputed_19122025_backup.csv",
  cards = "data/data_large/all_projects_data_backup.csv",
  qual120b = "data/data_large/appl_qual_120b.csv"
)

stopifnot(file.exists(paths$current))
stopifnot(file.exists(paths$backup_full))
stopifnot(file.exists(paths$cards))

normalize_href <- function(x) {
  x <- as.character(x)
  stringr::str_trim(x)
}

parse_money_anywhere <- function(x) {
  # Parses a money amount from strings that may contain Cyrillic,
  # currency symbols, NBSP, etc (e.g., "Получено грантов 1 234 567,00 ₽").
  x <- as.character(x)
  x <- stringi::stri_trans_nfkc(x)
  x <- stringr::str_replace_all(x, "&nbsp;", " ")
  x <- stringr::str_replace_all(x, "\u00A0", " ")
  x <- stringr::str_replace_all(x, "\u202F", " ")
  # Keep only digits, separators and whitespace
  x <- stringr::str_replace_all(x, "[^0-9,.[[:space:]]]+", " ")
  x <- stringr::str_squish(x)
  readr::parse_number(
    x,
    locale = readr::locale(decimal_mark = ",", grouping_mark = " ")
  )
}

message("Reading current dataset: ", paths$current)
cur <- readr::read_csv(paths$current, show_col_types = FALSE) %>%
  mutate(href = normalize_href(href)) %>%
  filter(!is.na(href), href != "", href != "NA")

want_cols <- names(cur)

message("Reading full backup: ", paths$backup_full)
bak <- readr::read_csv(paths$backup_full, show_col_types = FALSE) %>%
  mutate(href = normalize_href(href)) %>%
  filter(!is.na(href), href != "", href != "NA")

message("Reading cards (distinct href): ", paths$cards)
cards <- readr::read_csv(paths$cards, show_col_types = FALSE) %>%
  mutate(href = normalize_href(href)) %>%
  filter(!is.na(href), href != "", href != "NA") %>%
  distinct(href, .keep_all = TRUE) %>%
  transmute(
    href,
    contest_cards = as.character(contest),
    project_price_txt = as.character(project_price),
    fond_price_txt = as.character(fond_price),
    project_price_num = parse_money_anywhere(project_price_txt),
    fond_price_num = parse_money_anywhere(fond_price_txt),
    fond_starts_polucheno = str_detect(str_squish(fond_price_txt), "^Получено")
  )

cur_hrefs <- cur %>% distinct(href)

missing <- bak %>% anti_join(cur_hrefs, by = "href")

missing_non_special <- missing %>%
  filter(!str_detect(competition, regex("Специальный", ignore_case = TRUE)))

missing_non_special <- missing_non_special %>%
  left_join(cards, by = "href")

message("Missing non-special: ", nrow(missing_non_special))

# Identify the 1,646 winners to restore
to_restore <- missing_non_special %>%
  filter(project_status == 1, fond_starts_polucheno)

# Identify the 8 outlier losers to keep excluded
to_drop_losers <- missing_non_special %>%
  filter(project_status == 0, !fond_starts_polucheno) %>%
  distinct(href)

message("To restore (winners, polucheno): ", nrow(to_restore))
message("To drop (losers, outliers): ", nrow(to_drop_losers))

stopifnot(nrow(to_restore) == 1646)
stopifnot(nrow(to_drop_losers) == 8)

# Build restored rows aligned to current columns
restored <- to_restore %>%
  mutate(
    sum_requested = project_price_num,
    total_expenses = fond_price_num,
    proj_budget = total_expenses,
    cofinancing = if_else(
      !is.na(total_expenses) & !is.na(sum_requested),
      total_expenses - sum_requested,
      as.numeric(NA)
    ),
    cofinancing_share = if_else(
      !is.na(total_expenses) & total_expenses > 0 & !is.na(cofinancing),
      100 * cofinancing / total_expenses,
      as.numeric(NA)
    ),
    sum_requested_100k = sum_requested / 100000,
    sum_grants_100k = sum_grants / 100000,
    # Newer fields in current dataset (not present in old backup)
    implementation_length_months = as.numeric(NA),
    implementation_period = as.character(NA),
    implementation_start = as.Date(NA),
    implementation_end = as.Date(NA),
    status_code = as.integer(NA),
    fetch_error = as.character(NA),
    parsed_at = as.POSIXct(NA)
  )

# If the backup has appl_quality (older model), map it into appl_quality_qwen4_trained
if ("appl_quality" %in% names(restored) && ("appl_quality_qwen4_trained" %in% want_cols)) {
  restored <- restored %>%
    mutate(appl_quality_qwen4_trained = as.numeric(appl_quality))
}

# Attach modern appl_quality (120b) if available
if (file.exists(paths$qual120b)) {
  message("Joining appl_quality (120b): ", paths$qual120b)
  q <- readr::read_csv(paths$qual120b, show_col_types = FALSE) %>%
    mutate(href = normalize_href(href)) %>%
    select(href, appl_quality) %>%
    mutate(appl_quality = suppressWarnings(as.numeric(appl_quality))) %>%
    filter(!is.na(href), href != "") %>%
    group_by(href) %>%
    summarise(appl_quality = first(na.omit(appl_quality)), .groups = "drop")

  restored <- restored %>%
    mutate(href = normalize_href(href)) %>%
    left_join(q, by = "href")
}

# Drop helper columns from cards join
restored <- restored %>%
  select(-any_of(c(
    "contest_cards",
    "project_price_txt",
    "fond_price_txt",
    "project_price_num",
    "fond_price_num",
    "fond_starts_polucheno"
  )))

# Align to current schema: add missing columns as NA, then select in correct order
missing_cols <- setdiff(want_cols, names(restored))
if (length(missing_cols) > 0) {
  restored[missing_cols] <- NA
}
restored <- restored %>% select(all_of(want_cols))

updated <- bind_rows(cur, restored) %>%
  mutate(href = normalize_href(href)) %>%
  filter(!is.na(href), href != "", href != "NA") %>%
  distinct(href, .keep_all = TRUE) %>%
  anti_join(to_drop_losers, by = "href")

message("Rows before: ", nrow(cur))
message("Rows after:  ", nrow(updated))

# Sanity checks
stopifnot(nrow(updated) == nrow(cur) + 1646)
stopifnot(sum(str_detect(updated$competition, regex("Специальный", ignore_case = TRUE))) == 0)

money_ok <- updated %>%
  filter(!is.na(sum_requested), !is.na(total_expenses), !is.na(cofinancing)) %>%
  summarise(pct_consistent = mean(abs((total_expenses - sum_requested) - cofinancing) <= 1)) %>%
  pull(pct_consistent)
message("Money consistency (total - requested == cofinancing): ", round(100 * money_ok, 4), "%")
stopifnot(isTRUE(all.equal(money_ok, 1)))

# Safety copy, then overwrite the main dataset
backup_path <- "data/data_large/presi_variables_imputed_pre_fix_20260215.csv"
message("Writing safety backup: ", backup_path)
readr::write_csv(cur, backup_path)

message("Overwriting main dataset: ", paths$current)
readr::write_csv(updated, paths$current)

message("Done.")
