###############################################################################
# Refresh budget-period parsed fields for the main dataset.
#
# After restoring the 1,646 "Получено ..." winners back into
# `data/data_large/presi_variables_imputed.csv`, we re-ran the turbo
# budget-period parser which appended their true budget fields to:
#   data/data_large/reparsed_budget_period.csv
#
# This script joins those parsed fields back into the main dataset and
# overwrites the money/date fields from the parser wherever available.
###############################################################################

pacman::p_load(dplyr, readr, stringr, lubridate)

main_path <- "data/data_large/presi_variables_imputed.csv"
bp_path <- "data/data_large/reparsed_budget_period.csv"

stopifnot(file.exists(main_path))
stopifnot(file.exists(bp_path))

normalize_href <- function(x) {
  x <- as.character(x)
  stringr::str_trim(x)
}

message("Reading main dataset: ", main_path)
df <- readr::read_csv(main_path, show_col_types = FALSE) %>%
  mutate(href = normalize_href(href)) %>%
  filter(!is.na(href), href != "", href != "NA") %>%
  mutate(
    implementation_start = as.Date(implementation_start),
    implementation_end = as.Date(implementation_end),
    status_code = suppressWarnings(as.integer(status_code)),
    parsed_at = suppressWarnings(as.POSIXct(parsed_at))
  )

message("Reading reparsed budget period: ", bp_path)
bp_raw <- readr::read_csv(bp_path, show_col_types = FALSE) %>%
  mutate(href = normalize_href(href)) %>%
  filter(!is.na(href), href != "", href != "NA")

# Prefer successful parses with any money present; keep the "best" row per href
bp <- bp_raw %>%
  mutate(
    ok200 = status_code == 200,
    any_money = (!is.na(sum_requested) | !is.na(cofinancing) | !is.na(total_expenses)),
    na_count = rowSums(is.na(across(c(sum_requested, cofinancing, total_expenses))))
  ) %>%
  arrange(href, desc(ok200), desc(any_money), na_count) %>%
  group_by(href) %>%
  slice(1) %>%
  ungroup() %>%
  transmute(
    href,
    bp_ok200 = ok200,
    bp_any_money = any_money,
    bp_sum_requested = suppressWarnings(as.numeric(sum_requested)),
    bp_cofinancing = suppressWarnings(as.numeric(cofinancing)),
    bp_total_expenses = suppressWarnings(as.numeric(total_expenses)),
    bp_implementation_period = as.character(implementation_period),
    bp_implementation_start = as.Date(implementation_start),
    bp_implementation_end = as.Date(implementation_end),
    bp_status_code = suppressWarnings(as.integer(status_code)),
    bp_fetch_error = as.character(fetch_error),
    bp_parsed_at = suppressWarnings(as.POSIXct(parsed_at))
  )

message("Joining parsed fields...")
out <- df %>%
  left_join(bp, by = "href") %>%
  mutate(
    sum_requested = coalesce(bp_sum_requested, sum_requested),
    cofinancing = coalesce(bp_cofinancing, cofinancing),
    total_expenses = coalesce(bp_total_expenses, total_expenses),
    proj_budget = total_expenses,
    cofinancing_share = if_else(
      !is.na(total_expenses) & total_expenses > 0 & !is.na(cofinancing),
      100 * cofinancing / total_expenses,
      as.numeric(NA)
    ),
    implementation_period = coalesce(bp_implementation_period, implementation_period),
    implementation_start = coalesce(bp_implementation_start, implementation_start),
    implementation_end = coalesce(bp_implementation_end, implementation_end),
    status_code = coalesce(bp_status_code, status_code),
    fetch_error = coalesce(bp_fetch_error, fetch_error),
    parsed_at = coalesce(bp_parsed_at, parsed_at),
    implementation_length_months = case_when(
      !is.na(implementation_start) & !is.na(implementation_end) & implementation_end >= implementation_start ~
        time_length(interval(implementation_start, implementation_end), "months"),
      TRUE ~ as.numeric(NA)
    )
  ) %>%
  # If the parser hit the page (200) but found no budget fields, do NOT keep
  # the card-based "Получено" fallback as total_expenses; blank out budget vars.
  mutate(
    total_expenses = if_else(bp_ok200 & !bp_any_money, as.numeric(NA), total_expenses),
    proj_budget = if_else(bp_ok200 & !bp_any_money, as.numeric(NA), proj_budget),
    cofinancing = if_else(bp_ok200 & !bp_any_money, as.numeric(NA), cofinancing),
    cofinancing_share = if_else(bp_ok200 & !bp_any_money, as.numeric(NA), cofinancing_share)
  ) %>%
  select(-starts_with("bp_"))

# Sanity
neg_share <- sum(out$cofinancing_share < 0, na.rm = TRUE)
neg_cof <- sum(out$cofinancing < 0, na.rm = TRUE)
message("Negative cofinancing_share count: ", neg_share)
message("Negative cofinancing count: ", neg_cof)

backup_path <- "data/data_large/presi_variables_imputed_pre_bp_refresh_20260215.csv"
message("Writing safety backup: ", backup_path)
readr::write_csv(df, backup_path)

message("Overwriting main dataset: ", main_path)
readr::write_csv(out, main_path)

message("Done.")
