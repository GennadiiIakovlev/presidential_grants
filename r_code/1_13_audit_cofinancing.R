###############################################################################
# Audit: missing applications + co-financing fix (2017-2025H1)
#
# What it does:
# 1) Finds applications present in the 19122025 full backup but missing
#    from the current presi_variables_imputed.csv
# 2) Writes those missing applications as a metadata CSV + href list
# 3) Builds a FULL dataset (159,341 hrefs) with corrected money fields:
#    - sum_requested from cards: project_price
#    - total_expenses from cards: fond_price
#    - cofinancing = total_expenses - sum_requested
#    - cofinancing_share = cofinancing / total_expenses * 100
#
# Outputs:
# - data/data_results/lost_applications_metadata_20260215.csv
# - data/data_results/lost_applications_hrefs_20260215.txt
# - data/data_large/presi_variables_imputed_full_20260215.csv
###############################################################################

pacman::p_load(dplyr, readr, stringr, lubridate)

paths <- list(
  current = "data/data_large/presi_variables_imputed.csv",
  backup_full = "data/data_large/presi_variables_imputed_19122025_backup.csv",
  cards = "data/data_large/all_projects_data_backup.csv",
  budget_period = "data/data_large/reparsed_budget_period.csv",
  qual120b = "data/data_large/appl_qual_120b.csv"
)

stopifnot(file.exists(paths$current))
stopifnot(file.exists(paths$backup_full))
stopifnot(file.exists(paths$cards))

normalize_href <- function(x) {
  x <- as.character(x)
  x <- stringr::str_trim(x)
  x
}

parse_money_cards <- function(x) {
  x <- as.character(x)
  x <- str_remove_all(x, "[[:space:]₽]")
  x <- str_replace_all(x, ",", ".")
  x <- sub("[.].*$", "", x)
  suppressWarnings(as.numeric(x))
}

message("Reading current hrefs: ", paths$current)
cur_hrefs <- readr::read_csv(
  paths$current,
  col_types = cols_only(href = col_character()),
  show_col_types = FALSE
) %>%
  mutate(href = normalize_href(href)) %>%
  filter(!is.na(href), href != "", href != "NA") %>%
  distinct(href)

message("Reading full backup: ", paths$backup_full)
bak <- readr::read_csv(paths$backup_full, show_col_types = FALSE) %>%
  mutate(href = normalize_href(href)) %>%
  filter(!is.na(href), href != "", href != "NA")

message("Reading cards list (distinct href): ", paths$cards)
cards <- readr::read_csv(paths$cards, show_col_types = FALSE) %>%
  mutate(href = normalize_href(href)) %>%
  filter(!is.na(href), href != "", href != "NA") %>%
  distinct(href, .keep_all = TRUE) %>%
  transmute(
    href,
    cards_sum_requested = parse_money_cards(project_price),
    cards_total_expenses = parse_money_cards(fond_price),
    cards_contest = as.character(contest)
  )

missing <- bak %>%
  anti_join(cur_hrefs, by = "href")

message(
  "Missing applications: ", nrow(missing),
  " (special contests: ",
  sum(str_detect(missing$competition, regex("Специальный", ignore_case = TRUE))),
  ")"
)

missing_meta <- missing %>%
  left_join(cards, by = "href") %>%
  mutate(
    total_expenses = cards_total_expenses,
    sum_requested = cards_sum_requested,
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
    lost_reason = case_when(
      str_detect(competition, regex("Специальный", ignore_case = TRUE)) ~ "special_competition_excluded",
      TRUE ~ "dropped_other"
    )
  ) %>%
  select(
    href,
    title,
    competition,
    direction,
    region,
    project_status,
    year_extracted,
    date,
    sum_requested,
    total_expenses,
    cofinancing,
    cofinancing_share,
    lost_reason
  )

out_meta <- "data/data_results/lost_applications_metadata_20260215.csv"
out_hrefs <- "data/data_results/lost_applications_hrefs_20260215.txt"
message("Writing missing metadata: ", out_meta)
readr::write_csv(missing_meta, out_meta)

message("Writing missing href list: ", out_hrefs)
readr::write_lines(missing_meta$href, out_hrefs)

###############################################################################
# Build FULL corrected dataset
###############################################################################

full <- bak %>%
  left_join(cards, by = "href")

# Replace money fields using cards list wherever possible.
full <- full %>%
  mutate(
    sum_requested = coalesce(cards_sum_requested, sum_requested),
    total_expenses = cards_total_expenses,
    cofinancing = if_else(
      !is.na(total_expenses) & !is.na(sum_requested),
      total_expenses - sum_requested,
      as.numeric(NA)
    ),
    proj_budget = total_expenses,
    cofinancing_share = if_else(
      !is.na(total_expenses) & total_expenses > 0 & !is.na(cofinancing),
      100 * cofinancing / total_expenses,
      as.numeric(NA)
    ),
    sum_requested_100k = sum_requested / 100000,
    sum_grants_100k = sum_grants / 100000
  ) %>%
  select(-any_of(c("cards_sum_requested", "cards_total_expenses", "cards_contest")))

# Attach budget period fields if available (only covers a subset).
if (file.exists(paths$budget_period)) {
  message("Joining budget period fields: ", paths$budget_period)
  bp <- readr::read_csv(paths$budget_period, show_col_types = FALSE) %>%
    mutate(href = normalize_href(href)) %>%
    select(
      href,
      implementation_period,
      implementation_start,
      implementation_end,
      status_code,
      fetch_error,
      parsed_at
    ) %>%
    distinct(href, .keep_all = TRUE) %>%
    mutate(
      implementation_start = as.Date(implementation_start),
      implementation_end = as.Date(implementation_end)
    )

  full <- full %>%
    left_join(bp, by = "href") %>%
    mutate(
      implementation_length_months = case_when(
        !is.na(implementation_start) & !is.na(implementation_end) & implementation_end >= implementation_start ~
          time_length(interval(implementation_start, implementation_end), "months"),
        TRUE ~ as.numeric(NA)
      )
    )
}

# Keep old appl_quality as *_qwen4_trained, then join new appl_quality (120b).
full <- full %>%
  rename(appl_quality_qwen4_trained = any_of("appl_quality"))

if (file.exists(paths$qual120b)) {
  message("Joining appl_quality (120b): ", paths$qual120b)
  q <- readr::read_csv(paths$qual120b, show_col_types = FALSE) %>%
    mutate(href = normalize_href(href)) %>%
    select(href, appl_quality) %>%
    mutate(appl_quality = suppressWarnings(as.numeric(appl_quality))) %>%
    filter(!is.na(href), href != "") %>%
    group_by(href) %>%
    summarise(appl_quality = first(na.omit(appl_quality)), .groups = "drop")

  full <- full %>%
    left_join(q, by = "href")
}

out_full <- "data/data_large/presi_variables_imputed_full_20260215.csv"
message("Writing FULL corrected dataset: ", out_full)
readr::write_csv(full, out_full)

message("Done.")
