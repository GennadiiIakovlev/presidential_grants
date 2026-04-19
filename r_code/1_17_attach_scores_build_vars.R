###############################################################################
# 2_3  Attach Gemini 2.5 quality scores, build analysis variables, diagnostics
#
# Reads:  data/data_large/presi_variables_imputed.csv  (from 2_2)
# Writes: data/data_large/presi_variables_analysis.csv
###############################################################################

###############################################################################
# 0) Packages
###############################################################################

pacman::p_load(
  MASS, tidyr, purrr, progress, tidyverse, stringi, stringr, readr, httr,
  tibble, lubridate, margins, car, pscl, detectseparation, ggplot2, ggrepel,
  zoo, writexl, stringdist, fuzzyjoin, readxl, openxlsx, caret, stats, scales,
  tm, wordcloud, RColorBrewer, quanteda, quanteda.textstats, readtext, Hmisc,
  Rcpp, naniar, psych, lavaan, xtable, mice, tidymodels, UpSetR, Amelia,
  dotwhisker, forcats, dplyr, broom, grid
)

###############################################################################
# 1) Load data
###############################################################################

presi_variables <- read.csv("data/data_large/presi_variables_imputed.csv", sep = ",")

###############################################################################
# 1.1) Attach Gemini 2.5 quality scores (2026-02-14)
###############################################################################

extract_href_key <- function(x) {
  x <- as.character(x)
  x <- stringr::str_trim(x)
  x <- stringr::str_to_lower(x)
  x <- stringr::str_replace(x, "^https?://[^/]+", "")
  x <- stringr::str_replace(x, "&.*$", "")
  id <- stringr::str_match(x, "item[?]id=([0-9a-f-]{36})")[, 2]
  key <- ifelse(!is.na(id), paste0("/public/application/item?id=", id), x)
  stringr::str_sub(key, 1, 120)
}

gemini_scores <- purrr::map(
  list(
    "data/data_large/quality_scores_2026-02-14.csv",
    "data/data_large/patch1646_scores_2026-02-15.csv"
  ),
  readr::read_csv, show_col_types = FALSE
) %>%
  dplyr::bind_rows() %>%
  mutate(
    href_key       = extract_href_key(href),
    relevance      = suppressWarnings(as.numeric(relevance)),
    coherence      = suppressWarnings(as.numeric(coherence)),
    budget_realism = suppressWarnings(as.numeric(budget_realism)),
    scale          = suppressWarnings(as.numeric(scale)),
    gemini_quality = rowMeans(cbind(relevance, coherence, budget_realism, scale), na.rm = TRUE)
  ) %>%
  filter(!is.na(href_key), href_key != "") %>%
  arrange(href_key, desc(gemini_quality)) %>%
  distinct(href_key, .keep_all = TRUE) %>%
  select(href_key, gemini_quality)

if (!("href" %in% names(presi_variables))) {
  stop("presi_variables is missing href; cannot join Gemini quality")
}

presi_variables <- presi_variables %>%
  mutate(href_key = extract_href_key(href)) %>%
  left_join(gemini_scores, by = "href_key")

cat("Gemini quality merge complete. Rows:", nrow(presi_variables),
    " Missing gemini_quality:", sum(is.na(presi_variables$gemini_quality)), "
")


range(presi_variables$date)

###############################################################################
# 2) Factor / Date Variables, Competition Index, has_trad
###############################################################################

presi_variables <- presi_variables %>%
  filter(!str_detect(competition, stringr::fixed("Специальный", ignore_case = TRUE)))

# Direction: base category and youth shortening
presi_variables <- presi_variables %>%
  mutate(
    direction = as.character(direction),
    direction = case_when(
      direction == "Поддержка молодежных проектов, реализация которых охватывает виды деятельности, предусмотренные статьей 31.1 Федерального закона от 12 января 1996 г. № 7-ФЗ «О некоммерческих организациях»" ~ "Youth projects",
      TRUE ~ direction
    ),
    direction = factor(direction)
  )

presi_variables$direction <- relevel(
  presi_variables$direction,
  ref = "Поддержка проектов в области культуры и искусства"
)

# Date (recompute, since Amelia dropped earlier date column)
presi_variables <- presi_variables %>%
  mutate(
    year_extracted = as.numeric(str_extract(competition, "\\d{4}")),
    date = case_when(
      str_detect(competition, "Второй конкурс") ~ as.Date(
        paste(year_extracted, "07", "01", sep = "-")
      ),
      str_detect(competition, "Первый конкурс") ~ as.Date(
        paste(year_extracted, "01", "01", sep = "-")
      ),
      TRUE ~ as.Date(paste(year_extracted, "12", "01", sep = "-"))
    )
  )

# Ordered competition factor and numeric index
ordered_competitions <- c(
  "Первый конкурс 2017",  "Второй конкурс 2017",
  "Первый конкурс 2018",  "Второй конкурс 2018",
  "Первый конкурс 2019",  "Второй конкурс 2019",
  "Первый конкурс 2020",  "Второй конкурс 2020",
  "Первый конкурс 2021",  "Второй конкурс 2021",
  "Первый конкурс 2022",  "Второй конкурс 2022",
  "Первый конкурс 2023",  "Второй конкурс 2023",
  "Первый конкурс 2024",  "Второй конкурс 2024",
  "Первый конкурс 2025",  "Специальный конкурс 2020",
  "Специальный конкурс 2022"
)

presi_variables <- presi_variables %>%
  mutate(
    competition = factor(competition, levels = ordered_competitions, ordered = TRUE),
    competition_index = as.numeric(competition),
    has_trad = as.integer(trad_vals > 2)
  )

presi_variables <- presi_variables %>%
  mutate(
    has_cofinancing = as.integer(cofinancing_share > 5),
    sum_grants_m    = sum_grants    / 1e6,
    sum_requested_m = sum_requested / 1e6
  )

###############################################################################
# 2b) Construct pre-determined prior-grant variables from INN-level panel
#
# Three variables replace the cross-sectionally scraped badge_president_grants,
# supported_projects, and sum_grants — all of which were contaminated by
# reverse causation (scraped at one point in time, reflecting current-round
# outcomes for first-time winners).
#
#   has_won_before     : 1 if org won in ANY strictly prior round  (binary)
#   cum_prior_projects : count of winning applications in prior rounds (count)
#   cum_prior_grants_m : sum of requested amounts for prior wins, in M RUB
#
# INN is missing for rounds 2017-1, 2017-2, 2018-1 and partially 2019-2
# (early scraping gaps). For these rows all three variables are set to 0,
# which is factually correct: the system launched in 2017.
###############################################################################

org_lookup <- read_csv(
  "data/data_large/all_presi_variables.csv",
  col_select = c("href", "inn", "org_name"),
  show_col_types = FALSE
) %>%
  distinct(href, .keep_all = TRUE)

presi_variables <- presi_variables %>%
  left_join(org_lookup, by = "href")

# Aggregate to org x competition level
org_prior_wins <- presi_variables %>%
  filter(!is.na(inn)) %>%
  group_by(inn, competition_index) %>%
  summarise(
    won_this_comp    = as.integer(any(project_status == 1)),
    n_wins_this_comp = sum(project_status == 1),
    grants_this_comp = sum(sum_requested[project_status == 1], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(inn, competition_index) %>%
  group_by(inn) %>%
  mutate(
    has_won_before     = as.integer(lag(cumsum(won_this_comp),    default = 0) > 0),
    cum_prior_projects = lag(cumsum(n_wins_this_comp),             default = 0L),
    cum_prior_grants_m = lag(cumsum(grants_this_comp),             default = 0)  / 1e6
  ) %>%
  ungroup() %>%
  select(inn, competition_index, has_won_before, cum_prior_projects, cum_prior_grants_m)

presi_variables <- presi_variables %>%
  left_join(org_prior_wins, by = c("inn", "competition_index")) %>%
  mutate(
    has_won_before     = replace_na(has_won_before,     0L),
    cum_prior_projects = replace_na(cum_prior_projects, 0L),
    cum_prior_grants_m = replace_na(cum_prior_grants_m, 0)
  )

# ── Panel-correct "red badge" variables ─────────────────────────────────────
#
# Both badges are scraped cross-sectionally from platforma-nko.rf and carry
# no per-org acquisition timestamp (verified 2026-03-25: single static string
# across all orgs).  We reconstruct when each badge is *informative* using
# what we know from the panel data itself.
#
# had_no_report_before:
#   The badge text reads "no MoJ report filed SINCE 1 JANUARY 2023 to the
#   present."  This is a platform-wide cut-off date, not an org-specific
#   event.  For any application submitted before 2023-01-01, the badge
#   describes a FUTURE non-compliance that could not have been known at
#   application time → set to 0.  For 2023+ applications, the org's
#   non-compliance is demonstrably pre-existing (they haven't filed since
#   Jan 2023), so the badge is valid provided the org had also won at least
#   once before (otherwise the badge reflects other programmes, not PGF).
#
# had_proj_failure_before:
#   A project failure can only be recorded after the implementation period
#   ends.  PGF grants run ~10 months on average; the failure is typically
#   recorded within a further round.  We therefore require that the most
#   recent prior win was at least 2 competition rounds ago — enough time for
#   implementation + recording.  Since PGF data starts in 2017, we can
#   compute this precisely from the panel without any imputation.
#
# Both constructions use only observable panel data and known dates;
# no badge timestamp scraping is required.  The original scraped badges are
# kept in the data for reference but excluded from model formulae.

# For each org × competition, compute the most recent prior win round
# (strictly before the current competition_index).
org_last_win_round <- presi_variables %>%
  filter(!is.na(inn)) %>%
  group_by(inn, competition_index) %>%
  summarise(won_this_comp2 = as.integer(any(project_status == 1)), .groups = "drop") %>%
  arrange(inn, competition_index) %>%
  group_by(inn) %>%
  mutate(
    last_win_index = lag(
      zoo::na.locf(
        if_else(won_this_comp2 == 1, competition_index, NA_real_),
        na.rm = FALSE
      ),
      default = NA_real_
    )
  ) %>%
  ungroup() %>%
  select(inn, competition_index, last_win_index)

org_badge_status <- presi_variables %>%
  filter(!is.na(inn)) %>%
  group_by(inn) %>%
  summarise(
    org_has_no_report = max(badge_no_reporting,    na.rm = TRUE),
    org_has_proj_fail = max(badge_project_failure, na.rm = TRUE),
    .groups = "drop"
  )

presi_variables <- presi_variables %>%
  left_join(org_last_win_round, by = c("inn", "competition_index")) %>%
  left_join(org_badge_status,   by = "inn") %>%
  mutate(
    org_has_no_report     = replace_na(org_has_no_report, 0L),
    org_has_proj_fail     = replace_na(org_has_proj_fail, 0L),
    rounds_since_last_win = competition_index - replace_na(last_win_index, Inf),

    # no-reporting: badge text anchored at 2023-01-01; pre-2023 rows → 0
    had_no_report_before = as.integer(
      org_has_no_report  == 1 &
      has_won_before     == 1 &
      date               >= as.Date("2023-01-01")
    ),

    # project failure: require ≥2 rounds since last win (implementation lag)
    had_proj_failure_before = as.integer(
      org_has_proj_fail      == 1 &
      rounds_since_last_win  >= 2
    )
  ) %>%
  select(-org_has_no_report, -org_has_proj_fail, -last_win_index,
         -rounds_since_last_win)

cat("had_no_report_before distribution:\n")
print(table(presi_variables$had_no_report_before))
cat(sprintf("  Share: %.1f%%\n",
            100 * mean(presi_variables$had_no_report_before, na.rm = TRUE)))
cat("had_proj_failure_before distribution:\n")
print(table(presi_variables$had_proj_failure_before))
cat(sprintf("  Share: %.1f%%\n",
            100 * mean(presi_variables$had_proj_failure_before, na.rm = TRUE)))

# ── Religious-organisation indicator ──────────────────────────────────────────
# Matches on project title OR legal organisation name.
# Roots cover Orthodox Christianity (the dominant case), generic "religious",
# other confessions (Islam, Buddhism, Judaism) and canonical Orthodox terms
# (diocese, metropolitan, monastery, church/temple, theology).
religious_pat <- paste0(
  "религ",           # религиозный / религиозная / религиоведение
  "|православ",      # православный / православная
  "|свят",           # святой / святая / священный / Святейший
  "|монастыр",       # монастырь
  "|церков",         # церковь / церковный
  "|храм",           # храм
  "|епарх",          # епархия / епархиальный
  "|митропол",       # митрополия / митрополит
  "|богослов",       # богословие / богословский (seminary/theology)
  "|ислам|мечет|мусульман",   # Islamic
  "|буддист|дацан",           # Buddhist
  "|иудей|синагог"            # Jewish
)

presi_variables <- presi_variables %>%
  mutate(
    org_name_safe = replace_na(org_name, ""),
    title_safe    = replace_na(title,    ""),
    is_religious  = as.integer(
      str_detect(str_to_lower(title_safe),    religious_pat) |
      str_detect(str_to_lower(org_name_safe), religious_pat)
    )
  ) %>%
  select(-org_name_safe, -title_safe)

cat("is_religious distribution:\n")
print(table(presi_variables$is_religious))
cat(sprintf("  Share religious: %.1f%%\n",
            100 * mean(presi_variables$is_religious, na.rm = TRUE)))

cat("has_won_before distribution:\n")
print(table(presi_variables$has_won_before))
cat("cum_prior_projects summary:\n")
print(summary(presi_variables$cum_prior_projects))
cat("cum_prior_grants_m summary:\n")
print(summary(presi_variables$cum_prior_grants_m))

# Sanity checks on trad_vals
Hmisc::describe(presi_variables$has_trad)
with(presi_variables,
     table(has_trad, trad_vals, useNA = "ifany"))

###############################################################################
# 3) Basic Diagnostics: Structure, Missingness, Descriptive Stats
###############################################################################

cat("Number of rows:", nrow(presi_variables), "\n")
cat("Number of columns:", ncol(presi_variables), "\n")

glimpse(presi_variables)

# Missingness overview
naniar::pct_miss_var(presi_variables)

vis_miss(presi_variables, warn_large_data = FALSE) +
  ggtitle("Missingness Pattern in presi_variables")

# Descriptive stats per variable
numeric_cols_to_plot <- c(
  "sum_requested",
  "sum_grants",
  "cofinancing",
  "total_project_cost",
  "application",
  "org_age",
  "target_age"
)

for (colname in numeric_cols_to_plot) {
  if (colname %in% names(presi_variables)) {
    p <- ggplot(presi_variables, aes(x = .data[[colname]])) +
      geom_histogram(bins = 30) +
      labs(
        title = paste("Distribution of", colname),
        x = colname,
        y = "Count"
      )
    print(p)
  }
}

# Summary table for LaTeX
# NOTE: removed stray `presi_variables <- presi_model` that was here;
# presi_model is not yet built at this point and the line was overwriting
# presi_variables (including has_won_before) with a stale workspace object.
numeric_idx <- vapply(presi_variables, is.numeric, logical(1))
descriptive_stats <- data.frame(
  Variable = names(presi_variables),
  Mean     = sapply(presi_variables, function(x) if (is.numeric(x)) mean(x, na.rm = TRUE) else NA_real_),
  Median   = sapply(presi_variables, function(x) if (is.numeric(x)) median(x, na.rm = TRUE) else NA_real_),
  SD       = sapply(presi_variables, function(x) if (is.numeric(x)) sd(x, na.rm = TRUE) else NA_real_),
  Min      = sapply(presi_variables, function(x) if (is.numeric(x)) min(x, na.rm = TRUE) else NA_real_),
  Max      = sapply(presi_variables, function(x) if (is.numeric(x)) max(x, na.rm = TRUE) else NA_real_)
)

latex_table <- xtable(
  descriptive_stats,
  caption = "Descriptive Statistics for presi_variables Dataset",
  label   = "tab:desc_stats"
)

print(latex_table, include.rownames = FALSE, caption.placement = "top")

###############################################################################
# 4) Save Final Analysis Dataset
###############################################################################
write_csv(presi_variables, "data/data_large/presi_variables_analysis.csv")
cat("Saved analysis dataset:", nrow(presi_variables), "rows\n")
