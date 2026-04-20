pacman::p_load(dplyr, readr, stringr, ggplot2, broom, pROC, tidyr)

validation_run_tag <- "20260214"
validation_graph_dir <- file.path("validation_graphs", paste0("gemini25_", validation_run_tag))
validation_results_dir <- "data/data_results"
government_quality_path <- "data/data_large/application_rating_merged.csv"

dir.create(validation_graph_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(validation_results_dir, recursive = TRUE, showWarnings = FALSE)

resolve_existing_path <- function(candidates) {
  for (path in candidates) {
    if (file.exists(path)) return(path)
  }
  stop("None of the candidate paths exist: ", paste(candidates, collapse = ", "))
}

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

to_num <- function(x) suppressWarnings(as.numeric(x))

fmt <- function(x, d = 4) {
  vapply(x, function(v) {
    if (is.null(v) || length(v) == 0 || is.na(v)) return("NA")
    formatC(v, digits = d, format = "f")
  }, character(1))
}

safe_cor <- function(x, y, method = "pearson") {
  out <- suppressWarnings(cor(x, y, use = "complete.obs", method = method))
  if (!is.finite(out)) return(NA_real_)
  out
}

safe_auc <- function(y_binary, score) {
  keep <- !is.na(y_binary) & !is.na(score)
  y <- y_binary[keep]
  s <- score[keep]
  if (length(y) < 10 || length(unique(y)) < 2) return(NA_real_)
  suppressWarnings(as.numeric(pROC::auc(y, s, quiet = TRUE)))
}

conf_metrics <- function(truth, pred) {
  keep <- !is.na(truth) & !is.na(pred)
  y <- as.integer(truth[keep])
  p <- as.integer(pred[keep])

  if (length(y) == 0) {
    return(c(n = 0, accuracy = NA_real_, precision = NA_real_, recall = NA_real_, specificity = NA_real_, f1 = NA_real_))
  }

  tp <- sum(y == 1 & p == 1)
  tn <- sum(y == 0 & p == 0)
  fp <- sum(y == 0 & p == 1)
  fn <- sum(y == 1 & p == 0)

  precision <- if ((tp + fp) > 0) tp / (tp + fp) else NA_real_
  recall <- if ((tp + fn) > 0) tp / (tp + fn) else NA_real_
  specificity <- if ((tn + fp) > 0) tn / (tn + fp) else NA_real_
  f1 <- if (!is.na(precision) && !is.na(recall) && (precision + recall) > 0) {
    2 * precision * recall / (precision + recall)
  } else {
    NA_real_
  }

  c(
    n = length(y),
    accuracy = (tp + tn) / length(y),
    precision = precision,
    recall = recall,
    specificity = specificity,
    f1 = f1
  )
}


write_report <- function(lines, path) {
  readr::write_lines(lines, path)
  message("Wrote report: ", path)
}

save_plot <- function(plot_obj, path, width = 8, height = 6, dpi = 220) {
  ggplot2::ggsave(path, plot_obj, width = width, height = height, dpi = dpi)
  message("Wrote plot: ", path)
}

load_gemini25_scores <- function() {
  gemini_path <- resolve_existing_path(c("data/data_large/quality_scores_2026-02-14.csv"))
  raw <- readr::read_csv(gemini_path, show_col_types = FALSE)

  stopifnot(all(c("href", "relevance", "coherence", "budget_realism", "scale") %in% names(raw)))

  raw %>%
    mutate(
      href_key = extract_href_key(href),
      relevance = to_num(relevance),
      coherence = to_num(coherence),
      budget_realism = to_num(budget_realism),
      scale = to_num(scale),
      gemini_quality = rowMeans(cbind(relevance, coherence, budget_realism, scale), na.rm = TRUE),
      gemini_dims_n = rowSums(!is.na(cbind(relevance, coherence, budget_realism, scale)))
    ) %>%
    filter(!is.na(href_key), href_key != "") %>%
    arrange(href_key, desc(gemini_dims_n), desc(gemini_quality)) %>%
    group_by(href_key) %>%
    slice(1) %>%
    ungroup() %>%
    select(href_key, href, relevance, coherence, budget_realism, scale, gemini_quality)
}

load_government_scores <- function() {
  gov_path <- government_quality_path
  if (!file.exists(gov_path)) {
    stop("Government quality file not found: ", gov_path)
  }

  raw <- readr::read_csv(gov_path, show_col_types = FALSE)
  message("Using government quality source: ", gov_path, " | raw rows=", nrow(raw))

  if (nrow(raw) < 20000 || nrow(raw) > 40000) {
    warning(
      "Government quality file row count is outside expected ~27k range: ",
      nrow(raw)
    )
  }

  stopifnot(all(c("href", "application_rating") %in% names(raw)))

  raw %>%
    mutate(
      href_key = extract_href_key(href),
      application_rating = to_num(application_rating),
      status_code = if ("status_code" %in% names(raw)) to_num(status_code) else NA_real_,
      fetch_error = if ("fetch_error" %in% names(raw)) as.character(fetch_error) else NA_character_,
      sum_requested = if ("sum_requested" %in% names(raw)) to_num(sum_requested) else NA_real_,
      cofinancing = if ("cofinancing" %in% names(raw)) to_num(cofinancing) else NA_real_,
      cofinancing_share = if ("cofinancing_share" %in% names(raw)) to_num(cofinancing_share) else NA_real_,
      competition_index = if ("competition_index" %in% names(raw)) to_num(competition_index) else NA_real_,
      region = if ("region" %in% names(raw)) as.character(region) else NA_character_,
      direction = if ("direction" %in% names(raw)) as.character(direction) else NA_character_,
      log_sum_requested = log1p(pmax(sum_requested, 0)),
      log_cofinancing = log1p(pmax(cofinancing, 0)),
      log_cofinancing_share = log1p(pmax(cofinancing_share, 0)),
      log_competition_index = log1p(pmax(competition_index, 0))
    ) %>%
    filter(!is.na(href_key), href_key != "") %>%
    filter(!is.na(application_rating)) %>%
    filter(is.na(status_code) | status_code == 200) %>%
    filter(is.na(fetch_error) | fetch_error == "") %>%
    arrange(href_key, desc(application_rating)) %>%
    group_by(href_key) %>%
    slice(1) %>%
    ungroup() %>%
    select(
      href_key,
      href,
      application_rating,
      sum_requested,
      cofinancing,
      cofinancing_share,
      competition_index,
      region,
      direction,
      log_sum_requested,
      log_cofinancing,
      log_cofinancing_share,
      log_competition_index
    )
}

load_presi_status <- function() {
  presi_path <- resolve_existing_path(c("data/data_large/presi_variables_imputed.csv"))
  raw <- readr::read_csv(presi_path, show_col_types = FALSE)

  stopifnot(all(c("href", "project_status") %in% names(raw)))

  raw %>%
    mutate(
      href_key = extract_href_key(href),
      project_status = to_num(project_status),
      oss120b_quality = if ("appl_quality" %in% names(raw)) to_num(appl_quality) else NA_real_,
      supported_projects = if ("supported_projects" %in% names(raw)) to_num(supported_projects) else NA_real_,
      sum_grants = if ("sum_grants" %in% names(raw)) to_num(sum_grants) else NA_real_,
      sum_requested = if ("sum_requested" %in% names(raw)) to_num(sum_requested) else NA_real_,
      implementation_length_months = if ("implementation_length_months" %in% names(raw)) to_num(implementation_length_months) else NA_real_,
      cofinancing_share = if ("cofinancing_share" %in% names(raw)) to_num(cofinancing_share) else NA_real_,
      org_age = if ("org_age" %in% names(raw)) to_num(org_age) else NA_real_,
      badge_president_grants = if ("badge_president_grants" %in% names(raw)) to_num(badge_president_grants) else NA_real_,
      inn_raw = if ("inn" %in% names(raw)) as.character(inn) else NA_character_,
      badge_president_foundation = if ("badge_president_foundation" %in% names(raw)) to_num(badge_president_foundation) else NA_real_,
      badge_regional_grants = if ("badge_regional_grants" %in% names(raw)) to_num(badge_regional_grants) else NA_real_,
      badge_top_project = if ("badge_top_project" %in% names(raw)) to_num(badge_top_project) else NA_real_,
      badge_no_reporting = if ("badge_no_reporting" %in% names(raw)) to_num(badge_no_reporting) else NA_real_,
      badge_project_failure = if ("badge_project_failure" %in% names(raw)) to_num(badge_project_failure) else NA_real_,
      org_website = if ("org_website" %in% names(raw)) to_num(org_website) else NA_real_,
      target_age = if ("target_age" %in% names(raw)) to_num(target_age) else NA_real_,
      target_disability = if ("target_disability" %in% names(raw)) to_num(target_disability) else NA_real_,
      has_trad = if ("has_trad" %in% names(raw)) to_num(has_trad) else NA_real_,
      model_trad_vals = if ("trad_vals" %in% names(raw)) to_num(trad_vals) else NA_real_,
      model_target_age = if ("target_age" %in% names(raw)) to_num(target_age) else NA_real_,
      cofinancing = if ("cofinancing" %in% names(raw)) to_num(cofinancing) else NA_real_,
      competition_index = if ("competition_index" %in% names(raw)) to_num(competition_index) else NA_real_,
      region = if ("region" %in% names(raw)) as.character(region) else NA_character_,
      direction = if ("direction" %in% names(raw)) as.character(direction) else NA_character_,
      log_sum_requested = log1p(pmax(sum_requested, 0)),
      log_cofinancing = log1p(pmax(cofinancing, 0)),
      log_cofinancing_share = log1p(pmax(cofinancing_share, 0)),
      log_competition_index = log1p(pmax(competition_index, 0))
    ) %>%
    filter(!is.na(href_key), href_key != "") %>%
    arrange(href_key, desc(project_status)) %>%
    group_by(href_key) %>%
    slice(1) %>%
    ungroup() %>%
    # --- has_won_before: lagged cumulative wins by INN ---
    # badge_president_grants is cross-sectionally scraped and includes current-
    # round wins, creating reverse causation. has_won_before uses only prior rounds.
    # Missing INN (early rounds 2017-2018) is set to 0 — correct for inaugural rounds.
    { d <- .
      inn_lkp <- tryCatch(
        readr::read_csv("data/data_large/all_presi_variables.csv",
                        col_select = c("href", "inn"), show_col_types = FALSE) %>%
          dplyr::distinct(href, .keep_all = TRUE) %>%
          dplyr::rename(inn_lkp = inn),
        error = function(e) NULL
      )
      if (!is.null(inn_lkp)) {
        d <- d %>% dplyr::left_join(inn_lkp, by = "href")
        opw <- d %>%
          dplyr::filter(!is.na(inn_lkp)) %>%
          dplyr::group_by(inn_lkp, competition_index) %>%
          dplyr::summarise(
            won_this_comp    = as.integer(any(project_status == 1)),
            n_wins_this_comp = sum(project_status == 1),
            grants_this_comp = sum(sum_requested[project_status == 1], na.rm = TRUE),
            .groups = "drop"
          ) %>%
          dplyr::arrange(inn_lkp, competition_index) %>%
          dplyr::group_by(inn_lkp) %>%
          dplyr::mutate(
            has_won_before     = as.integer(dplyr::lag(cumsum(won_this_comp), default = 0) > 0),
            cum_prior_projects = dplyr::lag(cumsum(n_wins_this_comp), default = 0L),
            cum_prior_grants_m = dplyr::lag(cumsum(grants_this_comp), default = 0) / 1e6
          ) %>%
          dplyr::ungroup() %>%
          dplyr::select(inn_lkp, competition_index, has_won_before, cum_prior_projects, cum_prior_grants_m)
        d <- d %>%
          dplyr::left_join(opw, by = c("inn_lkp", "competition_index")) %>%
          dplyr::mutate(
            has_won_before     = tidyr::replace_na(has_won_before, 0L),
            cum_prior_projects = tidyr::replace_na(cum_prior_projects, 0L),
            cum_prior_grants_m = tidyr::replace_na(cum_prior_grants_m, 0)
          ) %>%
          dplyr::select(-inn_lkp)
      } else {
        d <- d %>% dplyr::mutate(
          has_won_before     = 0L,
          cum_prior_projects = 0L,
          cum_prior_grants_m = 0
        )
      }
      d
    } %>%
    select(
      href_key,
      project_status,
      oss120b_quality,
      cum_prior_projects,
      cum_prior_grants_m,
      sum_requested,
      implementation_length_months,
      cofinancing_share,
      org_age,
      has_won_before,
      badge_president_foundation,
      badge_regional_grants,
      badge_top_project,
      badge_no_reporting,
      badge_project_failure,
      org_website,
      target_age,
      target_disability,
      has_trad,
      model_trad_vals,
      model_target_age,
      cofinancing,
      competition_index,
      region,
      direction,
      log_sum_requested,
      log_cofinancing,
      log_cofinancing_share,
      log_competition_index
    )
}

load_manual_labels <- function() {
  manual_path <- resolve_existing_path(c(
    "data/data_large/manual_trad_age_quality_labels_gena_20260214.csv",
    "manual_trad_age_quality_labels_gena_20260214.csv"
  ))

  raw <- readr::read_csv(manual_path, show_col_types = FALSE)
  stopifnot("href" %in% names(raw))

  row_id_num <- if ("row_id" %in% names(raw)) to_num(raw$row_id) else seq_len(nrow(raw))
  submitted_at_chr <- if ("submitted_at" %in% names(raw)) as.character(raw$submitted_at) else NA_character_

  out <- raw %>%
    mutate(
      href_key = extract_href_key(href),
      manual_trad_vals = if ("trad_vals" %in% names(raw)) to_num(trad_vals) else NA_real_,
      manual_age = if ("age" %in% names(raw)) to_num(age) else NA_real_,
      manual_relevance = if ("relevance" %in% names(raw)) to_num(relevance) else NA_real_,
      manual_coherence = if ("coherence" %in% names(raw)) to_num(coherence) else NA_real_,
      manual_budget_realism = if ("budget_realism" %in% names(raw)) to_num(budget_realism) else NA_real_,
      manual_scale = if ("scale" %in% names(raw)) to_num(scale) else NA_real_,
      manual_preliminary_score = if ("preliminary_score" %in% names(raw)) to_num(preliminary_score) else NA_real_,
      manual_quality_recomputed = rowMeans(
        cbind(manual_relevance, manual_coherence, manual_budget_realism, manual_scale),
        na.rm = TRUE
      ),
      manual_quality_recomputed = if_else(
        is.finite(manual_quality_recomputed),
        manual_quality_recomputed,
        NA_real_
      ),
      manual_quality = if_else(
        !is.na(manual_preliminary_score),
        manual_preliminary_score,
        manual_quality_recomputed
      ),
      submitted_at = submitted_at_chr,
      submitted_at_ts = suppressWarnings(as.numeric(as.POSIXct(submitted_at_chr, tz = "UTC"))),
      row_id = row_id_num,
      valid_trad = manual_trad_vals %in% c(0, 1),
      valid_age = !is.na(manual_age) & manual_age >= 0 & manual_age <= 120,
      valid_components =
        !is.na(manual_relevance) & dplyr::between(manual_relevance, 0, 100) &
        !is.na(manual_coherence) & dplyr::between(manual_coherence, 0, 100) &
        !is.na(manual_budget_realism) & dplyr::between(manual_budget_realism, 0, 100) &
        !is.na(manual_scale) & dplyr::between(manual_scale, 0, 100),
      valid_quality = !is.na(manual_quality) & dplyr::between(manual_quality, 0, 100),
      valid_row = valid_trad & valid_age & valid_components & valid_quality
    ) %>%
    filter(!is.na(href_key), href_key != "") %>%
    arrange(href_key, desc(valid_row), desc(submitted_at_ts), desc(row_id)) %>%
    group_by(href_key) %>%
    slice(1) %>%
    ungroup() %>%
    select(
      href_key,
      href,
      row_id,
      submitted_at,
      manual_trad_vals,
      manual_age,
      manual_relevance,
      manual_coherence,
      manual_budget_realism,
      manual_scale,
      manual_preliminary_score,
      manual_quality,
      valid_trad,
      valid_age,
      valid_components,
      valid_quality,
      valid_row
    )

  out
}

.validation_cache <- new.env(parent = emptyenv())

prepare_validation_data <- function(force = FALSE) {
  if (!force && exists("bundle", envir = .validation_cache, inherits = FALSE)) {
    return(get("bundle", envir = .validation_cache, inherits = FALSE))
  }

  gemini <- load_gemini25_scores()
  gov <- load_government_scores()
  presi <- load_presi_status()
  manual <- load_manual_labels()
  manual_valid <- manual %>% filter(valid_row)

  gemini_vs_gov <- gemini %>% inner_join(gov, by = "href_key")
  gemini_vs_status <- gemini %>% inner_join(presi %>% filter(project_status %in% c(0, 1)), by = "href_key")
  gemini_vs_presi <- gemini %>% inner_join(presi, by = "href_key")
  gemini_vs_manual <- gemini %>% inner_join(manual_valid, by = "href_key")

  triangulation <- manual_valid %>%
    inner_join(gemini, by = "href_key") %>%
    left_join(gov %>% select(href_key, application_rating), by = "href_key") %>%
    left_join(
      presi %>% select(href_key, project_status, model_target_age),
      by = "href_key"
    )

  bundle <- list(
    gemini = gemini,
    gov = gov,
    presi = presi,
    manual = manual,
    manual_valid = manual_valid,
    gemini_vs_gov = gemini_vs_gov,
    gemini_vs_status = gemini_vs_status,
    gemini_vs_presi = gemini_vs_presi,
    gemini_vs_manual = gemini_vs_manual,
    triangulation = triangulation
  )

  assign("bundle", bundle, envir = .validation_cache)
  bundle
}

run_gemini25_vs_oss120b_quality_validation <- function() {
  bundle <- prepare_validation_data()

  merged <- bundle$gemini_vs_presi %>%
    filter(!is.na(gemini_quality), !is.na(oss120b_quality))

  out_merged <- file.path(validation_results_dir, paste0("gemini25_vs_oss120b_quality_merged_", validation_run_tag, ".csv"))
  out_report <- file.path(validation_results_dir, paste0("gemini25_vs_oss120b_quality_report_", validation_run_tag, ".txt"))
  plot_scatter <- file.path(validation_graph_dir, paste0("gemini25_vs_oss120b_scatter_", validation_run_tag, ".png"))
  plot_binned <- file.path(validation_graph_dir, paste0("gemini25_vs_oss120b_binned_", validation_run_tag, ".png"))

  readr::write_csv(merged, out_merged)

  pear <- safe_cor(merged$gemini_quality, merged$oss120b_quality, "pearson")
  spear <- safe_cor(merged$gemini_quality, merged$oss120b_quality, "spearman")
  kend <- safe_cor(merged$gemini_quality, merged$oss120b_quality, "kendall")

  lm_basic <- lm(oss120b_quality ~ gemini_quality, data = merged)
  lm_basic_coef <- broom::tidy(lm_basic)
  r2_basic <- summary(lm_basic)$r.squared

  merged_dims <- merged %>%
    filter(!is.na(relevance), !is.na(coherence), !is.na(budget_realism), !is.na(scale))
  lm_dims <- lm(oss120b_quality ~ relevance + coherence + budget_realism + scale, data = merged_dims)
  r2_dims <- summary(lm_dims)$r.squared

  # Binned check (mean oss120b by Gemini bins)
  n_bins <- 20
  binned <- merged %>%
    mutate(bin = dplyr::ntile(gemini_quality, n_bins)) %>%
    group_by(bin) %>%
    summarise(
      n = n(),
      gemini_mean = mean(gemini_quality, na.rm = TRUE),
      oss_mean = mean(oss120b_quality, na.rm = TRUE),
      oss_sd = sd(oss120b_quality, na.rm = TRUE),
      .groups = "drop"
    )
  binned_pear <- safe_cor(binned$gemini_mean, binned$oss_mean, "pearson")
  binned_spear <- safe_cor(binned$gemini_mean, binned$oss_mean, "spearman")
  binned_r2 <- tryCatch(
    {
      if (nrow(binned) < 2) NA_real_ else summary(lm(oss_mean ~ gemini_mean, data = binned))$r.squared
    },
    error = function(e) NA_real_
  )

  # Victory prediction AUCs on status-defined subset (if available)
  status <- merged %>% filter(project_status %in% c(0, 1))
  auc_gemini <- safe_auc(status$project_status, status$gemini_quality)
  auc_oss <- safe_auc(status$project_status, status$oss120b_quality)

  p_scatter <- ggplot(merged, aes(x = gemini_quality, y = oss120b_quality)) +
    geom_point(alpha = 0.10, color = "#1f77b4", size = 1.1) +
    geom_smooth(method = "lm", se = FALSE, color = "#d62728", linewidth = 1) +
    labs(
      title = "Gemini 2.5 quality vs OSS120B quality",
      subtitle = paste0("N = ", nrow(merged), " (merged by href_key)"),
      x = "Gemini 2.5 composite quality",
      y = "OSS120B (presi_variables_imputed: appl_quality)"
    ) +
    theme_minimal(base_size = 12)

  p_binned <- ggplot(binned, aes(x = gemini_mean, y = oss_mean)) +
    geom_point(size = 2.2, color = "#1f77b4") +
    geom_line(color = "#1f77b4") +
    geom_smooth(method = "lm", se = FALSE, color = "#d62728") +
    labs(
      title = "Binned means: OSS120B quality by Gemini 2.5 bins",
      subtitle = paste0(n_bins, " equal-count Gemini bins"),
      x = "Mean Gemini 2.5 quality (bin)",
      y = "Mean OSS120B quality (bin)"
    ) +
    theme_minimal(base_size = 12)

  save_plot(p_scatter, plot_scatter, width = 8.5, height = 6)
  save_plot(p_binned, plot_binned, width = 8.5, height = 6)

  lines <- c(
    "GEMINI 2.5 VS OSS120B QUALITY (PRESI_VARIABLES_IMPUTED)",
    "====================================================",
    "",
    sprintf("Rows with both scores: %s", nrow(merged)),
    sprintf("OSS120B column used: appl_quality (loaded as oss120b_quality)"),
    "",
    "Association:",
    sprintf("- Pearson: %s | Spearman: %s | Kendall: %s", fmt(pear), fmt(spear), fmt(kend)),
    "",
    "Calibration regression:",
    sprintf(
      "- OLS: oss120b = %s + %s * gemini | R2=%s",
      fmt(lm_basic_coef$estimate[lm_basic_coef$term == "(Intercept)"]),
      fmt(lm_basic_coef$estimate[lm_basic_coef$term == "gemini_quality"]),
      fmt(r2_basic)
    ),
    sprintf("- OLS (4 components): oss120b ~ relevance+coherence+budget_realism+scale | R2=%s | N=%s",
            fmt(r2_dims), nrow(merged_dims)),
    "",
    "Binning check (mean oss120b by Gemini bins):",
    sprintf("- Bins: %s", nrow(binned)),
    sprintf("- Binned Pearson: %s | Spearman: %s | R2 (lm on bin means): %s",
            fmt(binned_pear), fmt(binned_spear), fmt(binned_r2)),
    "",
    "Victory prediction (AUC on rows with project_status in {0,1}):",
    sprintf("- Gemini composite AUC: %s", fmt(auc_gemini)),
    sprintf("- OSS120B quality AUC: %s", fmt(auc_oss)),
    "",
    "Saved outputs:",
    sprintf("- merged: %s", out_merged),
    sprintf("- report: %s", out_report),
    sprintf("- scatter: %s", plot_scatter),
    sprintf("- binned: %s", plot_binned)
  )

  write_report(lines, out_report)
  invisible(list(merged = out_merged, report = out_report))
}

run_manual_vs_winner_loser_validation <- function() {
  bundle <- prepare_validation_data()

  merged <- bundle$manual_valid %>%
    left_join(bundle$presi %>% select(href_key, project_status), by = "href_key") %>%
    filter(project_status %in% c(0, 1)) %>%
    mutate(
      winner_flag = as.integer(project_status),
      human_quality = manual_quality,
      status_label = factor(if_else(winner_flag == 1, "Winner", "Loser"), levels = c("Loser", "Winner"))
    )

  out_merge <- file.path(validation_results_dir, paste0("gemini25_human_vs_winner_loser_merged_", validation_run_tag, ".csv"))
  out_report <- file.path(validation_results_dir, paste0("gemini25_human_vs_winner_loser_report_", validation_run_tag, ".txt"))
  plot_density <- file.path(validation_graph_dir, paste0("human_quality_density_by_winner_loser_", validation_run_tag, ".png"))
  plot_roc <- file.path(validation_graph_dir, paste0("human_vs_winner_loser_roc_", validation_run_tag, ".png"))

  readr::write_csv(merged, out_merge)

  auc_human <- safe_auc(merged$winner_flag, merged$human_quality)
  roc_human <- NULL
  best_thr <- NA_real_

  if (nrow(merged) > 1 && length(unique(merged$winner_flag)) == 2) {
    roc_human <- pROC::roc(merged$winner_flag, merged$human_quality, quiet = TRUE)
    best_thr <- as.numeric(pROC::coords(roc_human, x = "best", best.method = "youden", ret = "threshold"))
  }

  pred_fix60 <- if_else(merged$human_quality >= 60, 1, 0)
  pred_best <- if (is.na(best_thr)) pred_fix60 else if_else(merged$human_quality >= best_thr, 1, 0)

  cm_fix60 <- conf_metrics(merged$winner_flag, pred_fix60)
  cm_best <- conf_metrics(merged$winner_flag, pred_best)

  p_density <- ggplot(merged, aes(x = human_quality, fill = status_label, color = status_label)) +
    geom_density(alpha = 0.45, linewidth = 1) +
    geom_vline(xintercept = 60, linetype = "dashed", color = "gray45") +
    scale_fill_manual(values = c("Loser" = "#efb5b3", "Winner" = "#72c9cb")) +
    scale_color_manual(values = c("Loser" = "#111111", "Winner" = "#0b5157")) +
    labs(
      title = "Human quality by winner/loser status",
      subtitle = paste0("Manual labels with status overlap | N=", nrow(merged)),
      x = "Human quality",
      y = "Density",
      fill = "Status",
      color = "Status"
    ) +
    coord_cartesian(xlim = c(0, 100)) +
    theme_minimal(base_size = 12)

  save_plot(p_density, plot_density, width = 10, height = 8)

  if (!is.null(roc_human)) {
    roc_df <- data.frame(
      tpr = roc_human$sensitivities,
      fpr = 1 - roc_human$specificities
    )

    p_roc <- ggplot(roc_df, aes(x = fpr, y = tpr)) +
      geom_line(color = "#0b5157", linewidth = 1.2) +
      geom_abline(linetype = "dashed", alpha = 0.6) +
      coord_equal() +
      labs(
        title = "Human quality ROC for winner/loser",
        subtitle = paste0("AUC=", fmt(auc_human)),
        x = "False positive rate",
        y = "True positive rate"
      ) +
      theme_minimal(base_size = 12)

    save_plot(p_roc, plot_roc, width = 8, height = 6.5)
  }

  lines <- c(
    "HUMAN QUALITY VS WINNER/LOSER STATUS",
    "===================================",
    "",
    sprintf("Rows with manual quality + winner/loser status: %s", nrow(merged)),
    sprintf("Winners: %s | Losers: %s", sum(merged$winner_flag == 1, na.rm = TRUE), sum(merged$winner_flag == 0, na.rm = TRUE)),
    "",
    sprintf("- AUC (human quality): %s", fmt(auc_human)),
    sprintf("- Best threshold (Youden): %s", fmt(best_thr, 2)),
    sprintf(
      "- Fixed threshold 60 -> accuracy=%s, precision=%s, recall=%s, specificity=%s, f1=%s",
      fmt(cm_fix60[["accuracy"]]),
      fmt(cm_fix60[["precision"]]),
      fmt(cm_fix60[["recall"]]),
      fmt(cm_fix60[["specificity"]]),
      fmt(cm_fix60[["f1"]])
    ),
    sprintf(
      "- Best threshold -> accuracy=%s, precision=%s, recall=%s, specificity=%s, f1=%s",
      fmt(cm_best[["accuracy"]]),
      fmt(cm_best[["precision"]]),
      fmt(cm_best[["recall"]]),
      fmt(cm_best[["specificity"]]),
      fmt(cm_best[["f1"]])
    ),
    "",
    "Saved outputs:",
    sprintf("- merged: %s", out_merge),
    sprintf("- report: %s", out_report),
    sprintf("- density: %s", plot_density),
    sprintf("- roc: %s", plot_roc)
  )

  write_report(lines, out_report)
  invisible(list(merged = out_merge, report = out_report, density = plot_density, roc = plot_roc))
}

