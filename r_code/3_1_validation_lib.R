pacman::p_load(
  dplyr,
  readr,
  stringr,
  ggplot2,
  broom,
  pROC,
  tidyr,
  splines
)

if (!requireNamespace("xgboost", quietly = TRUE)) {
  message("NOTE: xgboost is not installed. XGB script will write a skip report.")
}

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

reg_metrics <- function(y, yhat) {
  keep <- !is.na(y) & !is.na(yhat)
  yy <- y[keep]
  pp <- yhat[keep]

  if (length(yy) == 0) {
    return(tibble::tibble(
      n = 0,
      mae = NA_real_,
      medae = NA_real_,
      rmse = NA_real_,
      pearson = NA_real_,
      spearman = NA_real_,
      kendall = NA_real_,
      bias = NA_real_,
      r2 = NA_real_
    ))
  }

  ss_res <- sum((yy - pp)^2, na.rm = TRUE)
  ss_tot <- sum((yy - mean(yy, na.rm = TRUE))^2, na.rm = TRUE)
  r2 <- if (is.finite(ss_tot) && ss_tot > 0) 1 - ss_res / ss_tot else NA_real_

  tibble::tibble(
    n = length(yy),
    mae = mean(abs(yy - pp), na.rm = TRUE),
    medae = median(abs(yy - pp), na.rm = TRUE),
    rmse = sqrt(mean((yy - pp)^2, na.rm = TRUE)),
    pearson = safe_cor(yy, pp, "pearson"),
    spearman = safe_cor(yy, pp, "spearman"),
    kendall = safe_cor(yy, pp, "kendall"),
    bias = mean(pp - yy, na.rm = TRUE),
    r2 = r2
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

align_model_matrices <- function(mm_train, mm_test) {
  miss <- setdiff(colnames(mm_train), colnames(mm_test))
  if (length(miss) > 0) {
    add <- matrix(0, nrow = nrow(mm_test), ncol = length(miss))
    colnames(add) <- miss
    mm_test <- cbind(mm_test, add)
  }

  extra <- setdiff(colnames(mm_test), colnames(mm_train))
  if (length(extra) > 0) {
    mm_test <- mm_test[, setdiff(colnames(mm_test), extra), drop = FALSE]
  }

  mm_test <- mm_test[, colnames(mm_train), drop = FALSE]
  mm_test
}

fit_predict_quantile_map <- function(x_train, y_train, x_test) {
  probs <- pmin(pmax(stats::ecdf(x_train)(x_test), 0), 1)
  as.numeric(stats::quantile(y_train, probs = probs, type = 8, na.rm = TRUE))
}

fit_predict_isotonic <- function(x_train, y_train, x_test) {
  ord <- order(x_train)
  x_ord <- x_train[ord]
  y_ord <- y_train[ord]
  iso <- stats::isoreg(x_ord, y_ord)
  f <- stats::approxfun(iso$x, iso$yf, method = "linear", ties = mean, rule = 2)
  as.numeric(f(x_test))
}

cv_predict_lm <- function(data, formula_obj, y_col = "y", k = 5, seed = 42) {
  vars <- unique(c(y_col, all.vars(formula_obj)))
  d <- data[stats::complete.cases(data[, vars, drop = FALSE]), , drop = FALSE]
  n <- nrow(d)

  if (n < 40) {
    return(list(obs = d[[y_col]], pred = rep(NA_real_, n), n = n, k = 0))
  }

  k_use <- min(k, max(2, floor(n / 25)))
  set.seed(seed)
  fold <- sample(rep(seq_len(k_use), length.out = n))
  pred <- rep(NA_real_, n)

  for (f in seq_len(k_use)) {
    tr <- d[fold != f, , drop = FALSE]
    te <- d[fold == f, , drop = FALSE]
    fit <- tryCatch(lm(formula_obj, data = tr), error = function(e) NULL)
    if (!is.null(fit)) {
      if (!is.null(fit$xlevels) && length(fit$xlevels) > 0) {
        for (nm in names(fit$xlevels)) {
          if (nm %in% names(te)) {
            te[[nm]] <- factor(as.character(te[[nm]]), levels = fit$xlevels[[nm]])
          }
        }
      }
      pred[fold == f] <- suppressWarnings(as.numeric(stats::predict(fit, newdata = te)))
    }
  }

  list(obs = d[[y_col]], pred = pred, n = n, k = k_use)
}

pair_metrics_tbl <- function(df, x_col, y_col, pair_name) {
  if (nrow(df) == 0 || !(x_col %in% names(df)) || !(y_col %in% names(df))) {
    return(tibble::tibble(
      pair = pair_name,
      n = 0,
      mae = NA_real_,
      rmse = NA_real_,
      pearson = NA_real_,
      spearman = NA_real_,
      kendall = NA_real_
    ))
  }

  x <- to_num(df[[x_col]])
  y <- to_num(df[[y_col]])
  met <- reg_metrics(y, x)
  tibble::tibble(
    pair = pair_name,
    n = met$n,
    mae = met$mae,
    rmse = met$rmse,
    pearson = met$pearson,
    spearman = met$spearman,
    kendall = met$kendall
  )
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

cv_predict_glm <- function(data, formula_obj, y_col = "y", id_col = NULL, k = 5, seed = 42) {
  vars <- unique(c(y_col, all.vars(formula_obj)))
  d <- data[stats::complete.cases(data[, vars, drop = FALSE]), , drop = FALSE]
  n <- nrow(d)

  ids <- if (!is.null(id_col) && id_col %in% names(d)) d[[id_col]] else seq_len(n)

  if (n < 1000) {
    return(list(obs = d[[y_col]], pred = rep(NA_real_, n), n = n, k = 0))
  }

  k_use <- min(k, max(2, floor(n / 25000)))
  set.seed(seed)
  fold <- sample(rep(seq_len(k_use), length.out = n))
  pred <- rep(NA_real_, n)

  for (f in seq_len(k_use)) {
    tr <- d[fold != f, , drop = FALSE]
    te <- d[fold == f, , drop = FALSE]

    fit <- tryCatch(glm(formula_obj, data = tr, family = binomial), error = function(e) NULL)
    if (!is.null(fit)) {
      te_glm <- te
      if (!is.null(fit$xlevels) && length(fit$xlevels) > 0) {
        for (nm in names(fit$xlevels)) {
          if (nm %in% names(te_glm)) {
            te_glm[[nm]] <- factor(as.character(te_glm[[nm]]), levels = fit$xlevels[[nm]])
          }
        }
      }
      pred[fold == f] <- suppressWarnings(as.numeric(stats::predict(fit, newdata = te_glm, type = "response")))
    }
  }

  list(obs = d[[y_col]], pred = pred, id = ids, n = n, k = k_use)
}

run_gemini25_full_model_victory_validation <- function() {
  bundle <- prepare_validation_data()

  presi_model <- bundle$gemini_vs_status %>%
    mutate(
      y = as.integer(project_status),
      direction_f = factor(stringr::str_trim(as.character(direction))),
      region_f = factor(stringr::str_trim(as.character(region)))
    )

  out_report <- file.path(validation_results_dir, paste0("gemini25_full_model_victory_report_", validation_run_tag, ".txt"))
  out_preds <- file.path(validation_results_dir, paste0("gemini25_full_model_victory_oof_predictions_", validation_run_tag, ".csv"))
  plot_roc <- file.path(validation_graph_dir, paste0("gemini25_full_model_victory_roc_", validation_run_tag, ".png"))
  plot_cal <- file.path(validation_graph_dir, paste0("gemini25_full_model_victory_calibration_", validation_run_tag, ".png"))

  # Models
  formula_full <- y ~ gemini_quality +
    cum_prior_projects +
    cum_prior_grants_m +
    sum_requested +
    implementation_length_months +
    cofinancing_share +
    has_won_before +
    badge_president_foundation +
    badge_regional_grants +
    badge_top_project +
    badge_no_reporting +
    badge_project_failure +
    org_website +
    direction_f +
    competition_index +
    region_f

  formula_controls <- update(formula_full, . ~ . - gemini_quality)
  formula_gemini_only <- y ~ gemini_quality

  # Ensure fair comparison: restrict all models to full-formula complete cases
  vars_full <- unique(c("y", all.vars(formula_full)))
  d_common <- presi_model[stats::complete.cases(presi_model[, vars_full, drop = FALSE]), , drop = FALSE]

  cv_full <- cv_predict_glm(d_common, formula_full, y_col = "y", id_col = "href_key", k = 5, seed = 42)
  cv_ctrl <- cv_predict_glm(d_common, formula_controls, y_col = "y", id_col = "href_key", k = 5, seed = 42)
  cv_gem <- cv_predict_glm(d_common, formula_gemini_only, y_col = "y", id_col = "href_key", k = 5, seed = 42)

  auc_full <- safe_auc(cv_full$obs, cv_full$pred)
  auc_ctrl <- safe_auc(cv_ctrl$obs, cv_ctrl$pred)
  auc_gem <- safe_auc(cv_gem$obs, cv_gem$pred)

  binned_r2 <- function(y, pred, bins = 20) {
    d <- tibble::tibble(y = y, p = pred) %>%
      filter(!is.na(y), !is.na(p))
    if (nrow(d) < 10) {
      return(list(bins = 0L, r2 = NA_real_, y_min = NA_real_, y_max = NA_real_, p_min = NA_real_, p_max = NA_real_, avg_n = NA_real_))
    }

    dd <- d %>%
      mutate(bin = dplyr::ntile(p, bins)) %>%
      group_by(bin) %>%
      summarise(
        n = n(),
        p_mean = mean(p, na.rm = TRUE),
        y_mean = mean(y, na.rm = TRUE),
        .groups = "drop"
      )

    if (nrow(dd) < 2) {
      return(list(bins = nrow(dd), r2 = NA_real_, y_min = NA_real_, y_max = NA_real_, p_min = NA_real_, p_max = NA_real_, avg_n = NA_real_))
    }

    fit <- lm(y_mean ~ p_mean, data = dd)
    r2 <- summary(fit)$r.squared

    list(
      bins = nrow(dd),
      r2 = r2,
      y_min = min(dd$y_mean, na.rm = TRUE),
      y_max = max(dd$y_mean, na.rm = TRUE),
      p_min = min(dd$p_mean, na.rm = TRUE),
      p_max = max(dd$p_mean, na.rm = TRUE),
      avg_n = mean(dd$n, na.rm = TRUE)
    )
  }

  # Fit full model on all complete cases for coefficient table
  fit_full <- glm(formula_full, data = d_common, family = binomial)
  coef_full <- broom::tidy(fit_full)
  gem_row <- coef_full %>% filter(term == "gemini_quality")
  gem_beta <- if (nrow(gem_row) == 1) gem_row$estimate else NA_real_
  gem_or <- if (is.finite(gem_beta)) exp(gem_beta) else NA_real_
  gem_p <- if (nrow(gem_row) == 1) gem_row$p.value else NA_real_

  # Store OOF preds
  pred_tbl <- tibble::tibble(
    href_key = cv_full$id,
    y = cv_full$obs,
    pred_gemini_only = cv_gem$pred,
    pred_controls = cv_ctrl$pred,
    pred_full = cv_full$pred
  )
  readr::write_csv(pred_tbl, out_preds)

  # Regression-style metrics on probability scale (OOF)
  met_gem <- reg_metrics(pred_tbl$y, pred_tbl$pred_gemini_only)
  met_ctrl <- reg_metrics(pred_tbl$y, pred_tbl$pred_controls)
  met_full <- reg_metrics(pred_tbl$y, pred_tbl$pred_full)

  bin_gem <- binned_r2(pred_tbl$y, pred_tbl$pred_gemini_only, bins = 20)
  bin_ctrl <- binned_r2(pred_tbl$y, pred_tbl$pred_controls, bins = 20)
  bin_full <- binned_r2(pred_tbl$y, pred_tbl$pred_full, bins = 20)

  # ROC plot
  roc_df <- bind_rows(
    data.frame(model = "gemini_only", tpr = pROC::roc(cv_gem$obs, cv_gem$pred, quiet = TRUE)$sensitivities, fpr = 1 - pROC::roc(cv_gem$obs, cv_gem$pred, quiet = TRUE)$specificities),
    data.frame(model = "controls", tpr = pROC::roc(cv_ctrl$obs, cv_ctrl$pred, quiet = TRUE)$sensitivities, fpr = 1 - pROC::roc(cv_ctrl$obs, cv_ctrl$pred, quiet = TRUE)$specificities),
    data.frame(model = "controls_plus_gemini", tpr = pROC::roc(cv_full$obs, cv_full$pred, quiet = TRUE)$sensitivities, fpr = 1 - pROC::roc(cv_full$obs, cv_full$pred, quiet = TRUE)$specificities)
  )

  p_roc <- ggplot(roc_df, aes(x = fpr, y = tpr, color = model)) +
    geom_line(linewidth = 1) +
    geom_abline(linetype = "dashed", alpha = 0.6) +
    coord_equal() +
    labs(
      title = "Winner/loser ROC: controls vs Gemini",
      subtitle = paste0(
        "AUC gemini_only=", fmt(auc_gem),
        " | controls=", fmt(auc_ctrl),
        " | +gemini=", fmt(auc_full)
      ),
      x = "False positive rate",
      y = "True positive rate",
      color = "Model"
    ) +
    theme_minimal(base_size = 12)

  save_plot(p_roc, plot_roc, width = 8.5, height = 6.5)

  # Calibration (full model)
  calib <- pred_tbl %>%
    filter(!is.na(pred_full), !is.na(y)) %>%
    mutate(bin = ntile(pred_full, 10)) %>%
    group_by(bin) %>%
    summarise(
      n = n(),
      p_mean = mean(pred_full, na.rm = TRUE),
      y_mean = mean(y, na.rm = TRUE),
      .groups = "drop"
    )

  p_cal <- ggplot(calib, aes(x = p_mean, y = y_mean)) +
    geom_point(aes(size = n), alpha = 0.8, color = "#1f77b4") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray45") +
    labs(
      title = "Calibration (controls + Gemini full model)",
      subtitle = "10 bins over OOF predicted probabilities",
      x = "Predicted Pr(win)",
      y = "Observed win rate",
      size = "N"
    ) +
    theme_minimal(base_size = 12)

  save_plot(p_cal, plot_cal, width = 8, height = 6)

  lines <- c(
    "GEMINI 2.5 FULL MODEL VALIDATION (WINNER/LOSER)",
    "=============================================",
    "",
    sprintf("Rows with status (0/1) and Gemini: %s", nrow(bundle$gemini_vs_status)),
    sprintf("Rows used in full-model complete-case fit: %s", nrow(d_common)),
    sprintf("CV folds used (adaptive): full=%s | controls=%s | gemini_only=%s", cv_full$k, cv_ctrl$k, cv_gem$k),
    "",
    "OOF AUC (5-fold-ish CV):",
    sprintf("- Gemini only: %s", fmt(auc_gem)),
    sprintf("- Controls only: %s", fmt(auc_ctrl)),
    sprintf("- Controls + Gemini: %s", fmt(auc_full)),
    sprintf("- Delta AUC (+Gemini): %s", fmt(auc_full - auc_ctrl)),
    "",
    "OOF error metrics on probability scale (y in {0,1}, pred in [0,1]):",
    sprintf("- Gemini only: R2=%s | MAE=%s | RMSE=%s", fmt(met_gem$r2), fmt(met_gem$mae), fmt(met_gem$rmse)),
    sprintf("- Controls only: R2=%s | MAE=%s | RMSE=%s", fmt(met_ctrl$r2), fmt(met_ctrl$mae), fmt(met_ctrl$rmse)),
    sprintf("- Controls + Gemini: R2=%s | MAE=%s | RMSE=%s", fmt(met_full$r2), fmt(met_full$mae), fmt(met_full$rmse)),
    "",
    "20-bin calibration (OOF): R2 between bin mean predicted p and observed win rate:",
    sprintf("- Gemini only: bins=%s | R2=%s | y_mean range=%s..%s | p_mean range=%s..%s | avg bin N=%s",
            bin_gem$bins, fmt(bin_gem$r2), fmt(bin_gem$y_min, 4), fmt(bin_gem$y_max, 4), fmt(bin_gem$p_min, 4), fmt(bin_gem$p_max, 4), fmt(bin_gem$avg_n, 1)),
    sprintf("- Controls only: bins=%s | R2=%s | y_mean range=%s..%s | p_mean range=%s..%s | avg bin N=%s",
            bin_ctrl$bins, fmt(bin_ctrl$r2), fmt(bin_ctrl$y_min, 4), fmt(bin_ctrl$y_max, 4), fmt(bin_ctrl$p_min, 4), fmt(bin_ctrl$p_max, 4), fmt(bin_ctrl$avg_n, 1)),
    sprintf("- Controls + Gemini: bins=%s | R2=%s | y_mean range=%s..%s | p_mean range=%s..%s | avg bin N=%s",
            bin_full$bins, fmt(bin_full$r2), fmt(bin_full$y_min, 4), fmt(bin_full$y_max, 4), fmt(bin_full$p_min, 4), fmt(bin_full$p_max, 4), fmt(bin_full$avg_n, 1)),
    "",
    "Gemini effect in the full model (in-sample, complete cases):",
    sprintf("- beta(gemini_quality) = %s | OR per +1 point = %s | p = %s",
            fmt(gem_beta), fmt(gem_or), fmt(gem_p, 6)),
    sprintf("- OR per +10 points = %s", fmt(if (is.finite(gem_beta)) exp(10 * gem_beta) else NA_real_)),
    "",
    "Saved outputs:",
    sprintf("- report: %s", out_report),
    sprintf("- OOF predictions: %s", out_preds),
    sprintf("- ROC plot: %s", plot_roc),
    sprintf("- calibration plot: %s", plot_cal)
  )

  write_report(lines, out_report)
  invisible(list(report = out_report, preds = out_preds))
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

run_gemini25_vs_government_validation <- function() {
  bundle <- prepare_validation_data()
  merged <- bundle$gemini_vs_gov %>%
    mutate(
      abs_err = abs(gemini_quality - application_rating),
      diff = gemini_quality - application_rating
    )

  out_merged <- file.path(validation_results_dir, paste0("gemini25_vs_government_27k_quality_merged_", validation_run_tag, ".csv"))
  out_report <- file.path(validation_results_dir, paste0("gemini25_vs_government_27k_quality_report_", validation_run_tag, ".txt"))
  plot_scatter <- file.path(validation_graph_dir, paste0("gemini25_vs_government_27k_scatter_", validation_run_tag, ".png"))
  plot_binned <- file.path(validation_graph_dir, paste0("gemini25_vs_government_27k_binned_", validation_run_tag, ".png"))

  readr::write_csv(merged, out_merged)

  met <- reg_metrics(merged$application_rating, merged$gemini_quality)
  lm_fit <- lm(application_rating ~ gemini_quality, data = merged)
  lm_coef <- broom::tidy(lm_fit)

  pred_fit <- suppressWarnings(as.numeric(stats::predict(lm_fit, newdata = merged)))
  met_fit <- reg_metrics(merged$application_rating, pred_fit)

  # Scale-invariant / bias-adjusted diagnostics
  mu_gov <- mean(merged$application_rating, na.rm = TRUE)
  mu_gem <- mean(merged$gemini_quality, na.rm = TRUE)
  sd_gov <- stats::sd(merged$application_rating, na.rm = TRUE)
  sd_gem <- stats::sd(merged$gemini_quality, na.rm = TRUE)

  bias <- mean(merged$gemini_quality - merged$application_rating, na.rm = TRUE)
  pred_bias_adj <- merged$gemini_quality - bias
  met_bias_adj <- reg_metrics(merged$application_rating, pred_bias_adj)

  scale_ratio <- if (is.finite(sd_gov) && is.finite(sd_gem) && sd_gem > 0) sd_gov / sd_gem else NA_real_
  pred_loc_scale <- mu_gov + scale_ratio * (merged$gemini_quality - mu_gem)
  met_loc_scale <- reg_metrics(merged$application_rating, pred_loc_scale)

  z_gov <- if (is.finite(sd_gov) && sd_gov > 0) (merged$application_rating - mu_gov) / sd_gov else NA_real_
  z_gem <- if (is.finite(sd_gem) && sd_gem > 0) (merged$gemini_quality - mu_gem) / sd_gem else NA_real_
  met_z <- reg_metrics(z_gov, z_gem)

  cv_dat <- merged %>%
    transmute(y = application_rating, gemini_quality = gemini_quality)
  cv <- cv_predict_lm(cv_dat, y ~ gemini_quality, y_col = "y", k = 5, seed = 42)
  met_cv <- reg_metrics(cv$obs, cv$pred)

  p_scatter <- ggplot(merged, aes(x = gemini_quality, y = application_rating)) +
    geom_point(alpha = 0.20, color = "#1f77b4", size = 1.2) +
    geom_smooth(method = "lm", se = TRUE, color = "#d62728", linewidth = 1) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray45") +
    labs(
      title = "Gemini 2.5 vs government quality (27k winners)",
      subtitle = paste0("Merged N = ", nrow(merged)),
      x = "Gemini 2.5 composite quality",
      y = "Government application_rating"
    ) +
    theme_minimal(base_size = 12)

  n_bins <- min(20, max(3, nrow(merged)))
  binned <- merged %>%
    mutate(bin = dplyr::ntile(gemini_quality, n_bins)) %>%
    group_by(bin) %>%
    summarise(
      n = n(),
      gemini_mean = mean(gemini_quality, na.rm = TRUE),
      gov_mean = mean(application_rating, na.rm = TRUE),
      gov_sd = sd(application_rating, na.rm = TRUE),
      .groups = "drop"
    )

  binned_pear <- safe_cor(binned$gemini_mean, binned$gov_mean, "pearson")
  binned_spear <- safe_cor(binned$gemini_mean, binned$gov_mean, "spearman")
  binned_r2 <- tryCatch(
    {
      if (nrow(binned) < 2) NA_real_ else summary(lm(gov_mean ~ gemini_mean, data = binned))$r.squared
    },
    error = function(e) NA_real_
  )
  gov_mean_min <- suppressWarnings(min(binned$gov_mean, na.rm = TRUE))
  gov_mean_max <- suppressWarnings(max(binned$gov_mean, na.rm = TRUE))
  avg_within_bin_gov_sd <- suppressWarnings(mean(binned$gov_sd, na.rm = TRUE))

  p_binned <- ggplot(binned, aes(x = gemini_mean, y = gov_mean)) +
    geom_point(size = 2.2, color = "#1f77b4") +
    geom_line(color = "#1f77b4") +
    geom_smooth(method = "lm", se = TRUE, color = "#d62728") +
    labs(
      title = "Binned means: Gemini 2.5 vs government quality",
      subtitle = paste0(n_bins, " equal-count bins"),
      x = "Mean Gemini 2.5 quality",
      y = "Mean government application_rating"
    ) +
    theme_minimal(base_size = 12)

  save_plot(p_scatter, plot_scatter, width = 8.5, height = 6)
  save_plot(p_binned, plot_binned, width = 8.5, height = 6)

  lines <- c(
    "GEMINI 2.5 VS GOVERNMENT QUALITY (27K WINNERS)",
    "=============================================",
    "",
    sprintf("Gemini rows (deduped): %s", nrow(bundle$gemini)),
    sprintf("Government rows (deduped): %s", nrow(bundle$gov)),
    sprintf("Merged rows: %s", nrow(merged)),
    "",
    "Agreement metrics (relationship style; scale mismatch allowed):",
    sprintf("- Pearson: %s | Spearman: %s | Kendall: %s", fmt(met$pearson), fmt(met$spearman), fmt(met$kendall)),
    sprintf("- Mean bias (Gemini - Government): %s", fmt(bias)),
    sprintf("- SD gov: %s | SD gemini: %s", fmt(sd_gov, 4), fmt(sd_gem, 4)),
    sprintf(
      "- Calibration LM: gov = %s + %s * gemini | R2 = %s",
      fmt(lm_coef$estimate[lm_coef$term == "(Intercept)"]),
      fmt(lm_coef$estimate[lm_coef$term == "gemini_quality"]),
      fmt(summary(lm_fit)$r.squared)
    ),
    "",
    "Error checks after removing only scale artifacts:",
    sprintf("- Bias-adjusted (shift only): MAE=%s | RMSE=%s", fmt(met_bias_adj$mae), fmt(met_bias_adj$rmse)),
    sprintf("- Location+scale adjusted (match mean+SD): MAE=%s | RMSE=%s", fmt(met_loc_scale$mae), fmt(met_loc_scale$rmse)),
    sprintf("- Z-scored (both standardized): MAE=%s | RMSE=%s (errors in gov SD units)", fmt(met_z$mae), fmt(met_z$rmse)),
    "",
    "How well a simple linear mapping predicts government scores (uses correlation, not just moments):",
    sprintf("- In-sample (fitted): R2=%s | MAE=%s | RMSE=%s", fmt(met_fit$r2), fmt(met_fit$mae), fmt(met_fit$rmse)),
    sprintf("- 5-fold CV (OOF): R2=%s | MAE=%s | RMSE=%s", fmt(met_cv$r2), fmt(met_cv$mae), fmt(met_cv$rmse)),
    "",
    "Binning check (mean gov score by Gemini bins):",
    sprintf("- Bins: %s", nrow(binned)),
    sprintf("- Binned Pearson: %s | Spearman: %s | R2 (lm on bin means): %s",
            fmt(binned_pear), fmt(binned_spear), fmt(binned_r2)),
    sprintf("- Gov mean range across bins: %s to %s | Avg within-bin gov SD: %s",
            fmt(gov_mean_min, 2), fmt(gov_mean_max, 2), fmt(avg_within_bin_gov_sd, 2)),
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

run_gemini25_bins_plus_ivs_to_government_validation <- function(n_bins = 20) {
  bundle <- prepare_validation_data()

  dat <- bundle$gemini_vs_gov %>%
    transmute(
      href_key,
      y = application_rating,
      gemini_quality,
      gemini_bin = dplyr::ntile(gemini_quality, n_bins),
      gemini_bin_f = factor(gemini_bin),
      sum_requested,
      cofinancing,
      cofinancing_share,
      competition_index,
      log_sum_requested,
      log_cofinancing,
      log_cofinancing_share,
      log_competition_index,
      region_f = factor(stringr::str_trim(as.character(region))),
      direction_f = factor(stringr::str_trim(as.character(direction)))
    ) %>%
    filter(!is.na(y), !is.na(gemini_quality))

  out_report <- file.path(validation_results_dir, paste0("gemini25_bins_plus_ivs_to_gov_report_", validation_run_tag, ".txt"))
  out_preds <- file.path(validation_results_dir, paste0("gemini25_bins_plus_ivs_to_gov_oof_predictions_", validation_run_tag, ".csv"))
  plot_bin_effects <- file.path(validation_graph_dir, paste0("gemini25_bins_plus_ivs_bin_effects_", validation_run_tag, ".png"))

  # Model definitions
  f_bins_only <- y ~ gemini_bin_f
  f_ivs_only <- y ~ log_sum_requested + log_cofinancing + log_cofinancing_share + log_competition_index + region_f + direction_f
  f_bins_plus_ivs <- y ~ gemini_bin_f + log_sum_requested + log_cofinancing + log_cofinancing_share + log_competition_index + region_f + direction_f
  f_linear_gemini <- y ~ gemini_quality

  # Ensure fair comparison: restrict all models to bins+IVs complete cases
  vars_needed <- unique(c("y", all.vars(f_bins_plus_ivs)))
  d_common <- dat[stats::complete.cases(dat[, vars_needed, drop = FALSE]), , drop = FALSE]

  # CV predictions (same rows)
  cv_bins <- cv_predict_lm(d_common, f_bins_only, y_col = "y", k = 5, seed = 42)
  cv_ivs <- cv_predict_lm(d_common, f_ivs_only, y_col = "y", k = 5, seed = 42)
  cv_both <- cv_predict_lm(d_common, f_bins_plus_ivs, y_col = "y", k = 5, seed = 42)
  cv_lin <- cv_predict_lm(d_common, f_linear_gemini, y_col = "y", k = 5, seed = 42)

  met_bins <- reg_metrics(cv_bins$obs, cv_bins$pred)
  met_ivs <- reg_metrics(cv_ivs$obs, cv_ivs$pred)
  met_both <- reg_metrics(cv_both$obs, cv_both$pred)
  met_lin <- reg_metrics(cv_lin$obs, cv_lin$pred)

  # Report Spearman of predictions (ranking) too
  sp_bins <- safe_cor(cv_bins$obs, cv_bins$pred, "spearman")
  sp_ivs <- safe_cor(cv_ivs$obs, cv_ivs$pred, "spearman")
  sp_both <- safe_cor(cv_both$obs, cv_both$pred, "spearman")
  sp_lin <- safe_cor(cv_lin$obs, cv_lin$pred, "spearman")

  # Store OOF predictions
  pred_tbl <- tibble::tibble(
    href_key = d_common$href_key,
    y = cv_both$obs,
    pred_linear_gemini = cv_lin$pred,
    pred_bins_only = cv_bins$pred,
    pred_ivs_only = cv_ivs$pred,
    pred_bins_plus_ivs = cv_both$pred
  )
  readr::write_csv(pred_tbl, out_preds)

  # Fit full model on all complete cases for bin effects plot
  fit <- lm(f_bins_plus_ivs, data = d_common)
  coef_tbl <- broom::tidy(fit, conf.int = TRUE)

  bin_effects <- coef_tbl %>%
    filter(stringr::str_detect(term, "^gemini_bin_f")) %>%
    mutate(
      bin = as.integer(stringr::str_extract(term, "\\d+"))
    ) %>%
    arrange(bin)

  p_bin <- ggplot(bin_effects, aes(x = bin, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray45") +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.25, fill = "#1f77b4") +
    geom_line(color = "#1f77b4", linewidth = 1) +
    geom_point(color = "#1f77b4", size = 2) +
    labs(
      title = "Gemini-bin effects in government-score regression",
      subtitle = paste0(
        "Model: gov ~ gemini_bin + IVs + region/direction FE | N=", nrow(d_common),
        " | bins=", n_bins
      ),
      x = "Gemini quality bin (ntile)",
      y = "Bin coefficient vs baseline bin"
    ) +
    theme_minimal(base_size = 12)

  save_plot(p_bin, plot_bin_effects, width = 9, height = 6)

  lines <- c(
    "GEMINI 2.5 BINS + IVS MODEL (GOVERNMENT 27K SCORES)",
    "==================================================",
    "",
    sprintf("Rows in merged (Gemini + government): %s", nrow(bundle$gemini_vs_gov)),
    sprintf("Rows used (complete cases for bins+IVs): %s", nrow(d_common)),
    sprintf("Bins: %s (equal-count on gemini_quality)", n_bins),
    "",
    "5-fold CV (OOF) regression metrics (predicting government application_rating):",
    sprintf("- Linear Gemini only: R2=%s | MAE=%s | RMSE=%s | Spearman(pred,y)=%s",
            fmt(met_lin$r2), fmt(met_lin$mae), fmt(met_lin$rmse), fmt(sp_lin)),
    sprintf("- Bins only: R2=%s | MAE=%s | RMSE=%s | Spearman(pred,y)=%s",
            fmt(met_bins$r2), fmt(met_bins$mae), fmt(met_bins$rmse), fmt(sp_bins)),
    sprintf("- IVs only: R2=%s | MAE=%s | RMSE=%s | Spearman(pred,y)=%s",
            fmt(met_ivs$r2), fmt(met_ivs$mae), fmt(met_ivs$rmse), fmt(sp_ivs)),
    sprintf("- Bins + IVs: R2=%s | MAE=%s | RMSE=%s | Spearman(pred,y)=%s",
            fmt(met_both$r2), fmt(met_both$mae), fmt(met_both$rmse), fmt(sp_both)),
    "",
    "Saved outputs:",
    sprintf("- report: %s", out_report),
    sprintf("- OOF predictions: %s", out_preds),
    sprintf("- bin effects plot: %s", plot_bin_effects)
  )

  write_report(lines, out_report)
  invisible(list(report = out_report, preds = out_preds, plot = plot_bin_effects))
}

run_gemini25_fit_improvement <- function() {
  bundle <- prepare_validation_data()
  dat <- bundle$gemini_vs_gov %>%
    transmute(
      href_key,
      y = application_rating,
      x = gemini_quality,
      relevance,
      coherence,
      budget_realism,
      scale
    ) %>%
    filter(
      !is.na(y),
      !is.na(x),
      !is.na(relevance),
      !is.na(coherence),
      !is.na(budget_realism),
      !is.na(scale)
    )

  out_summary <- file.path(validation_results_dir, paste0("gemini25_fit_improvement_cv_summary_", validation_run_tag, ".csv"))
  out_preds <- file.path(validation_results_dir, paste0("gemini25_fit_improvement_cv_predictions_", validation_run_tag, ".csv"))
  out_report <- file.path(validation_results_dir, paste0("gemini25_fit_improvement_report_", validation_run_tag, ".txt"))
  plot_compare <- file.path(validation_graph_dir, paste0("gemini25_fit_improvement_scatter_compare_", validation_run_tag, ".png"))
  plot_calib <- file.path(validation_graph_dir, paste0("gemini25_fit_improvement_calibration_", validation_run_tag, ".png"))

  n <- nrow(dat)
  if (n < 50) {
    write_report(
      c(
        "GEMINI 2.5 FIT IMPROVEMENT",
        "==========================",
        "",
        sprintf("Insufficient rows for CV fit improvement: N=%s", n)
      ),
      out_report
    )
    return(invisible(NULL))
  }

  models <- c(
    "raw_gemini",
    "bias_corrected",
    "linear_calibration",
    "spline_calibration",
    "components_linear",
    "quantile_mapping",
    "isotonic"
  )

  k <- min(5, max(3, floor(n / 25)))
  set.seed(42)
  fold_id <- sample(rep(seq_len(k), length.out = n))

  pred_store <- lapply(models, function(m) rep(NA_real_, n))
  names(pred_store) <- models

  for (f in seq_len(k)) {
    train <- dat[fold_id != f, , drop = FALSE]
    test <- dat[fold_id == f, , drop = FALSE]

    pred_store[["raw_gemini"]][fold_id == f] <- test$x

    shift <- mean(train$x - train$y, na.rm = TRUE)
    pred_store[["bias_corrected"]][fold_id == f] <- test$x - shift

    fit_lin <- tryCatch(lm(y ~ x, data = train), error = function(e) NULL)
    if (!is.null(fit_lin)) {
      pred_store[["linear_calibration"]][fold_id == f] <- as.numeric(predict(fit_lin, newdata = test))
    }

    fit_spl <- tryCatch(lm(y ~ splines::ns(x, 4), data = train), error = function(e) NULL)
    if (!is.null(fit_spl)) {
      pred_store[["spline_calibration"]][fold_id == f] <- as.numeric(predict(fit_spl, newdata = test))
    }

    fit_comp <- tryCatch(lm(y ~ relevance + coherence + budget_realism + scale, data = train), error = function(e) NULL)
    if (!is.null(fit_comp)) {
      pred_store[["components_linear"]][fold_id == f] <- as.numeric(predict(fit_comp, newdata = test))
    }

    pred_store[["quantile_mapping"]][fold_id == f] <- fit_predict_quantile_map(train$x, train$y, test$x)
    pred_store[["isotonic"]][fold_id == f] <- fit_predict_isotonic(train$x, train$y, test$x)
  }

  ymin <- min(dat$y, na.rm = TRUE)
  ymax <- max(dat$y, na.rm = TRUE)
  pred_store <- lapply(pred_store, function(v) pmin(pmax(v, ymin), ymax))

  pred_df <- tibble::tibble(
    href_key = dat$href_key,
    government_rating = dat$y,
    gemini_quality = dat$x,
    fold = fold_id
  )
  for (m in models) pred_df[[paste0("pred_", m)]] <- pred_store[[m]]

  readr::write_csv(pred_df, out_preds)

  summary_tbl <- dplyr::bind_rows(lapply(models, function(m) {
    met <- reg_metrics(dat$y, pred_store[[m]])
    met$model <- m
    met
  })) %>%
    select(model, everything()) %>%
    arrange(rmse)

  readr::write_csv(summary_tbl, out_summary)

  top_models <- head(summary_tbl$model, 4)
  plot_df <- pred_df %>%
    select(government_rating, starts_with("pred_")) %>%
    pivot_longer(cols = starts_with("pred_"), names_to = "model", values_to = "pred") %>%
    mutate(model = stringr::str_remove(model, "^pred_")) %>%
    filter(model %in% top_models)

  p_compare <- ggplot(plot_df, aes(x = pred, y = government_rating)) +
    geom_point(alpha = 0.15, size = 1.2, color = "#1f77b4") +
    geom_smooth(method = "lm", se = FALSE, color = "#d62728") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray45") +
    facet_wrap(~model) +
    labs(
      title = "Gemini 2.5 calibration candidates (5-fold CV)",
      x = "Predicted government rating",
      y = "Observed government rating"
    ) +
    theme_minimal(base_size = 11)

  best_model <- summary_tbl$model[[1]]
  best_col <- paste0("pred_", best_model)

  calib_df <- pred_df %>%
    mutate(pred_best = .data[[best_col]], bin = ntile(pred_best, 20)) %>%
    group_by(bin) %>%
    summarise(
      n = n(),
      pred_mean = mean(pred_best, na.rm = TRUE),
      gov_mean = mean(government_rating, na.rm = TRUE),
      .groups = "drop"
    )

  p_calib <- ggplot(calib_df, aes(x = pred_mean, y = gov_mean)) +
    geom_point(size = 2.2, color = "#1f77b4") +
    geom_line(color = "#1f77b4") +
    geom_smooth(method = "lm", se = FALSE, color = "#d62728") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray45") +
    labs(
      title = paste0("Calibration bins for best model: ", best_model),
      subtitle = "20 bins over CV predictions",
      x = "Mean predicted government rating",
      y = "Mean observed government rating"
    ) +
    theme_minimal(base_size = 12)

  save_plot(p_compare, plot_compare, width = 11, height = 8)
  save_plot(p_calib, plot_calib, width = 8.5, height = 6)

  raw_row <- summary_tbl %>% filter(model == "raw_gemini")
  best_row <- summary_tbl[1, ]

  lines <- c(
    "WAYS TO FIT GEMINI 2.5 TO GOVERNMENT SCORES BETTER",
    "===============================================",
    "",
    sprintf("Rows used: %s", n),
    sprintf("CV folds: %s", k),
    "",
    "5-fold CV ranking (sorted by RMSE):",
    paste0(
      "- ", summary_tbl$model,
      " | MAE=", fmt(summary_tbl$mae),
      " | RMSE=", fmt(summary_tbl$rmse),
      " | R2=", fmt(summary_tbl$r2),
      " | Pearson=", fmt(summary_tbl$pearson),
      " | Spearman=", fmt(summary_tbl$spearman)
    ),
    "",
    sprintf("Best method: %s", best_model),
    sprintf("- Best RMSE: %s vs raw RMSE: %s", fmt(best_row$rmse), fmt(raw_row$rmse)),
    sprintf("- Best MAE: %s vs raw MAE: %s", fmt(best_row$mae), fmt(raw_row$mae)),
    sprintf("- Best Spearman: %s vs raw Spearman: %s", fmt(best_row$spearman), fmt(raw_row$spearman)),
    "",
    "Saved outputs:",
    sprintf("- summary: %s", out_summary),
    sprintf("- predictions: %s", out_preds),
    sprintf("- report: %s", out_report),
    sprintf("- compare plot: %s", plot_compare),
    sprintf("- calibration plot: %s", plot_calib)
  )

  write_report(lines, out_report)
  invisible(list(summary = out_summary, report = out_report))
}

run_gemini25_scriptclean_log_validation <- function() {
  bundle <- prepare_validation_data()
  dat <- bundle$gemini_vs_gov %>%
    mutate(
      y = application_rating,
      region = as.factor(region),
      direction = as.factor(direction)
    )

  out_summary <- file.path(validation_results_dir, paste0("gemini25_scriptclean_log_cv_summary_", validation_run_tag, ".csv"))
  out_report <- file.path(validation_results_dir, paste0("gemini25_scriptclean_log_report_", validation_run_tag, ".txt"))

  formulas <- list(
    raw_composite = y ~ gemini_quality,
    dims_only = y ~ relevance + coherence + budget_realism + scale,
    scriptclean_raw = y ~ relevance + coherence + budget_realism + scale +
      sum_requested + cofinancing + cofinancing_share + competition_index,
    scriptclean_log = y ~ relevance + coherence + budget_realism + scale +
      log_sum_requested + log_cofinancing + log_cofinancing_share + log_competition_index +
      region + direction
  )

  res <- lapply(names(formulas), function(name) {
    cv <- cv_predict_lm(dat, formulas[[name]], y_col = "y", k = 5, seed = 42)
    met <- reg_metrics(cv$obs, cv$pred)
    met$model <- name
    met$cv_n <- cv$n
    met$cv_k <- cv$k
    met
  })

  summary_tbl <- bind_rows(res) %>%
    select(model, cv_n, cv_k, everything()) %>%
    arrange(rmse)

  readr::write_csv(summary_tbl, out_summary)

  raw_row <- summary_tbl %>% filter(model == "raw_composite")
  best_row <- summary_tbl[1, ]

  lines <- c(
    "GEMINI 2.5 SCRIPTCLEAN-STYLE VALIDATION",
    "====================================",
    "",
    sprintf("Rows in joined analysis: %s", nrow(dat)),
    "",
    "Model comparison (5-fold CV):",
    paste0(
      "- ", summary_tbl$model,
      " | N=", summary_tbl$n,
      " | MAE=", fmt(summary_tbl$mae),
      " | RMSE=", fmt(summary_tbl$rmse),
      " | R2=", fmt(summary_tbl$r2),
      " | Pearson=", fmt(summary_tbl$pearson),
      " | Spearman=", fmt(summary_tbl$spearman)
    ),
    "",
    sprintf("Best model: %s", best_row$model),
    sprintf("- Best RMSE=%s vs raw composite RMSE=%s", fmt(best_row$rmse), fmt(raw_row$rmse)),
    sprintf("- Best MAE=%s vs raw composite MAE=%s", fmt(best_row$mae), fmt(raw_row$mae)),
    "",
    "Saved outputs:",
    sprintf("- summary: %s", out_summary),
    sprintf("- report: %s", out_report)
  )

  write_report(lines, out_report)
  invisible(list(summary = out_summary, report = out_report))
}

run_gemini25_xgb_fit_to_gov_rmse <- function() {
  out_preds <- file.path(validation_results_dir, paste0("gemini25_xgb_fit_oof_predictions_rmse_", validation_run_tag, ".csv"))
  out_report <- file.path(validation_results_dir, paste0("gemini25_xgb_fit_report_rmse_", validation_run_tag, ".txt"))
  out_importance <- file.path(validation_results_dir, paste0("gemini25_xgb_feature_importance_rmse_", validation_run_tag, ".csv"))

  plot_scatter <- file.path(validation_graph_dir, paste0("gemini25_xgb_scatter_oof_rmse_", validation_run_tag, ".png"))
  plot_calib <- file.path(validation_graph_dir, paste0("gemini25_xgb_calibration_oof_rmse_", validation_run_tag, ".png"))
  plot_resid <- file.path(validation_graph_dir, paste0("gemini25_xgb_residuals_oof_rmse_", validation_run_tag, ".png"))
  plot_err_by_dec <- file.path(validation_graph_dir, paste0("gemini25_xgb_error_by_gov_decile_rmse_", validation_run_tag, ".png"))

  if (!requireNamespace("xgboost", quietly = TRUE)) {
    write_report(
      c(
        "GEMINI 2.5 XGB FIT TO GOVERNMENT RMSE",
        "===================================",
        "",
        "Skipped because package xgboost is not installed in this R environment."
      ),
      out_report
    )
    return(invisible(NULL))
  }

  bundle <- prepare_validation_data()

  dat <- bundle$gemini_vs_gov %>%
    mutate(
      y = application_rating,
      region = as.factor(region),
      direction = as.factor(direction)
    )

  feature_formula <- ~ relevance + coherence + budget_realism + scale + gemini_quality +
    log_sum_requested + log_cofinancing + log_cofinancing_share + log_competition_index +
    region + direction

  feature_vars <- c(
    "y",
    "relevance", "coherence", "budget_realism", "scale", "gemini_quality",
    "log_sum_requested", "log_cofinancing", "log_cofinancing_share", "log_competition_index",
    "region", "direction"
  )

  dat2 <- dat[stats::complete.cases(dat[, feature_vars, drop = FALSE]), ]

  if (nrow(dat2) < 200) {
    write_report(
      c(
        "GEMINI 2.5 XGB FIT TO GOVERNMENT RMSE",
        "===================================",
        "",
        sprintf("Insufficient rows after complete-case filter: N=%s", nrow(dat2))
      ),
      out_report
    )
    return(invisible(NULL))
  }

  set.seed(42)
  k <- 5
  fold <- sample(rep(seq_len(k), length.out = nrow(dat2)))

  oof_pred_xgb <- rep(NA_real_, nrow(dat2))
  oof_pred_lm <- rep(NA_real_, nrow(dat2))
  best_iter <- rep(NA_integer_, k)

  params <- list(
    objective = "reg:squarederror",
    eval_metric = "rmse",
    eta = 0.05,
    max_depth = 6,
    subsample = 0.8,
    colsample_bytree = 0.8,
    min_child_weight = 5
  )

  for (f in seq_len(k)) {
    tr <- dat2[fold != f, , drop = FALSE]
    te <- dat2[fold == f, , drop = FALSE]

    fit_lm <- tryCatch(
      lm(y ~ relevance + coherence + budget_realism + scale + gemini_quality +
           log_sum_requested + log_cofinancing + log_cofinancing_share + log_competition_index +
           region + direction,
         data = tr),
      error = function(e) NULL
    )

    if (!is.null(fit_lm)) {
      te_lm <- te
      if (!is.null(fit_lm$xlevels) && length(fit_lm$xlevels) > 0) {
        for (nm in names(fit_lm$xlevels)) {
          if (nm %in% names(te_lm)) {
            te_lm[[nm]] <- factor(as.character(te_lm[[nm]]), levels = fit_lm$xlevels[[nm]])
          }
        }
      }
      oof_pred_lm[fold == f] <- suppressWarnings(as.numeric(predict(fit_lm, newdata = te_lm)))
    }

    mm_tr <- model.matrix(feature_formula, data = tr)[, -1, drop = FALSE]
    mm_te <- model.matrix(feature_formula, data = te)[, -1, drop = FALSE]
    mm_te <- align_model_matrices(mm_tr, mm_te)

    dtrain <- xgboost::xgb.DMatrix(mm_tr, label = tr$y)
    nfold_inner <- if (nrow(mm_tr) > 1500) 5 else 3

    cv_fit <- xgboost::xgb.cv(
      params = params,
      data = dtrain,
      nrounds = 1500,
      nfold = nfold_inner,
      early_stopping_rounds = 40,
      verbose = 0
    )

    best_n <- cv_fit$best_iteration
    if (!is.finite(best_n) || is.na(best_n) || best_n < 1) best_n <- 100

    fit_xgb <- xgboost::xgb.train(
      params = params,
      data = dtrain,
      nrounds = best_n,
      verbose = 0
    )

    oof_pred_xgb[fold == f] <- as.numeric(predict(fit_xgb, xgboost::xgb.DMatrix(mm_te)))
    best_iter[f] <- as.integer(best_n)
  }

  ymin <- min(dat2$y, na.rm = TRUE)
  ymax <- max(dat2$y, na.rm = TRUE)
  oof_pred_xgb <- pmin(pmax(oof_pred_xgb, ymin), ymax)
  oof_pred_lm <- pmin(pmax(oof_pred_lm, ymin), ymax)

  pred_tbl <- tibble::tibble(
    href_key = dat2$href_key,
    y = dat2$y,
    pred_xgb = oof_pred_xgb,
    pred_lm = oof_pred_lm,
    fold = fold
  )

  readr::write_csv(pred_tbl, out_preds)

  met_xgb <- reg_metrics(dat2$y, oof_pred_xgb)
  met_lm <- reg_metrics(dat2$y, oof_pred_lm)

  mm_all <- model.matrix(feature_formula, data = dat2)[, -1, drop = FALSE]
  dall <- xgboost::xgb.DMatrix(mm_all, label = dat2$y)
  nround_full <- max(50, round(mean(best_iter, na.rm = TRUE)))

  fit_full <- xgboost::xgb.train(
    params = params,
    data = dall,
    nrounds = nround_full,
    verbose = 0
  )

  imp <- xgboost::xgb.importance(model = fit_full)
  readr::write_csv(imp, out_importance)

  p_scatter <- ggplot(pred_tbl, aes(x = pred_xgb, y = y)) +
    geom_point(alpha = 0.18, color = "#1f77b4", size = 1.2) +
    geom_smooth(method = "lm", se = FALSE, color = "#d62728") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray45") +
    labs(
      title = "Gemini 2.5 XGB OOF: predicted vs observed government score",
      subtitle = paste0("N=", met_xgb$n, " | RMSE=", fmt(met_xgb$rmse), " | MAE=", fmt(met_xgb$mae)),
      x = "XGB predicted government rating",
      y = "Observed government rating"
    ) +
    theme_minimal(base_size = 12)

  calib <- pred_tbl %>%
    mutate(bin = ntile(pred_xgb, 20)) %>%
    group_by(bin) %>%
    summarise(
      n = n(),
      pred_mean = mean(pred_xgb, na.rm = TRUE),
      y_mean = mean(y, na.rm = TRUE),
      .groups = "drop"
    )

  p_calib <- ggplot(calib, aes(x = pred_mean, y = y_mean)) +
    geom_point(size = 2.1, color = "#1f77b4") +
    geom_line(color = "#1f77b4") +
    geom_smooth(method = "lm", se = FALSE, color = "#d62728") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray45") +
    labs(
      title = "Gemini 2.5 XGB OOF calibration",
      x = "Mean predicted government score",
      y = "Mean observed government score"
    ) +
    theme_minimal(base_size = 12)

  resid_tbl <- pred_tbl %>% mutate(resid_xgb = y - pred_xgb)
  p_resid <- ggplot(resid_tbl, aes(x = resid_xgb)) +
    geom_histogram(bins = 50, fill = "#1f77b4", color = "white") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "#d62728") +
    labs(
      title = "Gemini 2.5 XGB OOF residuals",
      x = "Observed - predicted",
      y = "Count"
    ) +
    theme_minimal(base_size = 12)

  err_by_dec <- pred_tbl %>%
    mutate(gov_decile = ntile(y, 10)) %>%
    group_by(gov_decile) %>%
    summarise(
      n = n(),
      gov_mean = mean(y, na.rm = TRUE),
      rmse = sqrt(mean((y - pred_xgb)^2, na.rm = TRUE)),
      .groups = "drop"
    )

  p_dec <- ggplot(err_by_dec, aes(x = gov_mean, y = rmse)) +
    geom_point(size = 2.0, color = "#1f77b4") +
    geom_line(color = "#1f77b4") +
    labs(
      title = "Gemini 2.5 XGB error by government-score decile",
      x = "Mean observed government score (decile)",
      y = "RMSE within decile"
    ) +
    theme_minimal(base_size = 12)

  save_plot(p_scatter, plot_scatter, width = 8.5, height = 6)
  save_plot(p_calib, plot_calib, width = 8.5, height = 6)
  save_plot(p_resid, plot_resid, width = 8.5, height = 6)
  save_plot(p_dec, plot_err_by_dec, width = 8.5, height = 6)

  top_imp <- head(imp$Feature, 10)

  lines <- c(
    "GEMINI 2.5 XGB FIT TO GOVERNMENT RMSE",
    "===================================",
    "",
    sprintf("Rows after complete-case filtering: %s", nrow(dat2)),
    sprintf("Outer CV folds: %s", k),
    sprintf("Mean best nrounds: %s", fmt(mean(best_iter, na.rm = TRUE), 2)),
    "",
    "OOF performance:",
    sprintf(
      "- XGB | N=%s | MAE=%s | RMSE=%s | R2=%s | Pearson=%s | Spearman=%s | Bias=%s",
      met_xgb$n,
      fmt(met_xgb$mae),
      fmt(met_xgb$rmse),
      fmt(met_xgb$r2),
      fmt(met_xgb$pearson),
      fmt(met_xgb$spearman),
      fmt(met_xgb$bias)
    ),
    sprintf(
      "- Linear baseline | N=%s | MAE=%s | RMSE=%s | R2=%s | Pearson=%s | Spearman=%s | Bias=%s",
      met_lm$n,
      fmt(met_lm$mae),
      fmt(met_lm$rmse),
      fmt(met_lm$r2),
      fmt(met_lm$pearson),
      fmt(met_lm$spearman),
      fmt(met_lm$bias)
    ),
    "",
    "Top full-model features:",
    paste0("- ", top_imp),
    "",
    "Saved outputs:",
    sprintf("- predictions: %s", out_preds),
    sprintf("- report: %s", out_report),
    sprintf("- importance: %s", out_importance),
    sprintf("- scatter: %s", plot_scatter),
    sprintf("- calibration: %s", plot_calib),
    sprintf("- residuals: %s", plot_resid),
    sprintf("- error by decile: %s", plot_err_by_dec)
  )

  write_report(lines, out_report)
  invisible(list(report = out_report, preds = out_preds, importance = out_importance))
}

run_gemini25_vs_government_on_human_subset <- function() {
  bundle <- prepare_validation_data()
  merged <- bundle$manual_valid %>%
    inner_join(bundle$gemini, by = "href_key") %>%
    inner_join(bundle$gov %>% select(href_key, application_rating), by = "href_key") %>%
    mutate(
      abs_err = abs(gemini_quality - application_rating),
      diff = gemini_quality - application_rating,
      abs_err_manual = abs(manual_quality - application_rating),
      diff_manual = manual_quality - application_rating
    )

  out_merged <- file.path(validation_results_dir, paste0("gemini25_vs_gov_quality_human_subset_manual_trad_age_quality_labels_gena_", validation_run_tag, ".csv"))
  out_report <- file.path(validation_results_dir, paste0("gemini25_vs_gov_quality_human_subset_report_manual_trad_age_quality_labels_gena_", validation_run_tag, ".txt"))
  plot_scatter <- file.path(validation_graph_dir, paste0("gemini25_vs_gov_quality_human_subset_scatter_", validation_run_tag, ".png"))
  plot_binned <- file.path(validation_graph_dir, paste0("gemini25_vs_gov_quality_human_subset_binned_", validation_run_tag, ".png"))

  readr::write_csv(merged, out_merged)

  met_gem <- reg_metrics(merged$application_rating, merged$gemini_quality)
  met_manual <- reg_metrics(merged$application_rating, merged$manual_quality)

  lm_fit <- lm(application_rating ~ gemini_quality, data = merged)
  lm_coef <- broom::tidy(lm_fit)

  p_scatter <- ggplot(merged, aes(x = gemini_quality, y = application_rating)) +
    geom_point(alpha = 0.85, color = "#1f77b4", size = 2.4) +
    geom_smooth(method = "lm", se = TRUE, color = "#d62728", linewidth = 1) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray45") +
    labs(
      title = "Gemini 2.5 vs government quality on manual subset",
      subtitle = paste0("Matched N = ", nrow(merged)),
      x = "Gemini 2.5 composite quality",
      y = "Government application_rating"
    ) +
    theme_minimal(base_size = 12)

  n_bins <- min(6, max(3, nrow(merged)))
  binned <- merged %>%
    mutate(bin = dplyr::ntile(gemini_quality, n_bins)) %>%
    group_by(bin) %>%
    summarise(
      n = n(),
      gemini_mean = mean(gemini_quality, na.rm = TRUE),
      gov_mean = mean(application_rating, na.rm = TRUE),
      .groups = "drop"
    )

  p_binned <- ggplot(binned, aes(x = gemini_mean, y = gov_mean)) +
    geom_point(size = 2.8, color = "#1f77b4") +
    geom_line(color = "#1f77b4") +
    geom_smooth(method = "lm", se = TRUE, color = "#d62728") +
    labs(
      title = "Binned means: Gemini 2.5 vs government on manual subset",
      x = "Mean Gemini 2.5 quality",
      y = "Mean government application_rating"
    ) +
    theme_minimal(base_size = 12)

  save_plot(p_scatter, plot_scatter, width = 8, height = 6)
  save_plot(p_binned, plot_binned, width = 8, height = 6)

  lines <- c(
    "GEMINI 2.5 VS GOVERNMENT QUALITY ON MANUAL SUBSET",
    "===============================================",
    "",
    sprintf("Manual rows (valid): %s", nrow(bundle$manual_valid)),
    sprintf("Matched rows (manual + Gemini + government): %s", nrow(merged)),
    "",
    "Gemini vs government metrics:",
    sprintf("- MAE: %s | MedAE: %s | RMSE: %s", fmt(met_gem$mae), fmt(met_gem$medae), fmt(met_gem$rmse)),
    sprintf("- Pearson: %s | Spearman: %s | Kendall: %s", fmt(met_gem$pearson), fmt(met_gem$spearman), fmt(met_gem$kendall)),
    sprintf("- Mean bias (Gemini - Gov): %s", fmt(met_gem$bias)),
    sprintf(
      "- Calibration LM: gov = %s + %s * gemini | R2 = %s",
      fmt(lm_coef$estimate[lm_coef$term == "(Intercept)"]),
      fmt(lm_coef$estimate[lm_coef$term == "gemini_quality"]),
      fmt(summary(lm_fit)$r.squared)
    ),
    "",
    "Manual vs government metrics (same subset):",
    sprintf("- MAE: %s | RMSE: %s", fmt(met_manual$mae), fmt(met_manual$rmse)),
    sprintf("- Pearson: %s | Spearman: %s", fmt(met_manual$pearson), fmt(met_manual$spearman)),
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

run_gemini25_vs_manual_quality_validation <- function() {
  bundle <- prepare_validation_data()
  merged <- bundle$gemini_vs_manual %>%
    mutate(
      quality_diff = gemini_quality - manual_quality,
      quality_abs_err = abs(quality_diff),
      manual_win60 = if_else(manual_quality >= 60, 1, 0),
      gemini_win60 = if_else(gemini_quality >= 60, 1, 0)
    )

  out_merged <- file.path(validation_results_dir, paste0("gemini25_vs_human_quality_merged_", validation_run_tag, ".csv"))
  out_report <- file.path(validation_results_dir, paste0("gemini25_vs_human_quality_report_", validation_run_tag, ".txt"))
  out_outliers <- file.path(validation_results_dir, paste0("gemini25_vs_human_quality_outliers_", validation_run_tag, ".csv"))
  plot_scatter <- file.path(validation_graph_dir, paste0("gemini25_vs_human_quality_scatter_", validation_run_tag, ".png"))
  plot_components <- file.path(validation_graph_dir, paste0("gemini25_vs_human_components_scatter_", validation_run_tag, ".png"))
  plot_density <- file.path(validation_graph_dir, paste0("gemini25_vs_human_threshold_density_", validation_run_tag, ".png"))
  plot_roc <- file.path(validation_graph_dir, paste0("gemini25_vs_human_threshold_roc_", validation_run_tag, ".png"))
  plot_bland <- file.path(validation_graph_dir, paste0("gemini25_vs_human_bland_altman_", validation_run_tag, ".png"))

  readr::write_csv(merged, out_merged)

  met <- reg_metrics(merged$manual_quality, merged$gemini_quality)
  lm_fit <- lm(manual_quality ~ gemini_quality, data = merged)
  lm_coef <- broom::tidy(lm_fit)

  # Scale-invariant / bias-adjusted diagnostics
  mu_h <- mean(merged$manual_quality, na.rm = TRUE)
  mu_g <- mean(merged$gemini_quality, na.rm = TRUE)
  sd_h <- stats::sd(merged$manual_quality, na.rm = TRUE)
  sd_g <- stats::sd(merged$gemini_quality, na.rm = TRUE)

  bias <- mean(merged$gemini_quality - merged$manual_quality, na.rm = TRUE)
  pred_bias_adj <- merged$gemini_quality - bias
  met_bias_adj <- reg_metrics(merged$manual_quality, pred_bias_adj)

  scale_ratio <- if (is.finite(sd_h) && is.finite(sd_g) && sd_g > 0) sd_h / sd_g else NA_real_
  pred_loc_scale <- mu_h + scale_ratio * (merged$gemini_quality - mu_g)
  met_loc_scale <- reg_metrics(merged$manual_quality, pred_loc_scale)

  z_h <- if (is.finite(sd_h) && sd_h > 0) (merged$manual_quality - mu_h) / sd_h else NA_real_
  z_g <- if (is.finite(sd_g) && sd_g > 0) (merged$gemini_quality - mu_g) / sd_g else NA_real_
  met_z <- reg_metrics(z_h, z_g)

  loocv_pred <- function(df) {
    n <- nrow(df)
    pred <- rep(NA_real_, n)
    if (n < 5) return(pred)

    for (i in seq_len(n)) {
      tr <- df[-i, , drop = FALSE]
      te <- df[i, , drop = FALSE]
      fit <- tryCatch(lm(manual_quality ~ gemini_quality, data = tr), error = function(e) NULL)
      if (!is.null(fit)) {
        pred[i] <- suppressWarnings(as.numeric(stats::predict(fit, newdata = te)))
      }
    }
    pred
  }

  pred_fit <- suppressWarnings(as.numeric(stats::predict(lm_fit, newdata = merged)))
  met_fit <- reg_metrics(merged$manual_quality, pred_fit)

  pred_loocv <- loocv_pred(merged)
  met_loocv <- reg_metrics(merged$manual_quality, pred_loocv)

  resid <- suppressWarnings(as.numeric(stats::residuals(lm_fit)))
  out_rstudent <- suppressWarnings(as.numeric(stats::rstudent(lm_fit)))
  out_hat <- suppressWarnings(as.numeric(stats::hatvalues(lm_fit)))
  out_cook <- suppressWarnings(as.numeric(stats::cooks.distance(lm_fit)))

  outlier_flag <- is.finite(out_rstudent) & abs(out_rstudent) > 2
  outliers_tbl <- merged %>%
    mutate(
      resid = resid,
      abs_resid = abs(resid),
      rstudent = out_rstudent,
      hat = out_hat,
      cook = out_cook,
      outlier_flag = outlier_flag
    ) %>%
    filter(outlier_flag) %>%
    arrange(desc(abs_resid)) %>%
    select(href_key, href.x, href.y, gemini_quality, manual_quality, resid, abs_resid, rstudent, hat, cook)

  readr::write_csv(outliers_tbl, out_outliers)

  trimmed <- merged %>% filter(!outlier_flag)
  met_trim_fit <- tibble::tibble(n = 0)
  met_trim_loocv <- tibble::tibble(n = 0)
  trim_r2 <- NA_real_

  if (nrow(trimmed) >= 10) {
    lm_trim <- lm(manual_quality ~ gemini_quality, data = trimmed)
    trim_r2 <- summary(lm_trim)$r.squared

    pred_trim_fit <- suppressWarnings(as.numeric(stats::predict(lm_trim, newdata = trimmed)))
    met_trim_fit <- reg_metrics(trimmed$manual_quality, pred_trim_fit)

    pred_trim_loocv <- loocv_pred(trimmed)
    met_trim_loocv <- reg_metrics(trimmed$manual_quality, pred_trim_loocv)
  }

  component_tbl <- tibble::tibble(
    component = c("relevance", "coherence", "budget_realism", "scale"),
    gemini_col = c("relevance", "coherence", "budget_realism", "scale"),
    manual_col = c("manual_relevance", "manual_coherence", "manual_budget_realism", "manual_scale")
  ) %>%
    rowwise() %>%
    mutate(
      n_component = sum(!is.na(merged[[gemini_col]]) & !is.na(merged[[manual_col]])),
      mae_component = mean(abs(merged[[gemini_col]] - merged[[manual_col]]), na.rm = TRUE),
      pearson_component = safe_cor(merged[[gemini_col]], merged[[manual_col]], "pearson"),
      spearman_component = safe_cor(merged[[gemini_col]], merged[[manual_col]], "spearman")
    ) %>%
    ungroup()

  auc_main <- safe_auc(merged$manual_win60, merged$gemini_quality)
  auc_rel <- safe_auc(merged$manual_win60, merged$relevance)
  auc_coh <- safe_auc(merged$manual_win60, merged$coherence)
  auc_bud <- safe_auc(merged$manual_win60, merged$budget_realism)
  auc_sca <- safe_auc(merged$manual_win60, merged$scale)

  roc_main <- NULL
  best_thr <- NA_real_
  if (length(unique(merged$manual_win60)) == 2) {
    roc_main <- pROC::roc(merged$manual_win60, merged$gemini_quality, quiet = TRUE)
    best_thr <- as.numeric(pROC::coords(roc_main, x = "best", best.method = "youden", ret = "threshold"))
  }

  pred_fix60 <- if_else(merged$gemini_quality >= 60, 1, 0)
  pred_best <- if (is.na(best_thr)) pred_fix60 else if_else(merged$gemini_quality >= best_thr, 1, 0)

  cm_fix60 <- conf_metrics(merged$manual_win60, pred_fix60)
  cm_best <- conf_metrics(merged$manual_win60, pred_best)

  near <- merged %>% filter(manual_quality >= 55, manual_quality <= 65)
  near_auc <- safe_auc(near$manual_win60, near$gemini_quality)
  near_cm60 <- conf_metrics(near$manual_win60, if_else(near$gemini_quality >= 60, 1, 0))

  p_scatter <- ggplot(merged, aes(x = gemini_quality, y = manual_quality)) +
    geom_point(size = 2.4, alpha = 0.8, color = "#1f77b4") +
    geom_smooth(method = "lm", se = TRUE, color = "#d62728", linewidth = 1) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray45") +
    geom_vline(xintercept = 60, linetype = "dotted", color = "gray55") +
    geom_hline(yintercept = 60, linetype = "dotted", color = "gray55") +
    labs(
      title = "Gemini 2.5 vs manual quality",
      subtitle = paste0("N = ", nrow(merged), " | manual threshold = 60"),
      x = "Gemini 2.5 quality",
      y = "Manual quality"
    ) +
    theme_minimal(base_size = 12)

  comp_long <- merged %>%
    select(
      relevance,
      coherence,
      budget_realism,
      scale,
      manual_relevance,
      manual_coherence,
      manual_budget_realism,
      manual_scale
    ) %>%
    mutate(row_id = row_number()) %>%
    tidyr::pivot_longer(
      cols = -row_id,
      names_to = c("source", "component"),
      names_pattern = "(manual|)(?:_)?(relevance|coherence|budget_realism|scale)",
      values_to = "score"
    ) %>%
    mutate(source = if_else(source == "", "gemini", source)) %>%
    tidyr::pivot_wider(names_from = source, values_from = score) %>%
    filter(!is.na(gemini), !is.na(manual))

  p_comp <- ggplot(comp_long, aes(x = gemini, y = manual)) +
    geom_point(alpha = 0.8, color = "#1f77b4", size = 2) +
    geom_smooth(method = "lm", se = FALSE, color = "#d62728") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray45") +
    facet_wrap(~component, scales = "free") +
    labs(
      title = "Gemini 2.5 vs manual component scores",
      x = "Gemini component score",
      y = "Manual component score"
    ) +
    theme_minimal(base_size = 11)

  p_density <- merged %>%
    mutate(manual_win_lbl = ifelse(manual_win60 == 1, "Manual winner (>=60)", "Manual non-winner (<60)")) %>%
    ggplot(aes(x = gemini_quality, fill = manual_win_lbl)) +
    geom_density(alpha = 0.35) +
    geom_vline(xintercept = 60, linetype = "dashed", color = "gray45") +
    labs(
      title = "Gemini 2.5 quality by manual threshold status",
      x = "Gemini 2.5 quality",
      y = "Density",
      fill = "Manual status"
    ) +
    theme_minimal(base_size = 12)

  p_bland <- merged %>%
    mutate(mean_score = (gemini_quality + manual_quality) / 2) %>%
    ggplot(aes(x = mean_score, y = quality_diff)) +
    geom_point(alpha = 0.8, color = "#1f77b4", size = 2.2) +
    geom_hline(yintercept = mean(merged$quality_diff, na.rm = TRUE), color = "#d62728") +
    geom_hline(
      yintercept = mean(merged$quality_diff, na.rm = TRUE) + 1.96 * sd(merged$quality_diff, na.rm = TRUE),
      linetype = "dashed",
      color = "gray45"
    ) +
    geom_hline(
      yintercept = mean(merged$quality_diff, na.rm = TRUE) - 1.96 * sd(merged$quality_diff, na.rm = TRUE),
      linetype = "dashed",
      color = "gray45"
    ) +
    labs(
      title = "Bland-Altman: Gemini 2.5 - manual quality",
      x = "Mean of Gemini and manual quality",
      y = "Difference (Gemini - manual)"
    ) +
    theme_minimal(base_size = 12)

  save_plot(p_scatter, plot_scatter, width = 8, height = 6)
  save_plot(p_comp, plot_components, width = 10.5, height = 7)
  save_plot(p_density, plot_density, width = 8, height = 6)
  save_plot(p_bland, plot_bland, width = 8, height = 6)

  if (!is.null(roc_main)) {
    roc_df <- bind_rows(
      data.frame(model = "gemini_composite", tpr = roc_main$sensitivities, fpr = 1 - roc_main$specificities),
      data.frame(model = "relevance", tpr = pROC::roc(merged$manual_win60, merged$relevance, quiet = TRUE)$sensitivities, fpr = 1 - pROC::roc(merged$manual_win60, merged$relevance, quiet = TRUE)$specificities),
      data.frame(model = "coherence", tpr = pROC::roc(merged$manual_win60, merged$coherence, quiet = TRUE)$sensitivities, fpr = 1 - pROC::roc(merged$manual_win60, merged$coherence, quiet = TRUE)$specificities),
      data.frame(model = "budget_realism", tpr = pROC::roc(merged$manual_win60, merged$budget_realism, quiet = TRUE)$sensitivities, fpr = 1 - pROC::roc(merged$manual_win60, merged$budget_realism, quiet = TRUE)$specificities),
      data.frame(model = "scale", tpr = pROC::roc(merged$manual_win60, merged$scale, quiet = TRUE)$sensitivities, fpr = 1 - pROC::roc(merged$manual_win60, merged$scale, quiet = TRUE)$specificities)
    )

    p_roc <- ggplot(roc_df, aes(x = fpr, y = tpr, color = model)) +
      geom_line(linewidth = 1) +
      geom_abline(linetype = "dashed", alpha = 0.6) +
      coord_equal() +
      labs(
        title = "ROC curves against manual threshold",
        subtitle = paste0("AUC composite=", fmt(auc_main)),
        x = "False positive rate",
        y = "True positive rate",
        color = "Score"
      ) +
      theme_minimal(base_size = 12)

    save_plot(p_roc, plot_roc, width = 8, height = 6.5)
  }

  lines <- c(
    "GEMINI 2.5 VS MANUAL QUALITY VALIDATION",
    "====================================",
    "",
    sprintf("Manual rows (valid and deduped): %s", nrow(bundle$manual_valid)),
    sprintf("Merged rows with Gemini: %s", nrow(merged)),
    "",
    "Agreement metrics (relationship style; scale mismatch allowed):",
    sprintf("- Pearson: %s | Spearman: %s | Kendall: %s", fmt(met$pearson), fmt(met$spearman), fmt(met$kendall)),
    sprintf("- Mean bias (Gemini - manual): %s", fmt(bias)),
    sprintf("- SD manual: %s | SD gemini: %s", fmt(sd_h, 4), fmt(sd_g, 4)),
    sprintf(
      "- Calibration LM: manual = %s + %s*gemini | R2=%s",
      fmt(lm_coef$estimate[lm_coef$term == "(Intercept)"]),
      fmt(lm_coef$estimate[lm_coef$term == "gemini_quality"]),
      fmt(summary(lm_fit)$r.squared)
    ),
    "",
    "Error checks after removing only scale artifacts:",
    sprintf("- Bias-adjusted (shift only): MAE=%s | RMSE=%s", fmt(met_bias_adj$mae), fmt(met_bias_adj$rmse)),
    sprintf("- Location+scale adjusted (match mean+SD): MAE=%s | RMSE=%s", fmt(met_loc_scale$mae), fmt(met_loc_scale$rmse)),
    sprintf("- Z-scored (both standardized): MAE=%s | RMSE=%s (errors in manual SD units)", fmt(met_z$mae), fmt(met_z$rmse)),
    "",
    "Gemini -> manual prediction error (OLS mapping):",
    sprintf("- Fitted MAE: %s | RMSE: %s", fmt(met_fit$mae), fmt(met_fit$rmse)),
    sprintf("- LOOCV MAE: %s | RMSE: %s | R2: %s", fmt(met_loocv$mae), fmt(met_loocv$rmse), fmt(met_loocv$r2)),
    "",
    "Outlier sensitivity (rule: |studentized residual| > 2 in the linear model):",
    sprintf("- Outliers flagged: %s", nrow(outliers_tbl)),
    sprintf("- Trimmed N: %s | Trimmed R2: %s", nrow(trimmed), fmt(trim_r2)),
    sprintf("- Trimmed LOOCV MAE: %s | RMSE: %s | R2: %s", fmt(met_trim_loocv$mae), fmt(met_trim_loocv$rmse), fmt(met_trim_loocv$r2)),
    sprintf("- Outlier href_keys: %s", if (nrow(outliers_tbl) > 0) paste(outliers_tbl$href_key, collapse = ", ") else "none"),
    "",
    "Component-level agreement:",
    sprintf(
      "- relevance: n=%s | MAE=%s | Pearson=%s | Spearman=%s",
      component_tbl$n_component[component_tbl$component == "relevance"],
      fmt(component_tbl$mae_component[component_tbl$component == "relevance"]),
      fmt(component_tbl$pearson_component[component_tbl$component == "relevance"]),
      fmt(component_tbl$spearman_component[component_tbl$component == "relevance"])
    ),
    sprintf(
      "- coherence: n=%s | MAE=%s | Pearson=%s | Spearman=%s",
      component_tbl$n_component[component_tbl$component == "coherence"],
      fmt(component_tbl$mae_component[component_tbl$component == "coherence"]),
      fmt(component_tbl$pearson_component[component_tbl$component == "coherence"]),
      fmt(component_tbl$spearman_component[component_tbl$component == "coherence"])
    ),
    sprintf(
      "- budget_realism: n=%s | MAE=%s | Pearson=%s | Spearman=%s",
      component_tbl$n_component[component_tbl$component == "budget_realism"],
      fmt(component_tbl$mae_component[component_tbl$component == "budget_realism"]),
      fmt(component_tbl$pearson_component[component_tbl$component == "budget_realism"]),
      fmt(component_tbl$spearman_component[component_tbl$component == "budget_realism"])
    ),
    sprintf(
      "- scale: n=%s | MAE=%s | Pearson=%s | Spearman=%s",
      component_tbl$n_component[component_tbl$component == "scale"],
      fmt(component_tbl$mae_component[component_tbl$component == "scale"]),
      fmt(component_tbl$pearson_component[component_tbl$component == "scale"]),
      fmt(component_tbl$spearman_component[component_tbl$component == "scale"])
    ),
    "",
    "Manual threshold tests (threshold=60):",
    sprintf("- AUC Gemini composite: %s", fmt(auc_main)),
    sprintf("- AUC relevance: %s | coherence: %s | budget_realism: %s | scale: %s", fmt(auc_rel), fmt(auc_coh), fmt(auc_bud), fmt(auc_sca)),
    sprintf("- Best Gemini threshold (Youden): %s", fmt(best_thr, 2)),
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
    "Near-threshold stress test (manual quality in [55,65]):",
    sprintf("- N: %s", nrow(near)),
    sprintf("- AUC: %s", fmt(near_auc)),
    sprintf(
      "- Fixed 60 accuracy=%s, precision=%s, recall=%s, specificity=%s, f1=%s",
      fmt(near_cm60[["accuracy"]]),
      fmt(near_cm60[["precision"]]),
      fmt(near_cm60[["recall"]]),
      fmt(near_cm60[["specificity"]]),
      fmt(near_cm60[["f1"]])
    ),
    "",
    "Saved outputs:",
    sprintf("- merged: %s", out_merged),
    sprintf("- outliers: %s", out_outliers),
    sprintf("- report: %s", out_report),
    sprintf("- scatter: %s", plot_scatter),
    sprintf("- components: %s", plot_components),
    sprintf("- threshold density: %s", plot_density),
    sprintf("- threshold ROC: %s", plot_roc),
    sprintf("- bland-altman: %s", plot_bland)
  )

  write_report(lines, out_report)
  invisible(list(merged = out_merged, report = out_report))
}

run_gemini25_vs_government_quality_threshold_tests <- function() {
  bundle <- prepare_validation_data()
  merged <- bundle$gemini_vs_status %>%
    mutate(gov_win = as.integer(project_status))

  out_merge <- file.path(validation_results_dir, paste0("gemini25_vs_winner_loser_merged_", validation_run_tag, ".csv"))
  out_report <- file.path(validation_results_dir, paste0("gemini25_vs_winner_loser_threshold_report_", validation_run_tag, ".txt"))
  plot_scatter <- file.path(validation_graph_dir, paste0("gemini25_vs_winner_loser_scatter_", validation_run_tag, ".png"))
  plot_density <- file.path(validation_graph_dir, paste0("gemini25_quality_density_by_winner_loser_", validation_run_tag, ".png"))
  plot_roc <- file.path(validation_graph_dir, paste0("gemini25_vs_winner_loser_roc_curves_", validation_run_tag, ".png"))
  plot_deciles <- file.path(validation_graph_dir, paste0("gemini25_win_rate_by_quality_decile_", validation_run_tag, ".png"))
  plot_calibration <- file.path(validation_graph_dir, paste0("gemini25_quality_calibration_winner_loser_", validation_run_tag, ".png"))

  readr::write_csv(merged, out_merge)

  auc_main <- safe_auc(merged$gov_win, merged$gemini_quality)
  auc_rel <- safe_auc(merged$gov_win, merged$relevance)
  auc_coh <- safe_auc(merged$gov_win, merged$coherence)
  auc_bud <- safe_auc(merged$gov_win, merged$budget_realism)
  auc_sca <- safe_auc(merged$gov_win, merged$scale)

  roc_main <- pROC::roc(merged$gov_win, merged$gemini_quality, quiet = TRUE)
  thr_best <- as.numeric(pROC::coords(roc_main, x = "best", best.method = "youden", ret = "threshold"))

  pred_fix60 <- if_else(merged$gemini_quality >= 60, 1, 0)
  pred_best <- if_else(merged$gemini_quality >= thr_best, 1, 0)

  cm_fix60 <- conf_metrics(merged$gov_win, pred_fix60)
  cm_best <- conf_metrics(merged$gov_win, pred_best)

  glm_thr <- glm(gov_win ~ gemini_quality, data = merged, family = binomial)
  glm_thr_coef <- broom::tidy(glm_thr)

  glm_dims <- glm(gov_win ~ relevance + coherence + budget_realism + scale, data = merged, family = binomial)
  pred_prob_dims <- suppressWarnings(as.numeric(stats::predict(glm_dims, type = "response")))
  auc_dims <- safe_auc(merged$gov_win, pred_prob_dims)

  p_scatter <- ggplot(merged, aes(x = gemini_quality, y = gov_win)) +
    geom_jitter(alpha = 0.15, color = "#1f77b4", size = 1.1, height = 0.08, width = 0) +
    geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE, color = "#d62728") +
    labs(
      title = "Winner probability by Gemini 2.5 quality",
      subtitle = paste0("All apps with winner/loser status | N=", nrow(merged)),
      x = "Gemini 2.5 quality",
      y = "Winner indicator"
    ) +
    theme_minimal(base_size = 12)

  p_density <- merged %>%
    mutate(win_lbl = ifelse(gov_win == 1, "Winner", "Loser")) %>%
    ggplot(aes(x = gemini_quality, fill = win_lbl)) +
    geom_density(alpha = 0.35) +
    geom_vline(xintercept = 60, linetype = "dashed", color = "gray45") +
    labs(
      title = "Gemini 2.5 quality by winner/loser status",
      x = "Gemini 2.5 quality",
      y = "Density",
      fill = "Status"
    ) +
    theme_minimal(base_size = 12)

  roc_df <- bind_rows(
    data.frame(model = "gemini_composite", tpr = roc_main$sensitivities, fpr = 1 - roc_main$specificities),
    data.frame(model = "relevance", tpr = pROC::roc(merged$gov_win, merged$relevance, quiet = TRUE)$sensitivities, fpr = 1 - pROC::roc(merged$gov_win, merged$relevance, quiet = TRUE)$specificities),
    data.frame(model = "coherence", tpr = pROC::roc(merged$gov_win, merged$coherence, quiet = TRUE)$sensitivities, fpr = 1 - pROC::roc(merged$gov_win, merged$coherence, quiet = TRUE)$specificities),
    data.frame(model = "budget_realism", tpr = pROC::roc(merged$gov_win, merged$budget_realism, quiet = TRUE)$sensitivities, fpr = 1 - pROC::roc(merged$gov_win, merged$budget_realism, quiet = TRUE)$specificities),
    data.frame(model = "scale", tpr = pROC::roc(merged$gov_win, merged$scale, quiet = TRUE)$sensitivities, fpr = 1 - pROC::roc(merged$gov_win, merged$scale, quiet = TRUE)$specificities)
  )

  p_roc <- ggplot(roc_df, aes(x = fpr, y = tpr, color = model)) +
    geom_line(linewidth = 1) +
    geom_abline(linetype = "dashed", alpha = 0.6) +
    coord_equal() +
    labs(
      title = "ROC curves for winner/loser classification",
      subtitle = paste0(
        "AUC composite=", fmt(auc_main),
        " | rel=", fmt(auc_rel),
        " | coh=", fmt(auc_coh),
        " | bud=", fmt(auc_bud),
        " | scale=", fmt(auc_sca)
      ),
      x = "False positive rate",
      y = "True positive rate",
      color = "Score"
    ) +
    theme_minimal(base_size = 12)

  dec <- merged %>%
    mutate(gemini_decile = dplyr::ntile(gemini_quality, 10)) %>%
    group_by(gemini_decile) %>%
    summarise(
      n = n(),
      gemini_mean = mean(gemini_quality, na.rm = TRUE),
      win_rate = mean(gov_win, na.rm = TRUE),
      .groups = "drop"
    )

  p_dec <- ggplot(dec, aes(x = gemini_mean, y = win_rate)) +
    geom_line(linewidth = 1, color = "#1f77b4") +
    geom_point(size = 2.1, color = "#1f77b4") +
    labs(
      title = "Winner rate by Gemini 2.5 quality decile",
      x = "Mean Gemini quality in decile",
      y = "Winner share"
    ) +
    theme_minimal(base_size = 12)

  pred_prob <- as.numeric(stats::predict(glm_thr, type = "response"))
  calib <- merged %>%
    mutate(pred_prob = pred_prob, prob_bin = ntile(pred_prob, 10)) %>%
    group_by(prob_bin) %>%
    summarise(
      n = n(),
      p_mean = mean(pred_prob, na.rm = TRUE),
      y_mean = mean(gov_win, na.rm = TRUE),
      .groups = "drop"
    )

  p_cal <- ggplot(calib, aes(x = p_mean, y = y_mean)) +
    geom_point(aes(size = n), alpha = 0.8, color = "#1f77b4") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray45") +
    labs(
      title = "Calibration for winner/loser model",
      subtitle = "Model: winner ~ Gemini 2.5 quality",
      x = "Predicted winner probability",
      y = "Observed winner rate",
      size = "N"
    ) +
    theme_minimal(base_size = 12)

  save_plot(p_scatter, plot_scatter, width = 8, height = 6)
  save_plot(p_density, plot_density, width = 8, height = 6)
  save_plot(p_roc, plot_roc, width = 8, height = 6.5)
  save_plot(p_dec, plot_deciles, width = 8, height = 6)
  save_plot(p_cal, plot_calibration, width = 8, height = 6)

  lines <- c(
    "GEMINI 2.5 VS WINNER/LOSER STATUS",
    "===============================",
    "",
    sprintf("Rows with winner/loser status: %s", nrow(merged)),
    sprintf("Winners: %s | Losers: %s", sum(merged$gov_win == 1, na.rm = TRUE), sum(merged$gov_win == 0, na.rm = TRUE)),
    "",
    "AUC tests:",
    sprintf("- Gemini composite: %s", fmt(auc_main)),
    sprintf("- relevance: %s | coherence: %s | budget_realism: %s | scale: %s", fmt(auc_rel), fmt(auc_coh), fmt(auc_bud), fmt(auc_sca)),
    sprintf("- Logit (4 components) AUC: %s", fmt(auc_dims)),
    "",
    "Threshold diagnostics:",
    sprintf("- Best Gemini threshold (Youden): %s", fmt(thr_best, 2)),
    sprintf(
      "- Fixed threshold 60 -> accuracy=%s, precision=%s, recall=%s, specificity=%s, f1=%s",
      fmt(cm_fix60[["accuracy"]]), fmt(cm_fix60[["precision"]]), fmt(cm_fix60[["recall"]]), fmt(cm_fix60[["specificity"]]), fmt(cm_fix60[["f1"]])
    ),
    sprintf(
      "- Best threshold -> accuracy=%s, precision=%s, recall=%s, specificity=%s, f1=%s",
      fmt(cm_best[["accuracy"]]), fmt(cm_best[["precision"]]), fmt(cm_best[["recall"]]), fmt(cm_best[["specificity"]]), fmt(cm_best[["f1"]])
    ),
    sprintf(
      "- Logit OR per +1 Gemini point: %s (p=%s)",
      fmt(exp(glm_thr_coef$estimate[glm_thr_coef$term == "gemini_quality"])),
      fmt(glm_thr_coef$p.value[glm_thr_coef$term == "gemini_quality"], 5)
    ),
    "",
    "Saved outputs:",
    sprintf("- merged: %s", out_merge),
    sprintf("- report: %s", out_report),
    sprintf("- scatter: %s", plot_scatter),
    sprintf("- density: %s", plot_density),
    sprintf("- roc: %s", plot_roc),
    sprintf("- deciles: %s", plot_deciles),
    sprintf("- calibration: %s", plot_calibration)
  )

  write_report(lines, out_report)
  invisible(list(merged = out_merge, report = out_report))
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

run_model_trad_age_vs_human_validation <- function() {
  bundle <- prepare_validation_data()

  merged <- bundle$manual_valid %>%
    inner_join(bundle$presi, by = "href_key") %>%
    mutate(
      manual_trad = as.integer(manual_trad_vals),
      manual_age = to_num(manual_age),
      model_trad_vals = to_num(model_trad_vals),
      model_has_trad = if ("has_trad" %in% names(.)) as.integer(to_num(has_trad)) else NA_integer_,
      model_age = to_num(model_target_age)
    )

  out_report <- file.path(validation_results_dir, paste0("human_vs_model_trad_age_report_", validation_run_tag, ".txt"))
  out_merged <- file.path(validation_results_dir, paste0("human_vs_model_trad_age_merged_", validation_run_tag, ".csv"))
  plot_age <- file.path(validation_graph_dir, paste0("human_vs_model_target_age_scatter_", validation_run_tag, ".png"))
  plot_trad <- file.path(validation_graph_dir, paste0("human_vs_model_trad_confusion_", validation_run_tag, ".png"))

  readr::write_csv(merged, out_merged)

  # ---- Trad values validation ----
  trad_dat <- merged %>%
    filter(manual_trad %in% c(0, 1)) %>%
    filter(!is.na(model_trad_vals) | !is.na(model_has_trad))

  cm_has <- c(n = 0, accuracy = NA_real_, precision = NA_real_, recall = NA_real_, specificity = NA_real_, f1 = NA_real_)
  if ("model_has_trad" %in% names(trad_dat)) {
    trad_has <- trad_dat %>% filter(model_has_trad %in% c(0, 1))
    if (nrow(trad_has) > 0) {
      cm_has <- conf_metrics(trad_has$manual_trad, trad_has$model_has_trad)
    }
  }

  # For numeric trad_vals (0..5), choose threshold that maximizes F1
  thresholds <- c(-0.5, 0, 1, 2, 3, 4)
  thr_tbl <- lapply(thresholds, function(th) {
    dd <- trad_dat %>% filter(!is.na(model_trad_vals))
    if (nrow(dd) == 0) {
      return(tibble::tibble(threshold = th, n = 0, f1 = NA_real_, precision = NA_real_, recall = NA_real_, specificity = NA_real_, accuracy = NA_real_))
    }
    pred <- if_else(dd$model_trad_vals > th, 1L, 0L)
    cm <- conf_metrics(dd$manual_trad, pred)
    tibble::tibble(
      threshold = th,
      n = cm[["n"]],
      f1 = cm[["f1"]],
      precision = cm[["precision"]],
      recall = cm[["recall"]],
      specificity = cm[["specificity"]],
      accuracy = cm[["accuracy"]]
    )
  }) %>%
    dplyr::bind_rows() %>%
    arrange(desc(f1), desc(recall), desc(precision))

  best_thr <- thr_tbl$threshold[[1]]
  best_row <- thr_tbl[1, ]

  # Confusion heatmap for best threshold
  dd_best <- trad_dat %>% filter(!is.na(model_trad_vals)) %>%
    mutate(pred_trad = if_else(model_trad_vals > best_thr, 1L, 0L))

  heat <- dd_best %>%
    count(manual_trad, pred_trad) %>%
    mutate(
      manual_trad = factor(manual_trad, levels = c(0, 1), labels = c("Manual=0", "Manual=1")),
      pred_trad = factor(pred_trad, levels = c(0, 1), labels = c("Model=0", "Model=1"))
    )

  p_trad <- ggplot(heat, aes(x = pred_trad, y = manual_trad, fill = n)) +
    geom_tile(color = "white") +
    geom_text(aes(label = n), color = "black", size = 6) +
    scale_fill_gradient(low = "#f2f2f2", high = "#1f77b4") +
    labs(
      title = "Trad values: manual vs model",
      subtitle = paste0("Threshold on model trad_vals: > ", best_thr, " | N=", nrow(dd_best)),
      x = "Model prediction",
      y = "Manual label",
      fill = "Count"
    ) +
    theme_minimal(base_size = 12) +
    theme(panel.grid = element_blank())

  save_plot(p_trad, plot_trad, width = 7.5, height = 5.5)

  # ---- Age validation ----
  age_dat <- merged %>% filter(!is.na(manual_age), !is.na(model_age))
  age_met <- reg_metrics(age_dat$manual_age, age_dat$model_age)
  age_fit <- NULL
  age_r2 <- NA_real_
  if (nrow(age_dat) >= 5) {
    age_fit <- lm(manual_age ~ model_age, data = age_dat)
    age_r2 <- summary(age_fit)$r.squared
  }

  p_age <- ggplot(age_dat, aes(x = model_age, y = manual_age)) +
    geom_point(alpha = 0.85, color = "#1f77b4", size = 2.4) +
    geom_smooth(method = "lm", se = TRUE, color = "#d62728", linewidth = 1) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray45") +
    labs(
      title = "Target age: model vs manual",
      subtitle = paste0("N=", nrow(age_dat), " | R2=", fmt(age_r2)),
      x = "Model target_age",
      y = "Manual age"
    ) +
    theme_minimal(base_size = 12)

  save_plot(p_age, plot_age, width = 7.5, height = 6)

  lines <- c(
    "HUMAN VS MODEL VALIDATION: TRAD VALUES + TARGET AGE",
    "===============================================",
    "",
    sprintf("Manual valid rows: %s", nrow(bundle$manual_valid)),
    sprintf("Merged rows (manual + presi): %s", nrow(merged)),
    "",
    "Trad values (manual trad_vals in {0,1}):",
    sprintf("- Manual trad share: %s", fmt(mean(trad_dat$manual_trad == 1, na.rm = TRUE), 4)),
    sprintf("- Model has_trad vs manual (if available): N=%s | F1=%s | precision=%s | recall=%s",
            cm_has[["n"]], fmt(cm_has[["f1"]]), fmt(cm_has[["precision"]]), fmt(cm_has[["recall"]])),
    sprintf("- Model trad_vals threshold scan (pred=1 if trad_vals > thr): best thr=%s | N=%s | F1=%s | precision=%s | recall=%s | specificity=%s | accuracy=%s",
            best_thr,
            best_row$n,
            fmt(best_row$f1),
            fmt(best_row$precision),
            fmt(best_row$recall),
            fmt(best_row$specificity),
            fmt(best_row$accuracy)),
    "",
    "Target age (manual age vs model target_age):",
    sprintf("- N=%s", age_met$n),
    sprintf("- MAE=%s | RMSE=%s | bias(model-manual)=%s", fmt(age_met$mae), fmt(age_met$rmse), fmt(mean(age_dat$model_age - age_dat$manual_age, na.rm = TRUE))),
    sprintf("- Pearson=%s | Spearman=%s | R2(lm manual~model)=%s", fmt(age_met$pearson), fmt(age_met$spearman), fmt(age_r2)),
    sprintf("- Manual mean age=%s | Model mean age=%s", fmt(mean(age_dat$manual_age, na.rm = TRUE), 2), fmt(mean(age_dat$model_age, na.rm = TRUE), 2)),
    "",
    "Saved outputs:",
    sprintf("- merged: %s", out_merged),
    sprintf("- report: %s", out_report),
    sprintf("- trad confusion: %s", plot_trad),
    sprintf("- age scatter: %s", plot_age)
  )

  write_report(lines, out_report)
  invisible(list(report = out_report, merged = out_merged))
}

run_gemini25_merge_manual_and_validate <- function() {
  bundle <- prepare_validation_data()

  merged <- bundle$manual_valid %>%
    left_join(
      bundle$presi %>%
        select(href_key, project_status, model_trad_vals, model_target_age),
      by = "href_key"
    ) %>%
    left_join(bundle$gemini %>% select(href_key, gemini_quality), by = "href_key") %>%
    left_join(bundle$gov %>% select(href_key, application_rating), by = "href_key")

  out_data <- file.path(validation_results_dir, paste0("gemini25_manual_merge_validation_", validation_run_tag, ".csv"))
  out_report <- file.path(validation_results_dir, paste0("gemini25_manual_merge_validation_report_", validation_run_tag, ".txt"))

  readr::write_csv(merged, out_data)

  trad_eval <- merged %>%
    filter(!is.na(model_trad_vals), !is.na(manual_trad_vals)) %>%
    mutate(
      pred = if_else(model_trad_vals >= 0.5, 1, 0),
      truth = if_else(manual_trad_vals >= 0.5, 1, 0)
    )

  cm_trad <- conf_metrics(trad_eval$truth, trad_eval$pred)

  age_eval <- merged %>% filter(!is.na(model_target_age), !is.na(manual_age))
  age_met <- reg_metrics(age_eval$manual_age, age_eval$model_target_age)

  q_gemini <- reg_metrics(merged$manual_quality, merged$gemini_quality)
  q_gov <- reg_metrics(merged$manual_quality, merged$application_rating)

  status_eval <- merged %>% filter(project_status %in% c(0, 1))
  auc_manual <- safe_auc(status_eval$project_status, status_eval$manual_quality)
  auc_gemini <- safe_auc(status_eval$project_status, status_eval$gemini_quality)

  lines <- c(
    "MANUAL LABEL MERGE + VALIDATION (GEMINI 2.5 ERA)",
    "=============================================",
    "",
    sprintf("Manual valid rows: %s", nrow(bundle$manual_valid)),
    sprintf("Merged rows (manual + presi + gemini + gov): %s", nrow(merged)),
    "",
    "TRAD_VALUES validation:",
    sprintf("- N evaluated: %s", cm_trad[["n"]]),
    sprintf("- Accuracy: %s | Precision: %s | Recall: %s | Specificity: %s | F1: %s",
            fmt(cm_trad[["accuracy"]]),
            fmt(cm_trad[["precision"]]),
            fmt(cm_trad[["recall"]]),
            fmt(cm_trad[["specificity"]]),
            fmt(cm_trad[["f1"]])),
    "",
    "AGE validation:",
    sprintf("- N evaluated: %s", age_met$n),
    sprintf("- MAE: %s | RMSE: %s", fmt(age_met$mae), fmt(age_met$rmse)),
    sprintf("- Pearson: %s | Spearman: %s", fmt(age_met$pearson), fmt(age_met$spearman)),
    "",
    "QUALITY validation (manual as reference):",
    sprintf("- Gemini 2.5 quality: N=%s | MAE=%s | RMSE=%s | Spearman=%s", q_gemini$n, fmt(q_gemini$mae), fmt(q_gemini$rmse), fmt(q_gemini$spearman)),
    sprintf("- Government rating: N=%s | MAE=%s | RMSE=%s | Spearman=%s", q_gov$n, fmt(q_gov$mae), fmt(q_gov$rmse), fmt(q_gov$spearman)),
    "",
    "Winner/loser prediction AUC (within manual rows with status):",
    sprintf("- Manual quality AUC: %s", fmt(auc_manual)),
    sprintf("- Gemini 2.5 quality AUC: %s", fmt(auc_gemini)),
    "",
    "Saved outputs:",
    sprintf("- merged: %s", out_data),
    sprintf("- report: %s", out_report)
  )

  write_report(lines, out_report)
  invisible(list(merged = out_data, report = out_report))
}

run_gemini25_measuring_the_gold <- function() {
  bundle <- prepare_validation_data()
  tri <- bundle$triangulation
  tri_gov <- tri %>% filter(!is.na(application_rating))
  tri_status <- tri %>% filter(project_status %in% c(0, 1))

  out_overlap <- file.path(validation_results_dir, paste0("gemini25_triangulation_overlap_", validation_run_tag, ".csv"))
  out_pair <- file.path(validation_results_dir, paste0("gemini25_triangulation_pair_metrics_", validation_run_tag, ".csv"))
  out_report <- file.path(validation_results_dir, paste0("gemini25_validation_report_full_", validation_run_tag, ".txt"))

  plot_gem_vs_gov <- file.path(validation_graph_dir, paste0("gemini25_triangulation_gemini_vs_government_", validation_run_tag, ".png"))
  plot_manual_vs_gov <- file.path(validation_graph_dir, paste0("gemini25_triangulation_manual_vs_government_", validation_run_tag, ".png"))
  plot_gem_vs_manual <- file.path(validation_graph_dir, paste0("gemini25_triangulation_gemini_vs_manual_", validation_run_tag, ".png"))

  readr::write_csv(tri, out_overlap)

  pair_tbl <- bind_rows(
    pair_metrics_tbl(tri, "gemini_quality", "manual_quality", "gemini_vs_manual"),
    pair_metrics_tbl(tri_gov, "gemini_quality", "application_rating", "gemini_vs_government"),
    pair_metrics_tbl(tri_gov, "manual_quality", "application_rating", "manual_vs_government")
  ) %>%
    arrange(desc(spearman), rmse)

  readr::write_csv(pair_tbl, out_pair)

  auc_all_status <- safe_auc(bundle$gemini_vs_status$project_status, bundle$gemini_vs_status$gemini_quality)
  auc_tri_gem <- safe_auc(tri_status$project_status, tri_status$gemini_quality)
  auc_tri_manual <- safe_auc(tri_status$project_status, tri_status$manual_quality)

  if (nrow(tri_gov) > 0) {
    p1 <- ggplot(tri_gov, aes(x = gemini_quality, y = application_rating)) +
      geom_point(alpha = 0.85, color = "#1f77b4", size = 2.4) +
      geom_smooth(method = "lm", se = TRUE, color = "#d62728") +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray45") +
      labs(
        title = "Triangulation: Gemini 2.5 vs government",
        x = "Gemini 2.5 quality",
        y = "Government application_rating"
      ) +
      theme_minimal(base_size = 12)

    p2 <- ggplot(tri_gov, aes(x = manual_quality, y = application_rating)) +
      geom_point(alpha = 0.85, color = "#1f77b4", size = 2.4) +
      geom_smooth(method = "lm", se = TRUE, color = "#d62728") +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray45") +
      labs(
        title = "Triangulation: manual vs government",
        x = "Manual quality",
        y = "Government application_rating"
      ) +
      theme_minimal(base_size = 12)

    save_plot(p1, plot_gem_vs_gov, width = 8, height = 6)
    save_plot(p2, plot_manual_vs_gov, width = 8, height = 6)
  }

  if (nrow(tri) > 0) {
    p3 <- ggplot(tri, aes(x = gemini_quality, y = manual_quality)) +
      geom_point(alpha = 0.85, color = "#1f77b4", size = 2.4) +
      geom_smooth(method = "lm", se = TRUE, color = "#d62728") +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray45") +
      labs(
        title = "Triangulation: Gemini 2.5 vs manual",
        x = "Gemini 2.5 quality",
        y = "Manual quality"
      ) +
      theme_minimal(base_size = 12)

    save_plot(p3, plot_gem_vs_manual, width = 8, height = 6)
  }

  lines <- c(
    "FULL TRIANGULATION REPORT (GEMINI 2.5)",
    "===================================",
    "",
    sprintf("Gemini rows: %s", nrow(bundle$gemini)),
    sprintf("Government rows: %s", nrow(bundle$gov)),
    sprintf("Winner/loser rows: %s", nrow(bundle$gemini_vs_status)),
    sprintf("Manual rows (valid): %s", nrow(bundle$manual_valid)),
    sprintf("Triangulation rows (manual + Gemini): %s", nrow(tri)),
    sprintf("Triangulation rows with government score: %s", nrow(tri_gov)),
    "",
    "Pairwise fit metrics on triangulation set:",
    paste0(
      "- ", pair_tbl$pair,
      " | N=", pair_tbl$n,
      " | MAE=", fmt(pair_tbl$mae),
      " | RMSE=", fmt(pair_tbl$rmse),
      " | Pearson=", fmt(pair_tbl$pearson),
      " | Spearman=", fmt(pair_tbl$spearman),
      " | Kendall=", fmt(pair_tbl$kendall)
    ),
    "",
    "Winner/loser discrimination checks:",
    sprintf("- Gemini 2.5 AUC on full 151k status set: %s", fmt(auc_all_status)),
    sprintf("- Gemini 2.5 AUC on manual-overlap status set: %s", fmt(auc_tri_gem)),
    sprintf("- Manual quality AUC on manual-overlap status set: %s", fmt(auc_tri_manual)),
    "",
    "Saved outputs:",
    sprintf("- overlap data: %s", out_overlap),
    sprintf("- pair metrics: %s", out_pair),
    sprintf("- report: %s", out_report),
    sprintf("- gemini_vs_government plot: %s", plot_gem_vs_gov),
    sprintf("- manual_vs_government plot: %s", plot_manual_vs_gov),
    sprintf("- gemini_vs_manual plot: %s", plot_gem_vs_manual)
  )

  write_report(lines, out_report)
  invisible(list(report = out_report, pair = out_pair, overlap = out_overlap))
}

run_all_gemini25_validations <- function() {
  run_gemini25_vs_government_validation()
  run_gemini25_fit_improvement()
  run_gemini25_scriptclean_log_validation()
  run_gemini25_xgb_fit_to_gov_rmse()
  run_gemini25_vs_government_on_human_subset()
  run_gemini25_vs_manual_quality_validation()
  run_gemini25_vs_government_quality_threshold_tests()
  run_manual_vs_winner_loser_validation()
  run_gemini25_full_model_victory_validation()
  run_gemini25_merge_manual_and_validate()
  run_gemini25_measuring_the_gold()
  run_gemini25_vs_oss120b_quality_validation()
  run_model_trad_age_vs_human_validation()
  message("Gemini 2.5 validation suite completed.")
}
