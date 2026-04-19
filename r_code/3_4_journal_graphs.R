pacman::p_load(dplyr, readr, stringr, ggplot2, pROC, irr, tidyr)

# ── config ────────────────────────────────────────────────────────────────────
run_tag   <- "20260419"
out_dir   <- "latex/newpresigranti"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ── shared theme ──────────────────────────────────────────────────────────────
jtheme <- theme_minimal(base_size = 10) +
  theme(
    plot.title    = element_text(size = 10, face = "bold"),
    plot.subtitle = element_text(size = 8,  color = "grey30"),
    axis.title    = element_text(size = 9),
    axis.text     = element_text(size = 8),
    legend.title  = element_text(size = 8),
    legend.text   = element_text(size = 8),
    panel.grid    = element_blank(),
    plot.margin   = margin(6, 8, 4, 6)
  )

save_plot <- function(p, fname, w = 6, h = 4.5, dpi = 300) {
  path <- file.path(out_dir, fname)
  ggplot2::ggsave(path, p, width = w, height = h, dpi = dpi)
  message("Saved: ", path)
}

# ── helpers ───────────────────────────────────────────────────────────────────
extract_href_key <- function(x) {
  x <- stringr::str_trim(as.character(x))
  x <- stringr::str_to_lower(x)
  x <- stringr::str_replace(x, "^https?://[^/]+", "")
  x <- stringr::str_replace(x, "&.*$", "")
  id <- stringr::str_match(x, "item[?]id=([0-9a-f-]{36})")[, 2]
  key <- ifelse(!is.na(id), paste0("/public/application/item?id=", id), x)
  stringr::str_sub(key, 1, 120)
}

to_num <- function(x) suppressWarnings(as.numeric(x))

fmt <- function(x, d = 2) vapply(x, function(v) {
  if (is.null(v) || length(v) == 0 || is.na(v)) "NA"
  else formatC(v, digits = d, format = "f")
}, character(1))

conf_metrics <- function(truth, pred) {
  keep <- !is.na(truth) & !is.na(pred)
  y <- as.integer(truth[keep]); p <- as.integer(pred[keep])
  if (!length(y)) return(c(n=0, accuracy=NA_real_, precision=NA_real_,
                            recall=NA_real_, specificity=NA_real_, f1=NA_real_))
  tp <- sum(y==1&p==1); tn <- sum(y==0&p==0); fp <- sum(y==0&p==1); fn <- sum(y==1&p==0)
  prec <- if ((tp+fp)>0) tp/(tp+fp) else NA_real_
  rec  <- if ((tp+fn)>0) tp/(tp+fn) else NA_real_
  spec <- if ((tn+fp)>0) tn/(tn+fp) else NA_real_
  f1   <- if (!is.na(prec) && !is.na(rec) && (prec+rec)>0) 2*prec*rec/(prec+rec) else NA_real_
  c(n=length(y), accuracy=(tp+tn)/length(y), precision=prec,
    recall=rec, specificity=spec, f1=f1)
}

reg_metrics <- function(y, yhat) {
  keep <- !is.na(y) & !is.na(yhat); yy <- y[keep]; pp <- yhat[keep]
  if (!length(yy)) return(list(n=0, mae=NA_real_, rmse=NA_real_,
                                pearson=NA_real_, spearman=NA_real_))
  list(n=length(yy), mae=mean(abs(yy-pp)), rmse=sqrt(mean((yy-pp)^2)),
       pearson=suppressWarnings(cor(yy, pp, method="pearson")),
       spearman=suppressWarnings(cor(yy, pp, method="spearman")))
}

# ═══════════════════════════════════════════════════════════════════════════════
# PART A — LLM vs. Human (Fra full Google Sheet log)
# ═══════════════════════════════════════════════════════════════════════════════

fra_raw <- readr::read_csv("presi_gold_fra - labels_franci_log.csv",
                            show_col_types = FALSE)
fra <- fra_raw %>%
  mutate(
    href_key         = extract_href_key(href),
    manual_trad_vals = to_num(trad_vals),
    manual_age       = to_num(age),
    manual_score     = to_num(preliminary_score),
    submitted_at_ts  = suppressWarnings(
      as.numeric(as.POSIXct(submitted_at, tz = "UTC")))
  ) %>%
  filter(!is.na(href_key), href_key != "") %>%
  arrange(href_key, desc(submitted_at_ts), desc(row_id)) %>%
  distinct(href_key, .keep_all = TRUE)

presi <- readr::read_csv("data/data_large/presi_variables_imputed.csv",
                          show_col_types = FALSE) %>%
  mutate(href_key         = extract_href_key(href),
         model_trad_vals  = to_num(trad_vals),
         model_target_age = to_num(target_age)) %>%
  filter(!is.na(href_key), href_key != "") %>%
  arrange(href_key) %>% distinct(href_key, .keep_all = TRUE) %>%
  select(href_key, model_trad_vals, model_target_age)

gemini <- readr::read_csv("data/data_large/quality_scores_2026-02-14.csv",
                           show_col_types = FALSE) %>%
  mutate(href_key = extract_href_key(href),
         gemini_quality = rowMeans(
           cbind(to_num(relevance), to_num(coherence),
                 to_num(budget_realism), to_num(scale)), na.rm = TRUE)) %>%
  filter(!is.na(href_key), href_key != "") %>%
  arrange(href_key, desc(gemini_quality)) %>%
  distinct(href_key, .keep_all = TRUE) %>%
  select(href_key, gemini_quality)

merged <- fra %>%
  inner_join(presi,  by = "href_key") %>%
  left_join(gemini,  by = "href_key")

# ── A1. Rhetoric (trad values) confusion heatmap ──────────────────────────────
trad_dat <- merged %>% filter(manual_trad_vals %in% c(0,1), !is.na(model_trad_vals))

thr_tbl <- lapply(c(-0.5, 0, 1, 2, 3, 4), function(th) {
  pred <- dplyr::if_else(trad_dat$model_trad_vals > th, 1L, 0L)
  cm   <- conf_metrics(trad_dat$manual_trad_vals, pred)
  tibble::tibble(threshold=th, n=cm[["n"]], f1=cm[["f1"]],
                 precision=cm[["precision"]], recall=cm[["recall"]],
                 specificity=cm[["specificity"]], accuracy=cm[["accuracy"]])
}) %>% dplyr::bind_rows() %>% arrange(desc(f1), desc(recall), desc(precision))

best_thr <- thr_tbl$threshold[[1]]
best_row <- thr_tbl[1, ]
auc_trad <- suppressWarnings(tryCatch(
  as.numeric(pROC::auc(trad_dat$manual_trad_vals, trad_dat$model_trad_vals, quiet=TRUE)),
  error = function(e) NA_real_))

dd_best <- trad_dat %>%
  mutate(pred_trad = dplyr::if_else(model_trad_vals > best_thr, 1L, 0L))
heat <- dd_best %>%
  count(manual_trad_vals, pred_trad) %>%
  mutate(
    manual_trad_vals = factor(manual_trad_vals, levels=c(1,0),
                              labels=c("Patriotic = yes","Patriotic = no")),
    pred_trad        = factor(pred_trad,        levels=c(0,1),
                              labels=c("LLM = no","LLM = yes"))
  )

p_trad <- ggplot(heat, aes(x=pred_trad, y=manual_trad_vals, fill=n)) +
  geom_tile(color="white", linewidth=0.8) +
  geom_text(aes(label=n), color="black", size=5, fontface="bold") +
  scale_fill_gradient(low="#f0f4ff", high="#2166ac") +
  labs(
    title    = "Patriotic rhetoric: LLM vs. human coding",
    subtitle = sprintf("N = %d  |  F\u2081 = %.2f  |  Precision = %.2f  |  Recall = %.2f  |  AUC = %.3f",
                       nrow(dd_best), best_row$f1, best_row$precision,
                       best_row$recall, auc_trad),
    x = "LLM prediction", y = "Human label", fill = "Count"
  ) +
  jtheme
save_plot(p_trad, paste0("human_vs_model_trad_confusion_", run_tag, ".png"),
          w = 5.5, h = 4)

# ── A2. Target age scatter ────────────────────────────────────────────────────
age_dat  <- merged %>%
  filter(!is.na(manual_age), !is.na(model_target_age),
         manual_age >= 0, manual_age <= 120)
age_met  <- reg_metrics(age_dat$manual_age, age_dat$model_target_age)
age_r2   <- summary(lm(manual_age ~ model_target_age, data=age_dat))$r.squared
bias_age <- mean(age_dat$model_target_age - age_dat$manual_age, na.rm=TRUE)

p_age <- ggplot(age_dat, aes(x=model_target_age, y=manual_age)) +
  geom_point(alpha=0.55, color="#2166ac", size=1.6) +
  geom_smooth(method="lm", se=TRUE, color="#d62728", linewidth=0.9, fill="#f4b8b8") +
  geom_abline(slope=1, intercept=0, linetype="dashed", color="grey55", linewidth=0.6) +
  labs(
    title    = "Target age: LLM vs. human coding",
    subtitle = sprintf("N = %d  |  r = %.2f  |  MAE = %.1f yr  |  bias = %+.1f yr",
                       age_met$n, age_met$pearson, age_met$mae, bias_age),
    x = "LLM estimate (years)", y = "Human estimate (years)"
  ) +
  jtheme + theme(panel.grid.major = element_line(color="grey92", linewidth=0.3))
save_plot(p_age, paste0("human_vs_model_target_age_scatter_", run_tag, ".png"),
          w = 5.5, h = 4.5)

# ── A3. Application quality scatter ──────────────────────────────────────────
q_dat  <- merged %>%
  filter(!is.na(manual_score), !is.na(gemini_quality),
         manual_score >= 0, manual_score <= 100)
q_met  <- reg_metrics(q_dat$manual_score, q_dat$gemini_quality)
q_r2   <- summary(lm(manual_score ~ gemini_quality, data=q_dat))$r.squared
bias_q <- mean(q_dat$gemini_quality - q_dat$manual_score, na.rm=TRUE)

p_quality <- ggplot(q_dat, aes(x=gemini_quality, y=manual_score)) +
  geom_point(alpha=0.55, color="#2166ac", size=1.6) +
  geom_smooth(method="lm", se=TRUE, color="#d62728", linewidth=0.9, fill="#f4b8b8") +
  geom_abline(slope=1, intercept=0, linetype="dashed", color="grey55", linewidth=0.6) +
  labs(
    title    = "Application quality: LLM vs. human coding",
    subtitle = sprintf("N = %d  |  r = %.2f  |  R\u00b2 = %.2f  |  MAE = %.1f pts  |  bias = %+.1f pts",
                       q_met$n, q_met$pearson, q_r2, q_met$mae, bias_q),
    x = "LLM quality score (Gemini 2.5)", y = "Human quality score"
  ) +
  jtheme + theme(panel.grid.major = element_line(color="grey92", linewidth=0.3))
save_plot(p_quality, paste0("gold_vs_gemini_overall_quality_scatter_", run_tag, ".png"),
          w = 5.5, h = 4.5)

# ═══════════════════════════════════════════════════════════════════════════════
# PART B — Inter-coder reliability
# ═══════════════════════════════════════════════════════════════════════════════

extract_id <- function(href) {
  m <- regmatches(href, regexpr("item[?]id=([0-9a-f-]{36})", href,
                                perl = TRUE, ignore.case = TRUE))
  ifelse(length(m) == 0 | m == "", NA_character_,
         tolower(sub(".*id=", "", m)))
}

fra_ic  <- read_csv("data/data_gold/manual_trad_age_quality_labels_fra_20260318.csv",
                    show_col_types = FALSE)
gena_ic <- read_csv("data/data_gold/manual_trad_age_quality_labels_gena_20260318.csv",
                    show_col_types = FALSE)

fra_ic$app_id  <- sapply(fra_ic$href,  extract_id)
gena_ic$app_id <- sapply(gena_ic$href, extract_id)

fra_ic  <- fra_ic  %>% filter(!is.na(app_id)) %>%
  arrange(app_id, desc(row_id)) %>% distinct(app_id, .keep_all = TRUE)
gena_ic <- gena_ic %>% filter(!is.na(app_id)) %>%
  arrange(app_id, desc(row_id)) %>% distinct(app_id, .keep_all = TRUE)

shared <- inner_join(
  fra_ic  %>% select(app_id, trad_fra=trad_vals, age_fra=age,
                     score_fra=preliminary_score),
  gena_ic %>% select(app_id, trad_gena=trad_vals, age_gena=age,
                     score_gena=preliminary_score),
  by = "app_id"
) %>% mutate(across(everything(),
               ~ if (is.character(.)) suppressWarnings(as.numeric(.)) else .))

# ── B1. Intercoder trad confusion ─────────────────────────────────────────────
tv <- shared %>% filter(trad_fra %in% c(0,1), trad_gena %in% c(0,1))
kappa_tv  <- irr::kappa2(data.frame(tv$trad_fra, tv$trad_gena), weight="unweighted")
pct_agree <- mean(tv$trad_fra == tv$trad_gena) * 100
tv_tbl    <- table(coder1=tv$trad_fra, coder2=tv$trad_gena)

heat_ic <- as.data.frame(tv_tbl) %>%
  mutate(coder1 = factor(coder1, levels=c(1,0),
                         labels=c("Coder 1 = yes","Coder 1 = no")),
         coder2 = factor(coder2, levels=c(0,1),
                         labels=c("Coder 2 = no","Coder 2 = yes")))

p_ic_trad <- ggplot(heat_ic, aes(x=coder2, y=coder1, fill=Freq)) +
  geom_tile(color="white", linewidth=0.8) +
  geom_text(aes(label=Freq), size=5, fontface="bold", color="black") +
  scale_fill_gradient(low="#f0f4ff", high="#2166ac") +
  labs(
    title    = "Patriotic rhetoric: inter-coder agreement",
    subtitle = sprintf("N = %d  |  \u03ba = %.3f  |  Agreement = %.1f%%",
                       nrow(tv), kappa_tv$value, pct_agree),
    x = "Coder 2", y = "Coder 1", fill = "Count"
  ) +
  jtheme
save_plot(p_ic_trad, paste0("intercoder_trad_confusion_", run_tag, ".png"), w=5.5, h=4)

# ── B2. Intercoder age scatter ────────────────────────────────────────────────
ag <- shared %>% filter(!is.na(age_fra), !is.na(age_gena))
icc_age <- irr::icc(data.frame(ag$age_fra, ag$age_gena),
                    model="twoway", type="agreement", unit="single")
cor_age <- cor.test(ag$age_fra, ag$age_gena, method="pearson")
mae_age <- mean(abs(ag$age_fra - ag$age_gena))

p_ic_age <- ggplot(ag, aes(x=age_gena, y=age_fra)) +
  geom_point(alpha=0.55, color="#2166ac", size=1.6) +
  geom_smooth(method="lm", se=TRUE, color="#d62728", linewidth=0.9, fill="#f4b8b8") +
  geom_abline(slope=1, intercept=0, linetype="dashed", color="grey55", linewidth=0.6) +
  labs(
    title    = "Target age: inter-coder agreement",
    subtitle = sprintf("N = %d  |  ICC = %.3f  |  r = %.2f  |  MAE = %.1f yr",
                       nrow(ag), icc_age$value, cor_age$estimate, mae_age),
    x = "Coder 2 (years)", y = "Coder 1 (years)"
  ) +
  jtheme + theme(panel.grid.major = element_line(color="grey92", linewidth=0.3))
save_plot(p_ic_age, paste0("intercoder_age_scatter_", run_tag, ".png"), w=5.5, h=4.5)

# ── B3. Intercoder quality scatter ────────────────────────────────────────────
sc <- shared %>% filter(!is.na(score_fra), !is.na(score_gena),
                        score_fra>=0, score_fra<=100,
                        score_gena>=0, score_gena<=100)
icc_sc  <- irr::icc(data.frame(sc$score_fra, sc$score_gena),
                    model="twoway", type="agreement", unit="single")
cor_sc  <- cor.test(sc$score_fra, sc$score_gena, method="pearson")
mae_sc  <- mean(abs(sc$score_fra - sc$score_gena))

p_ic_score <- ggplot(sc, aes(x=score_gena, y=score_fra)) +
  geom_point(alpha=0.55, color="#2166ac", size=1.6) +
  geom_smooth(method="lm", se=TRUE, color="#d62728", linewidth=0.9, fill="#f4b8b8") +
  geom_abline(slope=1, intercept=0, linetype="dashed", color="grey55", linewidth=0.6) +
  labs(
    title    = "Quality score: inter-coder agreement",
    subtitle = sprintf("N = %d  |  ICC = %.3f  |  r = %.2f  |  MAE = %.1f pts",
                       nrow(sc), icc_sc$value, cor_sc$estimate, mae_sc),
    x = "Coder 2 score", y = "Coder 1 score"
  ) +
  jtheme + theme(panel.grid.major = element_line(color="grey92", linewidth=0.3))
save_plot(p_ic_score, paste0("intercoder_score_scatter_", run_tag, ".png"), w=5.5, h=4.5)

message("\nAll journal-ready graphs saved to: ", out_dir)
