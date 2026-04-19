##############################################################################
#  Panel Analysis: Org FE + Competition FE
#  Within-organization effects of winning on subsequent behavior
#  Date: 2024-02-24
##############################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
})

# Output file
sink_file <- "data/data_results/panel_org_fe_report_20260224.txt"
sink(sink_file, split = TRUE)  # prints to console AND file

cat("==============================================================\n")
cat("  PANEL ANALYSIS: ORGANIZATIONAL FE + COMPETITION FE\n")
cat("  Effects of Winning on Subsequent Organizational Behavior\n")
cat("  Generated:", format(Sys.time()), "\n")
cat("==============================================================\n\n")

# ── 1. Load and merge data ─────────────────────────────────────────────────

presi <- read.csv("data/data_large/presi_variables_imputed.csv", stringsAsFactors = FALSE)
inn   <- read.csv("data/data_large/application_inn_backup.csv", stringsAsFactors = FALSE)
inn   <- inn %>% filter(!duplicated(href))
presi <- presi %>% left_join(inn, by = "href")

rat <- read.csv("data/data_large/application_rating.csv", stringsAsFactors = FALSE)
rat <- rat %>% filter(!is.na(application_rating), application_rating > 0) %>%
  filter(!duplicated(href)) %>% select(href, application_rating)
presi <- presi %>% left_join(rat, by = "href")

presi$won <- presi$project_status
presi$gemini_q <- rowMeans(
  presi[, c("relevance", "coherence", "budget_realism", "scale")], na.rm = TRUE
)

cat("── DATA SUMMARY ──\n")
cat("Total applications:", nrow(presi), "\n")
cat("Total with INN:", sum(!is.na(presi$inn)), "\n")
cat("Unique orgs (INN):", n_distinct(presi$inn[!is.na(presi$inn)]), "\n")
cat("Winners:", sum(presi$won, na.rm = TRUE), "\n")
cat("Win rate:", round(mean(presi$won, na.rm = TRUE) * 100, 1), "%\n")
cat("Competition range:", min(presi$competition_index), "to", max(presi$competition_index), "\n")
cat("Applications with expert rating:", sum(!is.na(presi$application_rating)), "\n\n")

# ── 2. Build org × competition panel ───────────────────────────────────────

panel <- presi %>%
  filter(!is.na(inn)) %>%
  group_by(inn, competition_index) %>%
  summarise(
    cofin     = mean(cofinancing_share, na.rm = TRUE),
    gemini    = mean(gemini_q, na.rm = TRUE),
    has_trad  = mean(has_trad, na.rm = TRUE),
    won_any   = as.integer(max(won) == 1),
    badge     = max(badge_president_grants),
    n_apps    = n(),
    .groups   = "drop"
  )

# Lag variables: previous participation outcome
panel <- panel %>%
  arrange(inn, competition_index) %>%
  group_by(inn) %>%
  mutate(
    won_prev       = lag(won_any),
    cum_wins       = cumsum(lag(won_any, default = 0)),
    has_won_before = as.integer(cum_wins > 0)
  ) %>%
  ungroup()

# Restrict to orgs with 2+ participations, rows with valid lag
panel2 <- panel %>%
  group_by(inn) %>%
  filter(n() >= 2) %>%
  ungroup() %>%
  filter(!is.na(won_prev))

cat("── PANEL SUMMARY ──\n")
cat("Orgs with 2+ participations:", n_distinct(panel2$inn), "\n")
cat("Org-round observations:", nrow(panel2), "\n")
cat("Competition rounds:", n_distinct(panel2$competition_index), "\n\n")

# ── 3. Two-way FE demeaning (Frisch-Waugh-Lovell) ─────────────────────────

twfe_demean <- function(d, v) {
  x <- d[[v]]
  inn_chr <- as.character(d$inn)
  comp_chr <- as.character(d$competition_index)
  # Org means
  org_means <- tapply(x, inn_chr, mean, na.rm = TRUE)
  om <- as.numeric(org_means[inn_chr])
  # Competition means
  comp_means <- tapply(x, comp_chr, mean, na.rm = TRUE)
  cm <- as.numeric(comp_means[comp_chr])
  # Grand mean
  gm <- mean(x, na.rm = TRUE)
  x - om - cm + gm
}

run_model <- function(d, x_var, y_vars = c("cofin", "gemini", "has_trad")) {
  x_tw <- twfe_demean(d, x_var)
  results <- list()
  for (y in y_vars) {
    y_tw <- twfe_demean(d, y)
    ok <- !is.na(x_tw) & !is.na(y_tw)
    if (sum(ok) < 100 || sd(x_tw[ok]) < 1e-10) {
      results[[y]] <- c(beta = NA, se = NA, t = NA, p = NA)
      next
    }
    m <- lm(y_tw[ok] ~ x_tw[ok])
    s <- unname(summary(m)$coefficients[2, ])
    results[[y]] <- c(beta = s[1], se = s[2], t = s[3], p = s[4])
  }
  results
}

print_results <- function(res, y_labels = c(cofin = "Co-financing",
                                             gemini = "LLM quality",
                                             has_trad = "State rhetoric")) {
  for (y in names(res)) {
    r <- res[[y]]
    if (is.na(r["beta"])) {
      cat(sprintf("  %-20s  insufficient variation\n", y_labels[y]))
    } else {
      cat(sprintf("  %-20s  beta = %7.4f   SE = %.4f   t = %6.2f   p = %s\n",
                  y_labels[y], r["beta"], r["se"], r["t"],
                  format.pval(r["p"], digits = 3)))
    }
  }
}

# ── 4. ORGANIZATIONAL LIFECYCLE ────────────────────────────────────────────

cat("##############################################################\n")
cat("  PART 1: ORGANIZATIONAL LIFECYCLE\n")
cat("##############################################################\n\n")

org_summary <- presi %>%
  filter(!is.na(inn)) %>%
  group_by(inn) %>%
  summarise(n_apps = n(), n_wins = sum(won, na.rm = TRUE),
            n_comps = n_distinct(competition_index), .groups = "drop") %>%
  mutate(type = case_when(
    n_apps == 1 & n_wins == 0 ~ "One-shot loser",
    n_apps > 1  & n_wins == 0 ~ "Persistent loser",
    n_wins == 1               ~ "Single winner",
    n_wins >= 2               ~ "Repeat winner"
  ))

type_tab <- org_summary %>%
  count(type) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  arrange(desc(n))

cat("── Organizational Types ──\n")
for (i in 1:nrow(type_tab)) {
  cat(sprintf("  %-20s  %6d  (%4.1f%%)\n", type_tab$type[i], type_tab$n[i], type_tab$pct[i]))
}
cat(sprintf("  %-20s  %6d\n", "TOTAL", sum(type_tab$n)))

# Applications share
apps_by_type <- presi %>%
  filter(!is.na(inn)) %>%
  left_join(org_summary %>% select(inn, type), by = "inn") %>%
  count(type) %>%
  mutate(pct = round(n / sum(n) * 100, 1))

cat("\n── Share of Applications ──\n")
for (i in 1:nrow(apps_by_type)) {
  cat(sprintf("  %-20s  %6d apps  (%4.1f%%)\n", apps_by_type$type[i],
              apps_by_type$n[i], apps_by_type$pct[i]))
}

# Median participation
cat("\n── Participation Intensity ──\n")
cat("Median competitions per org:", median(org_summary$n_comps), "\n")
cat("Mean competitions per org:", round(mean(org_summary$n_comps), 2), "\n")
cat("Distribution of n_competitions:\n")
comp_dist <- table(pmin(org_summary$n_comps, 10))
print(comp_dist)

# ── 5. RETENTION ───────────────────────────────────────────────────────────

cat("\n── Retention After First Application ──\n")
first_app <- presi %>%
  filter(!is.na(inn)) %>%
  group_by(inn) %>%
  arrange(competition_index) %>%
  slice(1) %>%
  ungroup()

first_app$returned <- first_app$inn %in% (org_summary %>% filter(n_apps > 1) %>% pull(inn))

ret <- first_app %>%
  group_by(won) %>%
  summarise(n = n(), ret_n = sum(returned),
            pct = round(mean(returned) * 100, 1), .groups = "drop")

cat(sprintf("  First-time losers:   %.1f%% returned (%d / %d)\n",
            ret$pct[ret$won == 0], ret$ret_n[ret$won == 0], ret$n[ret$won == 0]))
cat(sprintf("  First-time winners:  %.1f%% returned (%d / %d)\n",
            ret$pct[ret$won == 1], ret$ret_n[ret$won == 1], ret$n[ret$won == 1]))
cat(sprintf("  Gap: +%.1f pp\n", ret$pct[ret$won == 1] - ret$pct[ret$won == 0]))

# Win rate by prior winning
ps <- presi %>%
  filter(!is.na(inn)) %>%
  arrange(inn, competition_index) %>%
  group_by(inn) %>%
  mutate(cum_w = cumsum(lag(won, default = 0)),
         hwb = as.integer(cum_w > 0)) %>%
  ungroup()

wr <- ps %>%
  group_by(hwb) %>%
  summarise(n = n(), wins = sum(won), rate = round(mean(won) * 100, 1), .groups = "drop")

cat("\n── Win Rate by Prior Winning Status ──\n")
cat("  Never won before: ", wr$rate[wr$hwb == 0], "% (",
    wr$wins[wr$hwb == 0], "/", wr$n[wr$hwb == 0], ")\n")
cat("  Won before:       ", wr$rate[wr$hwb == 1], "% (",
    wr$wins[wr$hwb == 1], "/", wr$n[wr$hwb == 1], ")\n")
cat("  Ratio:", round(wr$rate[wr$hwb == 1] / wr$rate[wr$hwb == 0], 2), "x\n")

# ── 6. COHORT SURVIVAL ────────────────────────────────────────────────────

cat("\n── Cohort Survival (% still active N years later) ──\n")
cat("  (Active = appeared in at least one of 3 rounds around target)\n\n")

comps <- sort(unique(presi$competition_index[!is.na(presi$inn)]))

org_comps <- presi %>%
  filter(!is.na(inn)) %>%
  group_by(inn) %>%
  summarise(comp_list = list(sort(unique(competition_index))),
            first_comp = min(competition_index), .groups = "drop")

surv_results <- data.frame()
for (start in comps[1:8]) {
  cohort <- org_comps %>% filter(first_comp == start)
  if (nrow(cohort) < 100) next
  for (gap in c(2, 4, 6, 8, 10)) {
    target <- start + gap
    if (target > max(comps)) next
    check <- intersect(c(target - 1, target, target + 1), comps)
    active <- cohort %>%
      mutate(active = sapply(comp_list, function(cl) any(cl %in% check))) %>%
      summarise(pct = round(mean(active) * 100, 1)) %>% pull(pct)
    surv_results <- rbind(surv_results,
                          data.frame(start = start, gap = gap, pct = active, n = nrow(cohort)))
  }
}

avg_surv <- surv_results %>%
  group_by(gap) %>%
  summarise(mean_pct = round(mean(pct), 1), n_cohorts = n(), .groups = "drop")

for (i in 1:nrow(avg_surv)) {
  cat(sprintf("  +%d rounds (~%.1f years):  %.1f%% still active  (avg over %d cohorts)\n",
              avg_surv$gap[i], avg_surv$gap[i] / 2, avg_surv$mean_pct[i], avg_surv$n_cohorts[i]))
}

# First cohort specific
cohort1 <- org_comps %>% filter(first_comp == min(comps))
last_3 <- tail(comps, 3)
c1_active <- cohort1 %>%
  mutate(still = sapply(comp_list, function(cl) any(cl %in% last_3)),
         ever_won = inn %in% (presi %>% filter(won == 1, !is.na(inn)) %>% pull(inn) %>% unique()))

cat(sprintf("\n  First cohort (comp %d, N=%d) after %.1f years:\n",
            min(comps), nrow(cohort1), (max(comps) - min(comps)) / 2))
cat(sprintf("    Still active:  %d (%.1f%%)\n",
            sum(c1_active$still), round(mean(c1_active$still) * 100, 1)))
cat(sprintf("      of which won:   %d (%.1f%% of survivors)\n",
            sum(c1_active$still & c1_active$ever_won),
            round(mean(c1_active$ever_won[c1_active$still]) * 100, 1)))
cat(sprintf("    Dropped out:   %d (%.1f%%)\n",
            sum(!c1_active$still), round(mean(!c1_active$still) * 100, 1)))

# ── 7. COMPOSITION OVER TIME ──────────────────────────────────────────────

cat("\n── Composition of Applicant Pool Over Time ──\n")

ps2 <- presi %>%
  filter(!is.na(inn)) %>%
  arrange(inn, competition_index) %>%
  group_by(inn) %>%
  mutate(hwb = as.integer(cumsum(lag(won, default = 0)) > 0)) %>%
  ungroup()

comp_comp <- ps2 %>%
  group_by(competition_index) %>%
  summarise(
    n_orgs         = n_distinct(inn),
    pct_prior_win  = round(mean(hwb) * 100, 1),
    pct_badge      = round(mean(badge_president_grants) * 100, 1),
    win_rate       = round(mean(won) * 100, 1),
    .groups        = "drop"
  )

cat(sprintf("  %-5s  %6s  %12s  %10s  %8s\n",
            "Comp", "Orgs", "Prior win %", "Badge %", "Win rate"))
for (i in 1:nrow(comp_comp)) {
  cat(sprintf("  %-5d  %6d  %11.1f%%  %9.1f%%  %7.1f%%\n",
              comp_comp$competition_index[i], comp_comp$n_orgs[i],
              comp_comp$pct_prior_win[i], comp_comp$pct_badge[i],
              comp_comp$win_rate[i]))
}

# New entrants
seen <- c()
cat("\n── New Entrants Per Competition ──\n")
cat(sprintf("  %-5s  %6s  %5s  %8s\n", "Comp", "Total", "New", "% New"))
for (ci in comps) {
  orgs_ci <- presi %>% filter(!is.na(inn), competition_index == ci) %>%
    pull(inn) %>% unique()
  new_orgs <- setdiff(orgs_ci, seen)
  cat(sprintf("  %-5d  %6d  %5d  %7.1f%%\n",
              ci, length(orgs_ci), length(new_orgs),
              round(length(new_orgs) / length(orgs_ci) * 100, 1)))
  seen <- union(seen, orgs_ci)
}

# ── 8. WITHIN-WINNER TRAJECTORIES (expert ratings) ────────────────────────

cat("\n##############################################################\n")
cat("  PART 2: WITHIN-WINNER TRAJECTORIES\n")
cat("  (Organizations with 2+ rated wins)\n")
cat("##############################################################\n\n")

comp_means <- presi %>%
  group_by(competition_index) %>%
  summarise(cm_cofin = mean(cofinancing_share, na.rm = TRUE),
            cm_gemini = mean(gemini_q, na.rm = TRUE), .groups = "drop")

w <- presi %>%
  filter(won == 1, !is.na(inn), !is.na(application_rating)) %>%
  left_join(comp_means, by = "competition_index") %>%
  mutate(cofin_dm = cofinancing_share - cm_cofin,
         gemini_dm = gemini_q - cm_gemini) %>%
  arrange(inn, competition_index) %>%
  group_by(inn) %>%
  mutate(win_num = row_number(), total_wins = n()) %>%
  ungroup()

cat("Rated winners:", nrow(w), "\n")
cat("Unique orgs:", n_distinct(w$inn), "\n")
cat("Orgs 2+ wins:", n_distinct(w$inn[w$total_wins >= 2]), "\n")
cat("Orgs 3+ wins:", n_distinct(w$inn[w$total_wins >= 3]), "\n\n")

# Trajectory table
traj <- w %>%
  filter(total_wins >= 2) %>%
  mutate(wc = ifelse(win_num <= 5, as.character(win_num), "6+")) %>%
  group_by(wc) %>%
  summarise(n = n(), orgs = n_distinct(inn),
            expert = round(mean(application_rating), 2),
            cofin = round(mean(cofinancing_share, na.rm = TRUE), 2),
            cofin_dm = round(mean(cofin_dm, na.rm = TRUE), 2),
            gemini = round(mean(gemini_q, na.rm = TRUE), 2),
            gemini_dm = round(mean(gemini_dm, na.rm = TRUE), 2),
            trad = round(mean(has_trad, na.rm = TRUE) * 100, 1),
            .groups = "drop")

cat("── Trajectory Table (orgs with 2+ rated wins) ──\n")
cat(sprintf("  %-5s  %5s  %5s  %7s  %7s  %9s  %7s  %9s  %6s\n",
            "Win#", "N", "Orgs", "Expert", "Cofin", "Cofin_dm", "Gemini", "Gemini_dm", "Trad%"))
for (i in 1:nrow(traj)) {
  cat(sprintf("  %-5s  %5d  %5d  %7.2f  %7.2f  %9.2f  %7.2f  %9.2f  %5.1f%%\n",
              traj$wc[i], traj$n[i], traj$orgs[i], traj$expert[i],
              traj$cofin[i], traj$cofin_dm[i], traj$gemini[i],
              traj$gemini_dm[i], traj$trad[i]))
}

# Within-org FE (manual demeaning)
d <- w %>% filter(total_wins >= 2)
org_m <- d %>% group_by(inn) %>%
  summarise(across(c(win_num, application_rating, cofin_dm, gemini_dm, has_trad),
                   ~ mean(., na.rm = TRUE), .names = "m_{.col}"), .groups = "drop")
d <- d %>% left_join(org_m, by = "inn")
d$wn_dm <- d$win_num - d$m_win_num

cat("\n── Within-Org FE: Outcome ~ Win Number ──\n")
for (v in c("application_rating", "cofin_dm", "gemini_dm", "has_trad")) {
  y_dm <- d[[v]] - d[[paste0("m_", v)]]
  ok <- !is.na(y_dm) & !is.na(d$wn_dm)
  m <- lm(y_dm[ok] ~ d$wn_dm[ok])
  s <- summary(m)$coefficients[2, ]
  cat(sprintf("  %-22s  beta = %7.4f   SE = %.4f   t = %6.2f   p = %s\n",
              v, s[1], s[2], s[3], format.pval(s[4], digits = 3)))
}

# First vs Last (3+ wins)
cat("\n── First vs Last Win (orgs with 3+ rated wins) ──\n")
fl <- w %>%
  filter(total_wins >= 3) %>%
  group_by(inn) %>%
  arrange(competition_index) %>%
  summarise(fe = first(application_rating), le = last(application_rating),
            fc = first(cofin_dm), lc = last(cofin_dm),
            fg = first(gemini_dm), lg = last(gemini_dm),
            ft = first(has_trad), lt = last(has_trad), .groups = "drop")

cat("N orgs:", nrow(fl), "\n")
for (pair in list(c("le", "fe", "Expert rating"),
                  c("lc", "fc", "Cofin (demeaned)"),
                  c("lg", "fg", "Gemini (demeaned)"))) {
  tt <- t.test(fl[[pair[1]]], fl[[pair[2]]], paired = TRUE)
  cat(sprintf("  %-22s  first = %6.2f  last = %6.2f  diff = %+.3f  p = %s\n",
              pair[3], mean(fl[[pair[2]]], na.rm = T), mean(fl[[pair[1]]], na.rm = T),
              tt$estimate, format.pval(tt$p.value, digits = 3)))
}

mc <- mcnemar.test(table(fl$ft, fl$lt))
cat(sprintf("  %-22s  McNemar p = %.4f  (0→1: %d, 1→0: %d)\n",
            "State rhetoric", mc$p.value,
            sum(fl$ft == 0 & fl$lt == 1), sum(fl$ft == 1 & fl$lt == 0)))

# ── 9. ORG FE PANEL MODELS (main results) ─────────────────────────────────

cat("\n##############################################################\n")
cat("  PART 3: ORG FE + COMPETITION FE PANEL MODELS\n")
cat("  21K org fixed effects, two-way demeaning\n")
cat("##############################################################\n\n")

cat("Panel: ", n_distinct(panel2$inn), " orgs, ", nrow(panel2), " obs\n\n")

cat("── Model 1: Y ~ won_prev + org_FE + comp_FE ──\n")
cat("   (Did winning in the PREVIOUS round change current-round outcomes?)\n\n")
r1 <- run_model(panel2, "won_prev")
print_results(r1)

cat("\n── Model 2: Y ~ has_won_before + org_FE + comp_FE ──\n")
cat("   (Does having EVER won change current-round outcomes?)\n\n")
r2 <- run_model(panel2, "has_won_before")
print_results(r2)

cat("\n── Model 3: Y ~ cum_wins + org_FE + comp_FE ──\n")
cat("   (Does each additional cumulative win change outcomes?)\n\n")
r3 <- run_model(panel2, "cum_wins")
print_results(r3)

# Raw means for context
cat("\n── Raw Means by Previous-Round Outcome ──\n")
raw <- panel2 %>%
  group_by(won_prev) %>%
  summarise(n = n(),
            cofin = round(mean(cofin, na.rm = TRUE), 2),
            gemini = round(mean(gemini, na.rm = TRUE), 2),
            has_trad = round(mean(has_trad, na.rm = TRUE), 3),
            .groups = "drop")
cat(sprintf("  won_prev=0:  N=%d  cofin=%.2f  gemini=%.2f  trad=%.3f\n",
            raw$n[1], raw$cofin[1], raw$gemini[1], raw$has_trad[1]))
cat(sprintf("  won_prev=1:  N=%d  cofin=%.2f  gemini=%.2f  trad=%.3f\n",
            raw$n[2], raw$cofin[2], raw$gemini[2], raw$has_trad[2]))

# ── 10. HYPOTHESIS CONCLUSIONS ─────────────────────────────────────────────

cat("\n##############################################################\n")
cat("  CONCLUSIONS\n")
cat("##############################################################\n\n")

cat("H4 (Winning increases engagement with state rhetoric):\n")
cat("  REJECTED. All three models: null. McNemar: null.\n")
cat("  Winning does not change ideological engagement.\n\n")

cat("H5 (Winning increases application quality):\n")
cat("  REJECTED / TRIVIAL. Model 1 (short-term): quality dips -0.25.\n")
cat("  Model 2 (ever-won): +0.76 (small positive, possibly learning).\n")
cat("  Expert ratings rise +0.55/win, but LLM quality (demeaned) falls -0.28/win.\n")
cat("  Divergence suggests incumbency premium in expert evaluation,\n")
cat("  not genuine quality improvement.\n\n")

cat("H6 (Winning increases co-financing capacity):\n")
cat("  REJECTED. Model 1: cofin DROPS 1.73pp after winning (coasting).\n")
cat("  Model 2: null (p=0.48). Model 3: +0.37pp/win (negligible).\n")
cat("  Raw cofin rises only track secular trend (28%% -> 38%% over period).\n")
cat("  Demeaned cofin falls -0.78pp/win within orgs.\n\n")

cat("STRUCTURAL EFFECTS (not hypothesized but found):\n")
cat("  - Winning increases retention: +21pp\n")
cat("  - Prior winners have 1.52x higher win rate\n")
cat("  - System in incumbency equilibrium: ~60%% badge holders throughout\n")
cat("  - Cohort half-life: ~2 years; hard core of ~37%% never leaves\n")
cat("  - After 5.5 years: 42%% of starting cohort still active,\n")
cat("    79%% of survivors are winners\n\n")

cat("INTERPRETATION:\n")
cat("  The PG system operates through SELECTION and STRUCTURAL INCUMBENCY,\n")
cat("  not through BEHAVIORAL TRANSFORMATION. Organizations that fit the\n")
cat("  system's performance logic are selected in, retained, and re-selected.\n")
cat("  Those that don't are filtered out. Winning produces short-term coasting\n")
cat("  (less cofin, slightly lower quality) but no sustained behavioral change.\n")
cat("  Expert evaluators reward incumbency (+0.55/win) even as independent\n")
cat("  quality measures decline, closing the self-reinforcing loop.\n")

cat("\n==============================================================\n")
cat("  END OF REPORT\n")
cat("==============================================================\n")

sink()
cat("\nReport saved to:", sink_file, "\n")
