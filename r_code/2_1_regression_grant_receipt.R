###############################################################################
# 3  Main regression analysis: grant-receipt logistic models
#
# Reads:  data/data_large/presi_variables_analysis.csv  (from 2_3)
###############################################################################

###############################################################################
# 0) Packages
###############################################################################

pacman::p_load(
  MASS, tidyr, purrr, tidyverse, stringi, stringr, readr,
  tibble, lubridate, margins, car, pscl, ggplot2, ggrepel,
  scales, Hmisc, naniar, xtable, dotwhisker, forcats, dplyr, broom, grid
)

###############################################################################
# 1) Load analysis dataset (built by 2_3_attach_scores_and_build_analysis_vars.R)
###############################################################################

presi_variables <- read_csv("data/data_large/presi_variables_analysis.csv",
                            show_col_types = FALSE)

###############################################################################
# 2) Exploratory Plots for Traditional Values
###############################################################################

# Age vs probability of traditional values (any trad_vals == 1)
presi_variables %>%
  filter(!is.na(target_age), target_age <= 95) %>%
  ggplot(aes(target_age, has_trad)) +
  geom_jitter(height = .02, width = 0, alpha = .12) +
  geom_smooth(method = "loess", formula = y ~ x,
              se = FALSE, span = .4, linewidth = 1.2) +
  scale_y_continuous(labels = percent_format(1), limits = c(0, 1)) +
  labs(x = "Age", y = "Probability of traditional values") +
  theme_minimal(base_size = 13)

# Trad share across competition index
presi_variables %>%
  filter(!is.na(competition_index), !is.na(has_trad)) %>%
  ggplot(aes(competition_index, has_trad)) +
  geom_jitter(height = .02, width = 0, alpha = .05, size = 0.6) +
  geom_smooth(
    method = "glm",
    method.args = list(family = "binomial"),
    se = FALSE,
    linewidth = 1.2
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    breaks  = seq(0, 0.25, by = 0.05)
  ) +
  coord_cartesian(ylim = c(0, 0.25)) +
  labs(
    x = "Semesters since 2017",
    y = "Probability of traditional values"
  ) +
  theme_minimal(base_size = 13)

###############################################################################
# 3) Main Logistic Regression: Effect of Application Quality
###############################################################################

# Build modelling subset (avoid global na.omit)
presi_model <- presi_variables %>%
  filter(!is.na(project_status)) %>%
  drop_na(
    gemini_quality,
    cofinancing_share,
    org_age,
    badge_president_foundation,
    badge_regional_grants,
    badge_top_project,
    org_website,
    target_age,
    target_disability,
    has_trad,
    direction,
    competition_index,
    region
    # has_won_before, cum_prior_projects, cum_prior_grants_m: no NAs (filled 0)
  )

hist(presi_model$gemini_quality)
hist(presi_model$cofinancing_share)
hist(presi_model$cum_prior_grants_m)
hist(presi_model$sum_requested_m)

Hmisc::describe(presi_model$gemini_quality)

# Shared formula components (prior-grant experience differs between models)
# NOTE: badge_no_reporting and badge_project_failure replaced by panel-correct
#       had_no_report_before and had_proj_failure_before (section 2b).
shared_rhs <- ~ gemini_quality +
    # Project financials
    sum_requested_m +
    cum_prior_grants_m +
    cofinancing_share +
    implementation_length_months +
    # Organisation characteristics
    org_age +
    org_website +
    # Badges (clean)
    badge_president_foundation +
    badge_regional_grants +
    badge_top_project +
    had_no_report_before +
    had_proj_failure_before +
    # Target population
    as.numeric(target_age) +
    target_disability +
    has_trad +
    # Fixed effects
    factor(direction) +
    competition_index +
    factor(region)

# Model A: binary prior-winner indicator
final_model_binary <- glm(
  update(shared_rhs, project_status ~ . + has_won_before),
  data   = presi_model,
  family = binomial
)

# Use model A as the default for downstream plots / bootstrap
final_model <- final_model_binary

cat("\n===== Model A: binary has_won_before =====\n")
summary(final_model_binary)
car::vif(final_model_binary)
print(pR2(final_model_binary))

# # Model B: cumulative count of prior wins (commented out to save compute)
# final_model_cumulative <- glm(
#   update(shared_rhs, project_status ~ . + cum_prior_projects),
#   data   = presi_model,
#   family = binomial
# )
# cat("\n===== Model B: cumulative cum_prior_projects =====\n")
# summary(final_model_cumulative)
# car::vif(final_model_cumulative)
# print(pR2(final_model_cumulative))

# ── Marginal effects (Model A) ──────────────────────────────────────────────
# margins() runtime is proportional to number of variables.  Computing AMEs
# for all ~100 coefficients (82 regions + 13 directions + key vars) takes
# ~40 min.  Splitting into substantive vars (~17) and direction FE (~13)
# avoids the 82-region Jacobian and cuts runtime to ~10 min.
#
# Further optimisation for M2 (8 performance cores): dispatch each variable
# as a separate margins() call via parallel::mclapply().  Each call handles
# one variable, so the Jacobian is trivially small; all cores fire at once.
# No package changes — mclapply is a base R parallel wrapper.

key_vars <- c(
  "gemini_quality", "sum_requested_m", "cum_prior_grants_m",
  "cofinancing_share", "implementation_length_months",
  "org_age", "org_website", "has_won_before",
  "badge_president_foundation", "badge_regional_grants", "badge_top_project",
  "had_no_report_before", "had_proj_failure_before",
  "target_age", "target_disability", "has_trad",
  "competition_index"
)

n_cores <- parallel::detectCores(logical = FALSE)   # physical cores on M2
cat(sprintf("\nComputing AMEs for key variables in parallel (%d cores)...\n", n_cores))
system.time({
  ame_list_key <- parallel::mclapply(
    key_vars,
    function(v) summary(margins::margins(final_model_binary, variables = v)),
    mc.cores = n_cores
  )
  s_a_key <- do.call(rbind, ame_list_key)
})
cat("\n--- AMEs Model A (key variables) ---\n")
print(s_a_key)

cat("\nComputing AMEs for direction FE (parallel per level)...\n")
system.time({
  s_a_dir <- summary(margins::margins(final_model_binary, variables = "direction"))
})
cat("\n--- AMEs Model A (direction) ---\n")
print(s_a_dir)

# Combine into single table for downstream use
s_a <- rbind(s_a_key, s_a_dir)

# Region AMEs (slow — ~30 min for 82 dummies; run separately if needed)
# s_a_reg <- summary(margins(final_model_binary, variables = "region"))
# s_a <- rbind(s_a, s_a_reg)

# Add case-level predictions (from model A)
presi_model_pred <- presi_model %>%
  mutate(
    pred_prob = predict(final_model, type = "response"),
    outcome = project_status
  )

# Jittered scatterplot
ggplot(presi_model_pred, aes(x = cofinancing_share, y = pred_prob)) +
  geom_point(alpha = 0.25, size = 0.5, shape = 20,
             position = position_jitter(width = 0.8, height = 0.002)) +
  geom_smooth(method = "loess", se = TRUE, color = "darkred", linewidth = 1.1) +
  geom_rug(aes(y = outcome), sides = "r", alpha = 0.5, length = unit(0.01, "npc"),
           outside = TRUE) +
  scale_x_continuous(labels = scales::comma_format(accuracy = 1),
                     breaks = seq(0, 100, 20)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 0.6)) +
  labs(
    x = "Co-financing share (%)",
    y = "Individual predicted Pr(win grant)",
    title = "Co-financing vs Predicted Grant Success (151k Cases)",
    subtitle = "Jittered points; loess smooth + 95% CI; right rug = observed wins/losses"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_line(color = "grey85")
  )

ggsave("graphs/cofin_scatter_cases_gemini25_20260214.png", width = 10, height = 6, dpi = 300)

###############################################################################
# 4) Parametric Bootstrap Plot for Application Quality Effect
###############################################################################

# 1) Grid for gemini_quality and average predicted probability
xr_all <- range(presi_model$gemini_quality, na.rm = TRUE)
grid_x <- seq(xr_all[1], xr_all[2], length.out = 150)

avg <- sapply(grid_x, function(x) {
  tmp <- presi_model
  tmp$gemini_quality <- x
  mean(predict(final_model, newdata = tmp, type = "response"), na.rm = TRUE)
})

avg_df <- data.frame(gemini_quality = grid_x, prob = avg)

# 2) 95% CI ribbon via parametric bootstrap of coefficients
set.seed(123)
B    <- 400
beta <- coef(final_model)
V    <- vcov(final_model)
X    <- model.matrix(final_model)
col_appl <- which(colnames(X) == "gemini_quality")

X0 <- X
X0[, col_appl] <- 0  # offset with gemini_quality set to 0

# Parallel bootstrap: each of B iterations is independent — dispatch across
# all M2 performance cores.  set.seed() per iteration ensures reproducibility.
boot_list <- parallel::mclapply(
  seq_len(B),
  function(i) {
    set.seed(123L + i)
    b     <- MASS::mvrnorm(1, beta, V)
    off   <- as.vector(X0 %*% b)
    slope <- b[col_appl]
    sapply(grid_x, function(x) mean(plogis(off + x * slope)))
  },
  mc.cores = parallel::detectCores(logical = FALSE)
)
boot <- do.call(cbind, boot_list)

avg_df$lo <- apply(boot, 1, quantile, 0.025)
avg_df$hi <- apply(boot, 1, quantile, 0.975)

# 3) Binned observed win rates (calibration points)
binned <- presi_model %>%
  transmute(x = gemini_quality, y = project_status) %>%
  filter(!is.na(x), !is.na(y)) %>%
  mutate(bin = ggplot2::cut_number(x, 20)) %>%
  group_by(bin) %>%
  summarise(x = mean(x), y = mean(y), n = n(), .groups = "drop")

# 4) Central 80% shading and a 40-point contrast
p10 <- quantile(presi_model$gemini_quality, 0.10, na.rm = TRUE)
p90 <- quantile(presi_model$gemini_quality, 0.90, na.rm = TRUE)

x0 <- as.numeric(p10)
x1 <- pmin(pmax(x0 + 40, xr_all[1]), xr_all[2])

p0 <- approx(avg_df$gemini_quality, avg_df$prob, xout = x0)$y
p1 <- approx(avg_df$gemini_quality, avg_df$prob, xout = x1)$y

ix0   <- which.min(abs(grid_x - x0))
ix1   <- which.min(abs(grid_x - x1))
delta <- boot[ix1, ] - boot[ix0, ]
ci40  <- quantile(delta, c(.025, .975))
lab   <- sprintf("+40 pts: %+0.1f pp (95%% CI %+0.1f, %+0.1f)",
                 100 * (p1 - p0), 100 * ci40[1], 100 * ci40[2])

# 5) Plot
ggplot(avg_df, aes(gemini_quality, prob)) +
  geom_rect(
    data = data.frame(xmin = p10, xmax = p90, ymin = -Inf, ymax = Inf),
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE, alpha = 0.06
  ) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.2) +
  geom_line(linewidth = 1) +
  geom_point(data = binned, aes(x = x, y = y, size = n),
             inherit.aes = FALSE, alpha = 0.6) +
  scale_size_continuous(range = c(1.5, 4), guide = "none") +
  geom_segment(aes(x = x0, xend = x1, y = p0, yend = p1), linetype = 2) +
  geom_point(aes(x = x0, y = p0)) +
  geom_point(aes(x = x1, y = p1)) +
  geom_label(aes(x = x1, y = p1, label = lab), hjust = 0, vjust = -0.5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Gemini quality",
    y = "Average predicted chance of winning",
    title = "Predicted probability of winning depending on Gemini quality",
    subtitle = "Solid: model average; points: binned observed win rates; ribbon: 95% parametric bootstrap",
    caption = "Shaded area = central 80% of Gemini quality"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

beta_appl <- coef(final_model)["gemini_quality"]
cat(sprintf("OR for +40 points in gemini_quality: %.3f\n", exp(40 * beta_appl)))

###############################################################################
# 5) Competition-level Lagged Crowding Variable (Alternative Spec)
###############################################################################

# Summarise number of applications per (direction, competition_index)
direction_competition_counts <- presi_variables %>%
  group_by(direction, competition_index) %>%
  summarise(n_apps_dir_competition = n(), .groups = "drop") %>%
  arrange(direction, competition_index) %>%
  group_by(direction) %>%
  mutate(n_apps_dir_competition_lag = lag(n_apps_dir_competition, 1)) %>%
  ungroup()

presi_variables_lagged <- presi_variables %>%
  left_join(
    direction_competition_counts %>%
      dplyr::select(direction, competition_index, n_apps_dir_competition_lag),
    by = c("direction", "competition_index")
  ) %>%
  mutate(
    n_apps_dir_competition_lag   = dplyr::if_else(is.na(n_apps_dir_competition_lag), 0L, n_apps_dir_competition_lag),
    n_apps_dir_competition_lag_k = n_apps_dir_competition_lag / 1000
  )

# Model A with lag (binary)
model_with_lag_binary <- glm(
  project_status ~
    # Application quality
    gemini_quality +
    # Project financials
    sum_requested_m +
    cum_prior_grants_m +
    cofinancing_share +
    implementation_length_months +
    # Organisation characteristics
    org_age +
    org_website +
    has_won_before +
    # Badges
    badge_president_foundation +
    badge_regional_grants +
    badge_top_project +
    had_no_report_before +
    had_proj_failure_before +
    # Target population
    as.numeric(target_age) +
    target_disability +
    has_trad +
    # Fixed effects
    factor(direction) +
    as.numeric(competition_index) +
    n_apps_dir_competition_lag_k +
    factor(region),
  family = binomial,
  data   = presi_variables_lagged
)

# Model B with lag (cumulative)
model_with_lag_cumulative <- glm(
  project_status ~
    # Application quality
    gemini_quality +
    # Project financials
    sum_requested_m +
    cum_prior_grants_m +
    cofinancing_share +
    implementation_length_months +
    # Organisation characteristics
    org_age +
    org_website +
    cum_prior_projects +
    # Badges
    badge_president_foundation +
    badge_regional_grants +
    badge_top_project +
    had_no_report_before +
    had_proj_failure_before +
    # Target population
    as.numeric(target_age) +
    target_disability +
    has_trad +
    # Fixed effects
    factor(direction) +
    as.numeric(competition_index) +
    n_apps_dir_competition_lag_k +
    factor(region),
  family = binomial,
  data   = presi_variables_lagged
)

cat("\n===== Model A with lag (binary) =====\n")
broom::tidy(model_with_lag_binary) %>%
  dplyr::filter(!str_detect(term, "factor\\(region\\)")) %>%
  print(n = 100)

cat("\n===== Model B with lag (cumulative) =====\n")
broom::tidy(model_with_lag_cumulative) %>%
  dplyr::filter(!str_detect(term, "factor\\(region\\)")) %>%
  print(n = 100)

###############################################################################
# 6) Export Subset of Trad Vals with Websites (for external scraping etc.)
###############################################################################

# Build a subset where: trad-values present, org has website, and direction is not
# "Сохранение исторической памяти".
presi_subset <- presi_variables %>%
  filter(
    has_trad == 1,
    org_website == 1,
    as.character(direction) != "Сохранение исторической памяти"
  ) %>%
  mutate(href = str_trim(href))

# Website lookup table from contacts
contacts_web <- read_csv("data/data_large/application_contacts_backup.csv") %>%
  mutate(
    href        = str_trim(href),
    website_raw = str_squish(str_extract(details, "(?<=Веб-сайт:).*")),
    website     = str_extract(
      website_raw,
      regex("(https?://[^\\s,;\\)\\]]+|www\\.[^\\s,;\\)\\]]+|vk\\.com/[^\\s,;\\)\\]]+)", ignore_case = TRUE)
    ),
    website = case_when(
      is.na(website) ~ NA_character_,
      str_detect(website, "^https?://") ~ website,
      TRUE ~ str_c("https://", website)
    )
  ) %>%
  group_by(href) %>%
  slice(1) %>%
  ungroup()

presi_subset <- presi_subset %>%
  left_join(contacts_web, by = "href") %>%
  arrange(
    desc(coalesce(str_detect(website, regex("\\bvk\\.com", ignore_case = TRUE)), FALSE)),
    website
  )

write_csv(presi_subset, "data/data_large/presi_subset_iskren.csv")

###############################################################################
# 7) Standard errors and SDs over time
###############################################################################

# Lookup table: which competition_index corresponds to which year
comp_year <- presi_model %>%
  distinct(competition_index, year_extracted)

# Compute SEs for gemini_quality and cofinancing_share by competition_index
# Parallelised: one core per competition round (M2 has 8 performance cores)
comp_indices <- sort(unique(presi_model$competition_index))
se_list <- parallel::mclapply(
  comp_indices,
  function(ci) {
    dat <- presi_model[presi_model$competition_index == ci, ]
    tryCatch({
      m <- glm(
        project_status ~
          gemini_quality +
          cofinancing_share +
          cum_prior_grants_m +
          org_age +
          org_website +
          has_won_before +
          badge_president_foundation +
          badge_regional_grants +
          badge_top_project +
          had_no_report_before +
          had_proj_failure_before +
          as.numeric(target_age) +
          target_disability +
          has_trad +
          factor(direction) +
          factor(region),
        data   = dat,
        family = binomial
      )
      broom::tidy(m) %>%
        dplyr::filter(term %in% c("gemini_quality", "cofinancing_share")) %>%
        dplyr::select(term, std.error) %>%
        dplyr::mutate(competition_index = ci)
    }, error = function(e) {
      data.frame(term = NA, std.error = NA, competition_index = ci)
    })
  },
  mc.cores = parallel::detectCores(logical = FALSE)
)
se_time <- dplyr::bind_rows(se_list) %>%
  dplyr::left_join(comp_year, by = "competition_index")

# Plot SE of gemini_quality over years
se_time %>%
  filter(term == "gemini_quality") %>%
  ggplot(aes(x = year_extracted, y = std.error, group = 1)) +
  geom_line() +
  geom_point() +
  labs(
    x     = "Year",
    y     = "Standard error of gemini_quality",
    title = "SE of Gemini quality coefficient over time"
  ) +
  theme_minimal()

# Plot SE of cofinancing_share over years
se_time %>%
  filter(term == "cofinancing_share") %>%
  ggplot(aes(x = year_extracted, y = std.error, group = 1)) +
  geom_line() +
  geom_point() +
  labs(
    x     = "Year",
    y     = "Standard error of cofinancing_share",
    title = "SE of cofinancing coefficient over time"
  ) +
  theme_minimal()

# Standard deviations by competition_index
sd_time <- presi_model %>%
  group_by(competition_index) %>%
  summarise(
    sd_gemini_quality       = sd(gemini_quality, na.rm = TRUE),
    sd_cofinancing_share  = sd(cofinancing_share, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(comp_year, by = "competition_index")

# Plot SD of gemini_quality over years
ggplot(sd_time, aes(x = year_extracted, y = sd_gemini_quality, group = 1)) +
  geom_line() +
  geom_point() +
  labs(
    x     = "Year",
    y     = "SD of gemini_quality",
    title = "Standard deviation of Gemini quality over time"
  ) +
  theme_minimal()

# Plot SD of cofinancing_share over years
ggplot(sd_time, aes(x = year_extracted, y = sd_cofinancing_share, group = 1)) +
  geom_line() +
  geom_point() +
  labs(
    x     = "Year",
    y     = "SD of cofinancing_share",
    title = "Standard deviation of cofinancing share over time"
  ) +
  theme_minimal()

###############################################################################
# 8) Means over time
###############################################################################

means_time <- presi_model %>%
  group_by(year_extracted) %>%
  summarise(
    mean_gemini_quality      = mean(gemini_quality, na.rm = TRUE),
    mean_cofinancing_share = mean(cofinancing_share, na.rm = TRUE),
    .groups = "drop"
  )

means_long <- means_time %>%
  pivot_longer(
    cols      = c(mean_gemini_quality, mean_cofinancing_share),
    names_to  = "variable",
    values_to = "mean_value"
  )

# Plot both over time
quality_cofin_time_plot <- ggplot(
  means_long,
  aes(x = year_extracted, y = mean_value, color = variable, group = variable)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c(
      mean_gemini_quality = "#0072B2",
      mean_cofinancing_share = "#D55E00"
    ),
    labels = c(
      mean_gemini_quality = "Gemini quality",
      mean_cofinancing_share = "Cofinancing share"
    )
  ) +
  labs(
    x     = "Year",
    y     = "Mean value",
    color = "Variable",
    title = "Gemini quality and cofinancing share over time"
  ) +
  theme_minimal()

quality_cofin_time_plot

ggsave(
  "graphs/gemini_quality_cofinancing_time_20260214.png",
  plot = quality_cofin_time_plot,
  width = 9,
  height = 5.5,
  dpi = 300
)

# Application quality over time
ggplot(means_time, aes(x = year_extracted, y = mean_gemini_quality)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Year",
    y = "Mean Gemini quality",
    title = "Gemini quality over time"
  ) +
  theme_minimal()

# Cofinancing share over time
ggplot(means_time, aes(x = year_extracted, y = mean_cofinancing_share)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Year",
    y = "Mean cofinancing share",
    title = "Cofinancing share over time"
  ) +
  theme_minimal()

###############################################################################
# 9) AME Graphs
###############################################################################

ame <- as.data.frame(s_a)
head(ame)

# Columns: factor, AME, SE, z, p, lower, upper
ame <- ame %>%
  mutate(
    term = factor(factor),
    term = fct_reorder(term, AME)
  )

ame_small <- ame %>%
  filter(factor %in% c("gemini_quality", "cofinancing_share", "has_trad")) %>%
  mutate(
    sig = if_else(p < 0.05, "p < 0.05", "n.s."),
    term = factor(factor, levels = c("gemini_quality", "cofinancing_share", "has_trad")),
    label = dplyr::recode(term,
      gemini_quality      = "Gemini quality (0–100)",
      cofinancing_share = "Share of co-financing (%)",
      has_trad         = "Traditional values, (0-1)"
    )
  )

ame_small_pp <- ame_small %>%
  mutate(
    AME_pp   = AME   * 100,
    lower_pp = lower * 100,
    upper_pp = upper * 100,
    label = forcats::fct_reorder(label, AME_pp)
  )

ggplot(ame_small_pp, aes(x = AME_pp, y = label)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(xmin = lower_pp, xmax = upper_pp), size = 0.6) +
  labs(
    x = "Average marginal effect on winning the grant, %",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    panel.grid.minor  = element_blank(),
    panel.grid.major.x = element_line(colour = "grey90")
  )

sd_info <- presi_model %>%
  summarise(
    gemini_quality      = sd(gemini_quality,      na.rm = TRUE),
    cofinancing_share = sd(cofinancing_share, na.rm = TRUE)
  ) %>%
  tidyr::pivot_longer(everything(),
                      names_to = "factor", values_to = "sd_x")

# Add 1 for the dummy (0 → 1 change)
sd_info <- bind_rows(
  sd_info,
  tibble(factor = "has_trad", sd_x = 1)
)

ame_scaled <- ame %>%
  filter(factor %in% c("gemini_quality", "cofinancing_share", "has_trad")) %>%
  left_join(sd_info, by = "factor") %>%
  mutate(
    sig = if_else(p < 0.05, "p < 0.05", "n.s."),
    AME_pp   = AME   * sd_x * 100,
    lower_pp = lower * sd_x * 100,
    upper_pp = upper * sd_x * 100,
    label = dplyr::recode(factor,
      gemini_quality      = "Application quality (1 SD)",
      cofinancing_share = "Share of co-financing (%)",
      has_trad          = "Traditional values, (0–1)"
    ),
    label = fct_reorder(label, AME_pp)
  )

ame_scaled_stars <- ame_scaled %>%
  mutate(
    stars = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      TRUE      ~ ""
    ),
    label_star = paste0(label, " ", stars),
    label_star = fct_reorder(label_star, AME_pp)
  )

ggplot(ame_scaled_stars, aes(x = AME_pp, y = label_star)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(xmin = lower_pp, xmax = upper_pp), size = 0.6) +
  labs(
    x = "Average marginal effect of a 1 SD increase on winning the grant, %",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor  = element_blank(),
    panel.grid.major.x = element_line(colour = "grey90")
  )

###############################################################################
# 10) Traditional values by age and period
###############################################################################

tv_age <- presi_variables %>%
  filter(
    target_age >= 0, target_age <= 70,
    !is.na(has_trad),
    !is.na(year_extracted)
  ) %>%
  mutate(
    period = if_else(year_extracted < 2022,
                     "Before 2022",
                     "2022 and later")
  )

ggplot(tv_age, aes(x = target_age, y = has_trad, colour = period)) +
  geom_smooth(
    method = "loess",
    span   = 0.6,
    se     = FALSE
  ) +
  scale_colour_manual(
    values = c(
      "Before 2022"    = "#0072B2",
      "2022 and later" = "#D55E00"
    )
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0, 0.60, by = 0.05)
  ) +
  coord_cartesian(ylim = c(0, 0.60)) +
  labs(
    x = "Target age",
    y = "Share of applications with traditional values",
    colour = NULL,
    title = "Traditional values vs target age, by period"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom"
  )

big_requests <- presi_variables %>%
  filter(sum_requested > 1e9) %>%
  arrange(desc(sum_requested))

big_requests
dir.create("data/data_results", showWarnings = FALSE, recursive = TRUE)
write_csv(big_requests, "data/data_results/big_requests_gemini25_20260214.csv")

###############################################################################
# 11) Canonical English labels and traditional-values figures
###############################################################################

presi_variables <- presi_variables %>%
  mutate(date = as.Date(date))

# Canonical English labels (match regression table / Overleaf)
presi_variables <- presi_variables %>%
  mutate(
    direction = str_squish(as.character(direction)),
    direction_en = dplyr::recode(
      direction,
      "Развитие институтов гражданского общества - долгосрочный проект" =
        "Civil Society",
      "Укрепление межнационального и межрелигиозного согласия" =
        "Interethnic / Interreligious Consensus",
      "Охрана окружающей среды и защита животных" =
        "Environmental Protection",
      "Развитие институтов гражданского общества" =
        "Civil Society",
      "Поддержка молодежных проектов" =
        "Youth Projects",
      "Поддержка семьи, материнства, отцовства и детства" =
        "Family Support",
      "Поддержка проектов в области науки, образования, просвещения" =
        "Science & Education",
      "Сохранение исторической памяти" =
        "Historical Memory",
      "Социальное обслуживание, социальная поддержка и защита граждан" =
        "Social Services",
      "Охрана здоровья граждан, пропаганда здорового образа жизни" =
        "Public Health",
      "Защита прав и свобод человека и гражданина" =
        "Protect Human Rights",
      "Защита прав и свобод человека и гражданина, в том числе защита прав заключенных" =
        "Protect Human Rights",
      "Развитие общественной дипломатии и поддержка соотечественников" =
        "Diplomacy / Supporting Compatriots Abroad",
      "Поддержка проектов в области культуры и искусства" =
        "Culture",
      "Выявление и поддержка молодых талантов в области культуры и искусства" =
        "Young Talent in Arts",
      .default = direction
    )
  )

# FIGURE 1: Traditional values prevalence over time
tv_time <- presi_variables %>%
  filter(!is.na(date), !is.na(has_trad)) %>%
  group_by(date) %>%
  summarise(
    pct_has_trad = mean(has_trad == 1, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(date)

ggplot(tv_time, aes(x = date, y = pct_has_trad, group = 1)) +
  geom_line(linewidth = 1.2, colour = "black") +
  geom_point(size = 2.5, colour = "black") +
  geom_smooth(
    method = "loess",
    span   = 0.6,
    se     = FALSE,
    linetype = "dashed",
    linewidth = 0.9,
    colour = "grey40"
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0.2, 0.65)
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  labs(
    x = "Competition year",
    y = "Share with traditional values (has_trad = 1)",
    title = "Traditional-values prevalence over time",
    subtitle = "Solid line = competition means; dashed = loess trend"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# FIGURE 2: Traditional values prevalence by direction
tv_by_dir <- presi_variables %>%
  filter(!is.na(direction_en), !is.na(has_trad)) %>%
  group_by(direction_en) %>%
  summarise(
    pct_has_trad = mean(has_trad == 1, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    direction_en = fct_reorder(direction_en, pct_has_trad)
  )

ggplot(tv_by_dir,
       aes(x = direction_en, y = pct_has_trad)) +
  geom_col(width = 0.85, fill = "grey30") +
  coord_flip() +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, max(tv_by_dir$pct_has_trad, na.rm = TRUE) * 1.1)
  ) +
  labs(
    x = "Direction",
    y = "Share with significant traditional values",
    title = "Traditional Values Usage Over Directions"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank()
  )

###############################################################################
# 12) Misc exports
###############################################################################

no_cof <- presi_model %>%
  subset(project_status == 1 & cofinancing_share == 0)
dir.create("data/data_results", showWarnings = FALSE, recursive = TRUE)
write_csv(no_cof, "data/data_results/no_cofinancing_winners_gemini25_20260214.csv")

###############################################################################
# APPENDIX — Religious-organisation extension (commented out; see
#   3_reciept_regression_relig_20260316.R for the full standalone version)
#
# is_religious is already built in section 2b and lives in presi_model.
# Uncomment the block below to run Models C / D alongside A / B.
###############################################################################

# # ── Shared RHS with religious indicator ──────────────────────────────────────
# shared_rhs_relig <- update(shared_rhs, ~ . + is_religious)
#
# # Model C: binary prior-winner + is_religious
# final_model_binary_relig <- glm(
#   update(shared_rhs_relig, project_status ~ . + has_won_before),
#   data   = presi_model,
#   family = binomial
# )
#
# # Model D: cumulative prior wins + is_religious
# final_model_cumulative_relig <- glm(
#   update(shared_rhs_relig, project_status ~ . + cum_prior_projects),
#   data   = presi_model,
#   family = binomial
# )
#
# cat("\n===== Model C: binary + is_religious =====\n")
# summary(final_model_binary_relig)
# print(pR2(final_model_binary_relig))
# cat(sprintf("OR for is_religious: %.3f\n",
#             exp(coef(final_model_binary_relig)["is_religious"])))
#
# cat("\n===== Model D: cumulative + is_religious =====\n")
# summary(final_model_cumulative_relig)
# print(pR2(final_model_cumulative_relig))
# cat(sprintf("OR for is_religious: %.3f\n",
#             exp(coef(final_model_cumulative_relig)["is_religious"])))
#
# # AMEs
# s_c <- summary(margins(final_model_binary_relig))
# cat("\n--- AMEs Model C (selected) ---\n")
# print(s_c[s_c$factor %in% c("has_won_before", "is_religious",
#                               "cum_prior_grants_m", "sum_requested_m"), ])
#
# s_d <- summary(margins(final_model_cumulative_relig))
# cat("\n--- AMEs Model D (selected) ---\n")
# print(s_d[s_d$factor %in% c("cum_prior_projects", "is_religious",
#                               "cum_prior_grants_m", "sum_requested_m"), ])
#
# # ATT: AME evaluated only over religious-org rows
# att_c <- summary(margins(final_model_binary_relig,
#                          variables = "is_religious",
#                          data      = subset(presi_model, is_religious == 1)))
# cat(sprintf("\nATT (Model C) = %.2f pp  (95%% CI: %.2f, %.2f pp)  p = %.4f\n",
#             att_c$AME * 100, att_c$lower * 100, att_c$upper * 100, att_c$p))

###############################################################################
# 13) Time-trend plots: application quality & co-financing share
#    (integrated from make_time_plots_20260220.R)
###############################################################################

plot_data <- presi_variables %>%
  filter(!is.na(gemini_quality), !is.na(cofinancing_share),
         is.finite(cofinancing_share))

dir.create("graphs", showWarnings = FALSE)

# ── 8a. Means over time ─────────────────────────────────────────────────────
means_time <- plot_data %>%
  group_by(year_extracted) %>%
  summarise(
    mean_appl_quality = mean(gemini_quality,    na.rm = TRUE),
    mean_cofinancing  = mean(cofinancing_share, na.rm = TRUE),
    sd_appl_quality   = sd(gemini_quality,      na.rm = TRUE),
    sd_cofinancing    = sd(cofinancing_share,   na.rm = TRUE),
    n                 = n(),
    .groups = "drop"
  )

means_long <- means_time %>%
  pivot_longer(
    cols      = c(mean_appl_quality, mean_cofinancing),
    names_to  = "variable",
    values_to = "mean_value"
  )

p_means <- ggplot(means_long,
       aes(x = year_extracted, y = mean_value,
           colour = variable, shape = variable, group = variable)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_colour_manual(
    values = c(mean_appl_quality = "#0072B2", mean_cofinancing = "#D55E00"),
    labels = c(mean_appl_quality = "Application quality (Gemini)",
               mean_cofinancing  = "Co-financing share (%)")
  ) +
  scale_shape_manual(
    values = c(mean_appl_quality = 16, mean_cofinancing = 17),
    labels = c(mean_appl_quality = "Application quality (Gemini)",
               mean_cofinancing  = "Co-financing share (%)")
  ) +
  scale_x_continuous(breaks = seq(2017, 2025, 1)) +
  labs(
    x      = "Year",
    y      = "Mean value",
    colour = NULL, shape = NULL,
    title  = "Mean application quality and co-financing share over time"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position  = "bottom",
        panel.grid.minor = element_blank(),
        axis.text.x      = element_text(angle = 45, hjust = 1))

ggsave("graphs/means_quality_cofin_over_time_gemini25_20260220.png",
       p_means, width = 9, height = 5.5, dpi = 300)
cat("Saved: means plot\n")

# ── 8b. SDs over time (dual axis) ───────────────────────────────────────────
sf <- mean(means_time$sd_appl_quality) / mean(means_time$sd_cofinancing)

p_sd <- ggplot(means_time, aes(x = year_extracted)) +
  geom_line(aes(y = sd_appl_quality,       colour = "Application quality (Gemini)"),
            linewidth = 1) +
  geom_point(aes(y = sd_appl_quality,      colour = "Application quality (Gemini)"),
             size = 2.5, shape = 16) +
  geom_line(aes(y = sd_cofinancing * sf,   colour = "Co-financing share (%)"),
            linewidth = 1, linetype = "dashed") +
  geom_point(aes(y = sd_cofinancing * sf,  colour = "Co-financing share (%)"),
             size = 2.5, shape = 17) +
  scale_y_continuous(
    name     = "SD — Application quality (Gemini)",
    sec.axis = sec_axis(~ . / sf,
                        name   = "SD — Co-financing share (%)",
                        labels = scales::number_format(accuracy = 0.1))
  ) +
  scale_colour_manual(
    values = c("Application quality (Gemini)" = "#0072B2",
               "Co-financing share (%)"       = "#D55E00")
  ) +
  scale_x_continuous(breaks = seq(2017, 2025, 1)) +
  labs(
    x        = "Year",
    colour   = NULL,
    title    = "Standard deviation of application quality and co-financing share over time",
    subtitle = "Solid / circles = quality (left axis)     Dashed / triangles = co-financing (right axis)"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position    = "bottom",
        panel.grid.minor   = element_blank(),
        axis.text.x        = element_text(angle = 45, hjust = 1),
        axis.title.y.left  = element_text(colour = "#0072B2"),
        axis.title.y.right = element_text(colour = "#D55E00"))

ggsave("graphs/sd_quality_cofin_over_time_gemini25_20260220.png",
       p_sd, width = 9, height = 5.5, dpi = 300)
cat("Saved: SD plot\n")

# ── 8c. Coefficient of Variation over time ──────────────────────────────────
cv_time <- means_time %>%
  mutate(
    cv_quality     = sd_appl_quality / mean_appl_quality,
    cv_cofinancing = sd_cofinancing  / mean_cofinancing
  )

cat("\nCV table (SD / mean):\n")
print(cv_time %>% select(year_extracted, cv_quality, cv_cofinancing), n = 20)

cv_long <- cv_time %>%
  select(year_extracted, cv_quality, cv_cofinancing) %>%
  pivot_longer(c(cv_quality, cv_cofinancing),
               names_to = "variable", values_to = "cv")

p_cv <- ggplot(cv_long,
       aes(x = year_extracted, y = cv, colour = variable, group = variable)) +
  geom_line(linewidth = 1) +
  geom_point(aes(shape = variable), size = 2.5) +
  scale_colour_manual(
    values = c(cv_quality = "#0072B2", cv_cofinancing = "#D55E00"),
    labels = c(cv_quality = "Application quality (Gemini)",
               cv_cofinancing = "Co-financing share (%)")
  ) +
  scale_shape_manual(
    values = c(cv_quality = 16, cv_cofinancing = 17),
    labels = c(cv_quality = "Application quality (Gemini)",
               cv_cofinancing = "Co-financing share (%)")
  ) +
  scale_x_continuous(breaks = seq(2017, 2025, 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x        = "Year",
    y        = "Coefficient of variation (SD / mean)",
    colour   = NULL,
    shape    = NULL,
    title    = "Relative dispersion of application quality and co-financing share over time",
    subtitle = "CV = SD / mean — both variables converge, co-financing much more so"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position  = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(angle = 45, hjust = 1)
  )

ggsave("graphs/cv_quality_cofin_over_time_gemini25_20260220.png",
       p_cv, width = 9, height = 5.5, dpi = 300)
cat("Saved: CV plot\n")

cat("\nMeans table:\n"); print(means_time, n = 20)

###############################################################################
# 14) Strategy B robustness: time-window badge variables (k >= 2 rounds)
#
# The badge_no_reporting text reads "no MoJ report since 1 Jan 2023" — a
# platform-wide cutoff, not an org-specific timestamp. The _v2 variables
# require that the most recent prior win was at least 2 competition rounds ago,
# which excludes the most ambiguously timed badge assignments.
#
# This block re-runs Models A and B with the _v2 badge variables and reports
# AMEs for the four badge coefficients side by side. Intended for the appendix.
###############################################################################

shared_rhs_v2 <- ~ gemini_quality +
  sum_requested_m +
  cum_prior_grants_m +
  cofinancing_share +
  implementation_length_months +
  org_age +
  org_website +
  badge_president_foundation +
  badge_regional_grants +
  badge_top_project +
  had_no_report_before_v2 +
  had_proj_failure_before_v2 +
  as.numeric(target_age) +
  target_disability +
  has_trad +
  factor(direction) +
  competition_index +
  factor(region)

cat("\n===== Strategy B — Model A_v2: binary + time-window badges =====\n")
final_model_binary_v2 <- glm(
  update(shared_rhs_v2, project_status ~ . + has_won_before),
  data   = presi_model,
  family = binomial
)
broom::tidy(final_model_binary_v2) %>%
  dplyr::filter(str_detect(term, "had_|has_won")) %>%
  print()

key_vars_v2 <- c(
  "has_won_before",
  "had_no_report_before_v2", "had_proj_failure_before_v2"
)
# Parallel AME for v2 badge check
ame_list_v2 <- parallel::mclapply(
  key_vars_v2,
  function(v) summary(margins::margins(final_model_binary_v2, variables = v)),
  mc.cores = parallel::detectCores(logical = FALSE)
)
s_v2 <- do.call(rbind, ame_list_v2)
cat("\n--- AMEs Model A_v2 (badge variables) ---\n")
print(s_v2)

# Comparison table: main vs v2 AMEs for the four badge-related variables
main_badge <- s_a_key[s_a_key$factor %in%
  c("has_won_before", "had_no_report_before", "had_proj_failure_before"), ]
v2_badge   <- s_v2[s_v2$factor %in%
  c("has_won_before", "had_no_report_before_v2", "had_proj_failure_before_v2"), ]

cat("\n=== Badge AME comparison: main (v1) vs time-window (v2) ===\n")
cat("Main model badge AMEs:\n")
print(main_badge[, c("factor", "AME", "SE", "p")])
cat("Time-window (k>=2) badge AMEs:\n")
print(v2_badge[, c("factor", "AME", "SE", "p")])

# Export comparison for LaTeX
badge_comparison <- dplyr::bind_rows(
  main_badge %>%
    select(factor, AME, SE, lower, upper, p) %>%
    mutate(specification = "Main (v1)"),
  v2_badge %>%
    select(factor, AME, SE, lower, upper, p) %>%
    mutate(specification = "Time-window k>=2 (v2)")
)
dir.create("data/data_results", showWarnings = FALSE, recursive = TRUE)
write_csv(badge_comparison,
          "data/data_results/badge_ame_comparison_v1_v2.csv")
cat("Saved badge AME comparison to data/data_results/badge_ame_comparison_v1_v2.csv\n")

###############################################################################
# END OF SCRIPT
###############################################################################

