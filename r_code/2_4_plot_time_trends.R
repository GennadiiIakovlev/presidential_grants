suppressPackageStartupMessages({
  library(tidyverse); library(scales)
})

extract_href_key <- function(x) {
  x <- as.character(x); x <- stringr::str_trim(x); x <- stringr::str_to_lower(x)
  x <- stringr::str_replace(x, "^https?://[^/]+", "")
  x <- stringr::str_replace(x, "&.*$", "")
  id <- stringr::str_match(x, "item[?]id=([0-9a-f-]{36})")[, 2]
  key <- ifelse(!is.na(id), paste0("/public/application/item?id=", id), x)
  stringr::str_sub(key, 1, 120)
}

score_files <- list(
  "data/data_large/quality_scores_2026-02-14.csv",
  "data/data_large/patch1646_scores_2026-02-15.csv"
)

gemini_scores <- purrr::map(score_files, readr::read_csv, show_col_types = FALSE) %>%
  dplyr::bind_rows() %>%
  mutate(
    href_key       = extract_href_key(href),
    relevance      = suppressWarnings(as.numeric(relevance)),
    coherence      = suppressWarnings(as.numeric(coherence)),
    budget_realism = suppressWarnings(as.numeric(budget_realism)),
    scale          = suppressWarnings(as.numeric(scale)),
    appl_quality   = rowMeans(cbind(relevance, coherence, budget_realism, scale), na.rm = TRUE)
  ) %>%
  filter(!is.na(href_key), href_key != "") %>%
  arrange(href_key, desc(appl_quality)) %>%
  distinct(href_key, .keep_all = TRUE) %>%
  select(href_key, appl_quality)

pv <- read.csv("data/data_large/presi_variables_imputed.csv") %>%
  filter(!str_detect(competition, stringr::fixed("Специальный", ignore_case = TRUE))) %>%
  mutate(href_key = extract_href_key(href)) %>%
  select(-any_of("appl_quality")) %>%
  left_join(gemini_scores, by = "href_key") %>%
  mutate(
    year_extracted = as.numeric(stringr::str_extract(competition, "[0-9]{4}"))
  ) %>%
  filter(!is.na(appl_quality), !is.na(cofinancing_share),
         is.finite(cofinancing_share))

dir.create("graphs", showWarnings = FALSE)

# ── 1a. MEANS over time (clean) ───────────────────────────────────────────────
means_time <- pv %>%
  group_by(year_extracted) %>%
  summarise(
    mean_appl_quality = mean(appl_quality,      na.rm = TRUE),
    mean_cofinancing  = mean(cofinancing_share, na.rm = TRUE),
    sd_appl_quality   = sd(appl_quality,        na.rm = TRUE),
    sd_cofinancing    = sd(cofinancing_share,   na.rm = TRUE),
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

# ── 1b. SDs over time (dual axis — scales differ too much for one axis) ────────
sd_long <- means_time %>%
  pivot_longer(
    cols      = c(sd_appl_quality, sd_cofinancing),
    names_to  = "variable",
    values_to = "sd_value"
  )

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

# ── 2. Coefficient of Variation over time (SD / mean, same axis) ─────────────
cv_time <- pv %>%
  group_by(year_extracted) %>%
  summarise(
    sd_appl_quality = sd(appl_quality,      na.rm = TRUE),
    sd_cofinancing  = sd(cofinancing_share, na.rm = TRUE),
    mean_appl_quality = mean(appl_quality,      na.rm = TRUE),
    mean_cofinancing  = mean(cofinancing_share, na.rm = TRUE),
    n               = n(),
    .groups = "drop"
  ) %>%
  mutate(
    cv_quality     = sd_appl_quality / mean_appl_quality,
    cv_cofinancing = sd_cofinancing  / mean_cofinancing
  )

cat("\nCV table (SD / mean):\n"); print(cv_time %>% select(year_extracted, cv_quality, cv_cofinancing), n = 20)

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
