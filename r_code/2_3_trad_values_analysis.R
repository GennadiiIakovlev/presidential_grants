



######################################################################
## 1. Logit models: all years and Post-2022
trad_all <- glm(
  has_trad ~
    as.numeric(target_age) +
    target_disability +
    org_age +
    org_website +
    badge_president_foundation +
    has_won_before +
    badge_regional_grants +
    badge_top_project +
    badge_project_failure +
    badge_no_reporting +
    cum_prior_projects +
    direction+
    # region+
    competition_index,          # labelled "Semester Since 2017"
  data   = presi_model,
  family = binomial
)

trad_post <- update(
  trad_all,
  data = subset(presi_model, date > 2021)   # <- adapt variable name if needed
)

## 2. Average marginal effects with robust SEs
library(marginaleffects)  # for avg_slopes (AMEs)

ame_all  <- avg_slopes(trad_all,  vcov = "HC3")
ame_post <- avg_slopes(trad_post, vcov = "HC3")

summary(ame_all)
summary(ame_post)

ame_all
## 3. Put AMEs and p-values into a tidy data frame
ame_all_df <- as.data.frame(ame_all)  |>
  select(term, estimate_all  = estimate, p_all  = p.value)

ame_post_df <- as.data.frame(ame_post) |>
  select(term, estimate_post = estimate, p_post = p.value)

## 4. Nice variable labels in the order you want
nice_order <- c(
  "as.numeric(target_age)",
  "target_disability",
  "org_age",
  "org_website",
  "badge_president_foundation",
  "has_won_before",
  "badge_regional_grants",
  "badge_top_project",
  "badge_project_failure",
  "badge_no_reporting",
  "cum_prior_projects",
  "competition_index"
)

nice_labels <- c(
  "as.numeric(target_age)"       = "Target Age",
  "target_disability"            = "Target Disability",
  "org_age"                      = "Org Age (years)",
  "org_website"                  = "Org Website",
  "badge_president_foundation"   = "President Foundation",
  "has_won_before"               = "Won Before (binary)",
  "badge_regional_grants"        = "Regional Grants",
  "badge_top_project"            = "Top Project",
  "badge_project_failure"        = "Project Failure",
  "badge_no_reporting"           = "No Reporting",
  "cum_prior_projects"           = "Cumulative prior wins ($n$)",
  "competition_index"            = "Semester Since 2017"
)

## 5. Function to add significance stars and format numbers
fmt_star <- function(est, p) {
  stars <- dplyr::case_when(
    p < 0.001 ~ "\\textsuperscript{***}",
    p < 0.01  ~ "\\textsuperscript{**}",
    p < 0.05  ~ "\\textsuperscript{*}",
    TRUE      ~ ""
  )
  sprintf("%0.2f%s", est, stars)
}

## 6. Merge AMEs and build the table contents
tab_df <- ame_all_df |>
  inner_join(ame_post_df, by = "term") |>
  filter(term %in% nice_order) |>
  mutate(
    Variable  = nice_labels[term],
    `All years`  = fmt_star(estimate_all,  p_all),
    `Post-2022`  = fmt_star(estimate_post, p_post),
    order = match(term, nice_order)
  ) |>
  arrange(order) |>
  select(Variable, `All years`, `Post-2022`)

## 7. Model fit: pseudo-R^2 (ML and CU)
r2_all  <- pscl::pR2(trad_all)
r2_post <- pscl::pR2(trad_post)

r2ML_all  <- unname(r2_all["r2ML"])
r2ML_post <- unname(r2_post["r2ML"])
r2CU_all  <- unname(r2_all["r2CU"])
r2CU_post <- unname(r2_post["r2CU"])

fit_line <- sprintf(
  "\\textit{Model fit:} pseudo-$R^{2}_{\\text{ML}} = %.3f$ (all) / %.3f (post); pseudo-$R^{2}_{\\text{CU}} = %.3f$ (all) / %.3f (post).",
  r2ML_all, r2ML_post, r2CU_all, r2CU_post
)

## 8. Produce LaTeX table (booktabs style)

kable(
  tab_df,
  format   = "latex",
  booktabs = TRUE,
  align    = c("l","r","r"),
  caption  = "Average marginal effects on the likelihood of using traditional values in an application",
  escape   = FALSE
) |>
  kable_styling(latex_options = c("hold_position")) |>
  pack_rows("Target",                        1,  2, italic = TRUE) |>
  pack_rows("Organisation characteristics",  3,  4, italic = TRUE) |>
  pack_rows("Green badges",                  5,  8, italic = TRUE) |>
  pack_rows("Red badges",                    9, 10, italic = TRUE) |>
  pack_rows("Experience / Funding",         11, 12, italic = TRUE) |>
  footnote(
    general = c(
      fit_line,
      "Robust standard errors (not shown) underpin the significance levels: \\textsuperscript{***}$p<0.001$, \\textsuperscript{**}$p<0.01$, \\textsuperscript{*}$p<0.05$."
    ),
    threeparttable   = TRUE,
    footnote_as_chunk = TRUE,
    escape = FALSE
  )

library(marginaleffects)  # for avg_slopes (AMEs)
library(dplyr)
library(pscl)            # for pseudo-R^2
library(knitr)
library(kableExtra)

