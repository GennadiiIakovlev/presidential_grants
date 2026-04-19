# Replication code

Scripts for "Governing Civil Society at a Distance: Authoritarian
Legitimation, Performance and Technocracy in Russia".

Scripts are grouped by pipeline stage:

- `1_x` — data collection and preparation
- `2_x` — analysis (paper tables/figures)
- `3_x` — validation (appendix figures and statistics)

Within each group, numbers reflect execution order. All paths are
relative to the repository root, so run scripts from there (not from
inside `r_code/`).

## 1. Data collection and preparation

| Script | Purpose |
|--------|---------|
| `1_1_scrape_pg_applications.R`      | Scrape Presidential Grants application cards from presidentskiegranty.rf. |
| `1_2_scrape_org_inn.R`              | Scrape organisation INN (tax ID) from gosgranti. |
| `1_3_scrape_org_inn_parallel.R`     | Parallel/faster variant of 1_2. |
| `1_4_scrape_winner_outcomes.R`      | Scrape reported outcomes and badges for winners. |
| `1_5_scrape_2025_round1.R`          | 2025 round-1 results (added post-initial scrape). |
| `1_6_scrape_2025_round2.R`          | 2025 round-2 application list refresh. |
| `1_7_reparse_budget_period.R`       | Extract budget start/end dates from raw pages. |
| `1_8_reparse_budget_period_turbo.R` | Faster re-parse of budget fields. |
| `1_9_parse_quality_turbo.R`         | Parse official expert-evaluation quality score. |
| `1_10_patch_inn_2017_2019.R`        | Recover INNs for 2017–2019 applications. |
| `1_11_restore_and_drop_outliers.R`  | Restore competition 1646 field `polucheno`; drop 8 outliers. |
| `1_12_apply_patch_scores.R`         | Apply corrected government scores for competition 1646. |
| `1_13_audit_cofinancing.R`          | Audit lost applications; recompute co-financing share. |
| `1_14_refresh_budget_period_fields.R` | Refresh project-duration variables after restore. |
| `1_15_merge_sources.R`              | Merge PG, platforma-NKO, and INN sources into the analysis panel. |
| `1_16_process_via_ai.R`             | LLM coding of rhetoric, target group, and quality (OpenAI / Gemini / Ollama). |
| `1_17_attach_scores_build_vars.R`   | Attach LLM scores and build all regression variables. |

## 2. Analysis (paper)

| Script | Produces |
|--------|----------|
| `2_1_regression_grant_receipt.R` | **Table 1** (descriptive stats), **Figure 3** (AME per 1 SD), **Table 2** (main AMEs), **Table A1** (rhetoric AMEs), **Table A2** (post-2022 AMEs), **Table A3** (AMEs by region), badge-robustness table. |
| `2_2_panel_twfe.R`               | **Table 3** (TWFE panel), **Table A5** (within-winner trajectories), cohort-survival and org-type tables. |
| `2_3_trad_values_analysis.R`     | **Figure 2** (share of applications engaging with state discourse by direction). |
| `2_4_plot_time_trends.R`         | **Figure 1** (mean quality and co-financing over time). |

## 3. Validation (appendix)

| Script | Produces |
|--------|----------|
| `3_1_validation_lib.R`             | Shared helpers sourced by `3_2` and `3_3` (not run directly). |
| `3_2_validate_winner_loser.R`      | **Figure A3** (density of Gemini 2.5 quality by winner/loser). |
| `3_3_validate_gemini_vs_oss120b.R` | **Figure A4** (Gemini 2.5 vs. OSS 120B scatter). |
| `3_4_journal_graphs.R`             | **Figures A5, A7, A8** (gold vs. Gemini; human vs. model target age; rhetoric confusion matrix) and inter-coder statistics reported in §Validation. |

## Libraries (not standalone)

- `lib_functions.R` — shared utilities for scraping and AI calls; sourced by `1_16`.
- `lib_askai_ollama.R` — Ollama API wrappers; sourced by `1_16`.

## `_deprecated/`

Archived drafts, superseded variants, and code for results not included
in the paper or appendix. Not required for replication.
