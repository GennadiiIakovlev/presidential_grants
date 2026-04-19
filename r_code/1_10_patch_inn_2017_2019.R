###############################################################################
# 0_patch_inn_2017_2019.R
#
# ONE-TIME patch: back-fills missing INN values in all_presi_variables.csv
# for the 2017-R1, 2017-R2, 2018-R1, 2018-R2, 2019-R1 rounds (and the ~20%
# gap in 2019-R2) that were never scraped in the original 1_4 run.
#
# Reads:   data/data_large/all_presi_variables.csv   (has href + stale inn)
#          data/data_large/application_inn_backup.csv (now complete after
#                                                      re-running 1_4)
# Writes:  data/data_large/all_presi_variables.csv   (in-place, backup first)
#
# After this patch, re-run:
#   2_3  → rebuilds presi_variables_analysis.csv with correct panel vars
#   3_reciept_regression_clean_gemini25_20260214.R
#   3_reciept_regression_relig_20260316.R
#   3_did_gemini25_20260316.R
#   3_panel_analysis_org_fe_20260224.R
#   make_event_study_cofinancing_20260220.R
#   make_time_plots_20260220.R
###############################################################################

pacman::p_load(tidyverse)

data_dir <- "data/data_large"

# ── 1. Load files ─────────────────────────────────────────────────────────────

apv <- read_csv(
  file.path(data_dir, "all_presi_variables.csv"),
  show_col_types = FALSE
)

inn_backup <- read_csv(
  file.path(data_dir, "application_inn_backup.csv"),
  col_types = cols(href = col_character(), inn = col_character())
) %>%
  mutate(href_key = tolower(href)) %>%
  filter(!is.na(inn), inn != "NA", inn != "") %>%
  distinct(href_key, .keep_all = TRUE) %>%
  select(href_key, inn_new = inn)

cat("all_presi_variables rows:", nrow(apv), "\n")
cat("INN backup rows (non-NA):", nrow(inn_backup), "\n")

# ── 2. Diagnose before ────────────────────────────────────────────────────────

na_before <- sum(is.na(apv$inn) | apv$inn == "NA" | apv$inn == "")
cat("\nMissing INN before patch:", na_before, "\n")

# ── 3. Backup original ────────────────────────────────────────────────────────

backup_path <- file.path(
  data_dir,
  paste0("all_presi_variables_pre_inn_patch_", Sys.Date(), ".csv")
)
write_csv(apv, backup_path)
cat("Backup written to:", backup_path, "\n")

# ── 4. Join and coalesce ──────────────────────────────────────────────────────

apv_patched <- apv %>%
  mutate(
    href_key = tolower(href),
    inn      = na_if(as.character(inn), "NA"),
    inn      = na_if(inn, "")
  ) %>%
  left_join(inn_backup, by = "href_key") %>%
  mutate(inn = coalesce(inn, inn_new)) %>%
  select(-href_key, -inn_new)

# ── 5. Diagnose after ─────────────────────────────────────────────────────────

na_after  <- sum(is.na(apv_patched$inn))
filled_in <- na_before - na_after

cat("Missing INN after patch: ", na_after,  "\n")
cat("INNs filled in:          ", filled_in, "\n")

if (filled_in < 30000) {
  warning(
    "Fewer INNs filled than expected (~36k). ",
    "Check that 1_4 finished successfully and application_inn_backup.csv is complete."
  )
}

# ── 6. Save in-place ──────────────────────────────────────────────────────────

write_csv(apv_patched, file.path(data_dir, "all_presi_variables.csv"))
cat("Patched file saved to: data/data_large/all_presi_variables.csv\n")
cat("\nNext: re-run 2_3 and all downstream 3_*.R scripts.\n")
