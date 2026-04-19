###############################################################################
# Apply patch scores for restored 1,646 winners into main dataset.
#
# Input:
# - data/data_large/presi_variables_imputed.csv
# - data/data_large/patch1646_scores_2026-02-15.csv
#
# Output (overwrites main dataset, with safety backup):
# - data/data_large/presi_variables_imputed.csv
###############################################################################

pacman::p_load(dplyr, readr, stringr)

main_path <- "data/data_large/presi_variables_imputed.csv"
patch_path <- "data/data_large/patch1646_scores_2026-02-15.csv"

stopifnot(file.exists(main_path))
stopifnot(file.exists(patch_path))

normalize_href <- function(x) {
  x <- as.character(x)
  stringr::str_trim(x)
}

message("Reading patch: ", patch_path)
patch <- readr::read_csv(patch_path, show_col_types = FALSE) %>%
  mutate(
    href = normalize_href(href),
    relevance = suppressWarnings(as.numeric(relevance)),
    coherence = suppressWarnings(as.numeric(coherence)),
    budget_realism = suppressWarnings(as.numeric(budget_realism)),
    scale = suppressWarnings(as.numeric(scale))
  ) %>%
  filter(!is.na(href), href != "", href != "NA") %>%
  distinct(href, .keep_all = TRUE)

need <- c("href", "relevance", "coherence", "budget_realism", "scale")
stopifnot(all(need %in% names(patch)))

message("Reading main dataset: ", main_path)
df <- readr::read_csv(main_path, show_col_types = FALSE) %>%
  mutate(href = normalize_href(href)) %>%
  filter(!is.na(href), href != "", href != "NA")

message("Joining patch into main...")
out <- df %>%
  left_join(
    patch %>% rename(
      relevance_patch = relevance,
      coherence_patch = coherence,
      budget_realism_patch = budget_realism,
      scale_patch = scale
    ),
    by = "href"
  ) %>%
  mutate(
    relevance = relevance_patch,
    coherence = coherence_patch,
    budget_realism = budget_realism_patch,
    scale = scale_patch
  ) %>%
  select(-relevance_patch, -coherence_patch, -budget_realism_patch, -scale_patch)

message(
  "Patch coverage in output (non-NA relevance): ",
  sum(!is.na(out$relevance)),
  " / ",
  nrow(out)
)

missing_in_main <- patch %>% anti_join(out %>% distinct(href), by = "href")
stopifnot(nrow(missing_in_main) == 0)

backup_path <- "data/data_large/presi_variables_imputed_pre_patch1646_scores_20260215.csv"
message("Writing safety backup: ", backup_path)
readr::write_csv(df, backup_path)

message("Overwriting main dataset: ", main_path)
readr::write_csv(out, main_path)

message("Done.")
