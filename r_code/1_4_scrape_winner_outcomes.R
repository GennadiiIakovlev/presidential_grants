# =============================================================================
#  1_5_scrape_winner_outcomes.R
#
#  Scrape implementation outcomes for all winning applications from
#  президентскиегранты.рф. Each winning application page contains:
#    - Implementation status (e.g., "Проект реализован неудовлетворительно
#      или не реализован")
#    - Implementation period (e.g., "02.11.2019 - 30.12.2020")
#
#  These allow us to reconstruct had_proj_failure_before from actual
#  grant outcome dates, eliminating circularity entirely.
# =============================================================================

pacman::p_load(rvest, tidyverse, httr, readr, tibble, furrr, future)

# Add delay between requests to avoid rate limiting
Sys.sleep <- function(...) base::Sys.sleep(...)  # Ensure Sys.sleep is available

data_dir <- "data/data_large"

# --- 1. Identify winner hrefs to scrape ---

all_vars <- read_csv(file.path(data_dir, "all_presi_variables.csv"),
                     show_col_types = FALSE)

winner_hrefs <- all_vars %>%
  filter(project_status == 1, !is.na(href), href != "NA") %>%
  pull(href) %>%
  unique()

message(sprintf("Total unique winner hrefs: %d", length(winner_hrefs)))

# --- 2. Load existing results (resume support) ---

outcome_file <- file.path(data_dir, "winner_outcomes_backup.csv")

existing_hrefs <- if (file.exists(outcome_file)) {
  read_csv(outcome_file, col_types = cols(href = col_character())) %>%
    pull(href) %>%
    unique()
} else {
  character()
}

hrefs_to_scrape <- setdiff(winner_hrefs, existing_hrefs)
message(sprintf("%d already scraped, %d remaining.",
                length(existing_hrefs), length(hrefs_to_scrape)))

if (length(hrefs_to_scrape) == 0) {
  message("Nothing to scrape. Exiting.")
  quit(save = "no")
}

# --- 3. Scraper function ---

scrape_outcome <- function(href) {
  url <- paste0("https://xn--80afcdbalict6afooklqi5o.xn--p1ai", href)

  parsed_html <- tryCatch({
    resp <- httr::GET(
      url,
      httr::timeout(30),
      httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"),
      httr::add_headers(
        `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
        `Accept-Language` = "en-US,en;q=0.5",
        `Referer` = "https://www.google.com/"
      )
    )
    
    # Don't stop for 403, handle it gracefully
    if (httr::status_code(resp) == 403) {
      message("403 Forbidden - Website blocking requests")
      return(NULL)
    }
    httr::stop_for_status(resp)
    read_html(httr::content(resp, as = "text", encoding = "UTF-8"))
  }, error = function(e) {
    return(NULL)
  })

  if (is.null(parsed_html)) {
    return(tibble(href = href,
                  implementation_status = NA_character_,
                  implementation_period = NA_character_,
                  scrape_error = TRUE))
  }

  # The full page text — we search for known status patterns
  page_text <- parsed_html %>% html_text2()

  # Extract implementation status using fixed=TRUE to avoid encoding issues
  # Known patterns on the application pages:
  #   "Проект реализован неудовлетворительно или не реализован" → failed
  #   "Проект реализован успешно" → completed
  #   "Проект реализован" (without неудовлетворительно) → completed
  #   "победитель конкурса" (no outcome yet) → winner_no_outcome
  status <- NA_character_

  has_fail <- grepl("\u043d\u0435\u0443\u0434\u043e\u0432\u043b\u0435\u0442\u0432\u043e\u0440\u0438\u0442\u0435\u043b\u044c\u043d\u043e",
                    page_text, fixed = TRUE) ||
              grepl("\u043d\u0435 \u0440\u0435\u0430\u043b\u0438\u0437\u043e\u0432\u0430\u043d",
                    page_text, fixed = TRUE)
  has_success <- grepl("\u0440\u0435\u0430\u043b\u0438\u0437\u043e\u0432\u0430\u043d \u0443\u0441\u043f\u0435\u0448\u043d\u043e",
                       page_text, fixed = TRUE)
  has_impl <- grepl("\u0440\u0435\u0430\u043b\u0438\u0437\u043e\u0432\u0430\u043d",
                    page_text, fixed = TRUE)

  if (has_fail) {
    status <- "failed"
  } else if (has_success) {
    status <- "completed"
  } else if (has_impl) {
    status <- "completed"
  } else {
    status <- "winner_no_outcome"
  }

  # Extract implementation period (DD.MM.YYYY - DD.MM.YYYY)
  period <- stringr::str_extract(page_text,
    "\\d{2}\\.\\d{2}\\.\\d{4}\\s*[-–]\\s*\\d{2}\\.\\d{2}\\.\\d{4}")

  tibble(
    href = href,
    implementation_status = status,
    implementation_period = period,
    scrape_error = FALSE
  )
}

# --- 4. Scrape in batches with progress ---

batch_size <- 100
n_batches <- ceiling(length(hrefs_to_scrape) / batch_size)

message(sprintf("Scraping %d hrefs in %d batches of %d...",
                length(hrefs_to_scrape), n_batches, batch_size))

for (b in seq_len(n_batches)) {
  start_idx <- (b - 1) * batch_size + 1
  end_idx <- min(b * batch_size, length(hrefs_to_scrape))
  batch_hrefs <- hrefs_to_scrape[start_idx:end_idx]

  batch_results <- map_dfr(batch_hrefs, function(h) {
    tryCatch({
      result <- scrape_outcome(h)
      Sys.sleep(1)  # 1 second delay between requests
      result
    }, error = function(e) {
      tibble(href = h, implementation_status = NA_character_,
             implementation_period = NA_character_, scrape_error = TRUE)
    })
  })

  # Append to backup file
  if (file.exists(outcome_file)) {
    write_csv(batch_results, outcome_file, append = TRUE)
  } else {
    write_csv(batch_results, outcome_file)
  }

  n_done <- end_idx
  n_failed <- sum(batch_results$scrape_error, na.rm = TRUE)
  n_proj_failed <- sum(batch_results$implementation_status == "failed", na.rm = TRUE)
  message(sprintf("Batch %d/%d done. %d/%d total. %d errors this batch. %d project failures found.",
                  b, n_batches, n_done, length(hrefs_to_scrape), n_failed, n_proj_failed))
}

# --- 5. Summary ---

outcomes <- read_csv(outcome_file, show_col_types = FALSE)
message("\n=== Scraping complete ===")
message(sprintf("Total scraped: %d", nrow(outcomes)))
message(sprintf("Errors: %d", sum(outcomes$scrape_error, na.rm = TRUE)))
message("Status distribution:")
print(table(outcomes$implementation_status, useNA = "ifany"))
