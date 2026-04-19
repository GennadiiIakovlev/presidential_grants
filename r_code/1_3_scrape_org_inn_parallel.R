################################################################################
# 1_4b_inn_parallel_scraper.R
#
# Fast async INN scraper for missing hrefs (2017-2019 rounds + any other gaps).
# Uses curl::multi_run() for true concurrent HTTP — no blocking per request.
#
# Strategy:
#   - 80 concurrent workers by default
#   - 8-second per-request timeout
#   - Rotating user-agents across Chrome/Firefox/Safari/Edge on Win/Mac/Linux
#   - Up to 3 retries per URL: short wait + new user-agent on each attempt
#   - Saves to backup CSV every 500 successful records (crash-safe)
#   - Skips anything already in application_inn_backup.csv
#   - Prints a live progress counter
################################################################################

pacman::p_load(curl, rvest, tidyverse, readr, tibble)

# ── Config ────────────────────────────────────────────────────────────────────
DATA_DIR       <- "data/data_large"
APPS_FILE      <- file.path(DATA_DIR, "all_projects_data_backup.csv")
BACKUP_FILE    <- file.path(DATA_DIR, "application_inn_backup.csv")
BASE_URL       <- "https://xn--80afcdbalict6afooklqi5o.xn--p1ai"

N_WORKERS      <- 80     # concurrent connections
TIMEOUT_S      <- 8      # seconds per request before giving up
MAX_RETRIES    <- 3      # attempts per href before marking as failed
SAVE_EVERY     <- 500    # flush results to disk every N successes
RETRY_DELAY_S  <- c(1, 3, 7)  # wait before each retry attempt

# ── User-agent pool ───────────────────────────────────────────────────────────
USER_AGENTS <- c(
  # Chrome on Windows
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36",
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
  # Chrome on Mac
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36",
  # Firefox on Windows
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:123.0) Gecko/20100101 Firefox/123.0",
  # Firefox on Linux
  "Mozilla/5.0 (X11; Linux x86_64; rv:122.0) Gecko/20100101 Firefox/122.0",
  # Safari on Mac
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 14_3) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.2 Safari/605.1.15",
  # Edge on Windows
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36 Edg/122.0.0.0",
  # Chrome on Android
  "Mozilla/5.0 (Linux; Android 14; Pixel 8) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.6261.64 Mobile Safari/537.36"
)

pick_ua <- function() sample(USER_AGENTS, 1)

# ── XPath for INN ─────────────────────────────────────────────────────────────
INN_XPATH <- "//li[.//span[contains(@class,'winner-info__list-item-title') and normalize-space(text())='ИНН']]//span[contains(@class,'winner-info__list-item-text')]"

extract_inn <- function(html_text) {
  tryCatch({
    doc <- read_html(html_text)
    val <- doc %>% html_elements(xpath = INN_XPATH) %>% html_text(trim = TRUE)
    if (length(val) > 0 && nchar(val[1]) > 0) val[1] else NA_character_
  }, error = function(e) NA_character_)
}

# ── Load existing hrefs ───────────────────────────────────────────────────────
existing_hrefs <- if (file.exists(BACKUP_FILE)) {
  read_csv(BACKUP_FILE, col_types = cols(href = col_character())) %>%
    pull(href) %>% unique()
} else {
  # Create file with header
  write_csv(tibble(href = character(), inn = character()), BACKUP_FILE)
  character()
}

# ── Compute missing hrefs ─────────────────────────────────────────────────────
all_hrefs <- read_csv(APPS_FILE, col_types = cols(.default = col_character())) %>%
  pull(href) %>% unique() %>% na.omit()

todo <- setdiff(all_hrefs, existing_hrefs)

message(sprintf("Already scraped: %d | To scrape: %d", length(existing_hrefs), length(todo)))
if (length(todo) == 0) { message("Nothing to do. Exiting."); quit(save = "no") }

# ── Async scrape function (one batch) ─────────────────────────────────────────
scrape_batch <- function(hrefs) {
  results <- vector("list", length(hrefs))
  names(results) <- hrefs

  handles <- lapply(hrefs, function(href) {
    h <- new_handle(
      url        = paste0(BASE_URL, href),
      timeout    = TIMEOUT_S,
      useragent  = pick_ua(),
      followlocation = TRUE,
      ssl_verifypeer = FALSE   # site sometimes has cert hiccups
    )
    h
  })

  pool <- new_pool(total_con = length(hrefs), host_con = N_WORKERS)

  for (i in seq_along(hrefs)) {
    href <- hrefs[i]
    multi_add(
      handles[[i]],
      pool = pool,
      done = function(res) {
        h <- sub(BASE_URL, "", res$url, fixed = TRUE)
        # strip query params that curl may have added
        h <- sub("\\?.*$", "", h)
        # normalise back to original href format
        matched <- hrefs[startsWith(paste0(BASE_URL, hrefs), res$url) |
                           hrefs == h]
        key <- if (length(matched) > 0) matched[1] else h
        if (res$status_code == 200) {
          inn_val <- extract_inn(rawToChar(res$content))
          results[[key]] <<- list(href = key, inn = inn_val, ok = TRUE)
        } else {
          results[[key]] <<- list(href = key, inn = NA_character_, ok = FALSE,
                                  status = res$status_code)
        }
      },
      fail = function(err) {
        # err is the error message string; we can't easily get the href back
        # so we just leave results entry as NULL (handled in retry logic)
      }
    )
  }

  multi_run(pool = pool)
  results
}

# ── Main loop with retries ─────────────────────────────────────────────────────
total      <- length(todo)
done_count <- 0
failed     <- character()
buffer     <- tibble(href = character(), inn = character())

# Split into batches of N_WORKERS
batches <- split(todo, ceiling(seq_along(todo) / N_WORKERS))

for (batch_i in seq_along(batches)) {
  batch   <- batches[[batch_i]]
  pending <- batch

  for (attempt in seq_len(MAX_RETRIES)) {
    if (length(pending) == 0) break
    if (attempt > 1) {
      Sys.sleep(RETRY_DELAY_S[min(attempt, length(RETRY_DELAY_S))])
      message(sprintf("  Retry %d for %d hrefs in batch %d",
                      attempt, length(pending), batch_i))
    }

    res <- scrape_batch(pending)

    succeeded <- character()
    still_pending <- character()

    for (href in pending) {
      r <- res[[href]]
      if (!is.null(r) && isTRUE(r$ok)) {
        buffer <- bind_rows(buffer, tibble(href = r$href, inn = coalesce(r$inn, "NA")))
        succeeded <- c(succeeded, href)
        done_count <- done_count + 1
      } else {
        still_pending <- c(still_pending, href)
      }
    }

    pending <- still_pending
  }

  # Mark permanently failed ones as NA so they're not retried on next run
  if (length(pending) > 0) {
    failed <- c(failed, pending)
    buffer <- bind_rows(buffer,
                        tibble(href = pending, inn = rep("FAILED", length(pending))))
  }

  # Flush buffer to disk
  if (nrow(buffer) >= SAVE_EVERY || batch_i == length(batches)) {
    write_csv(buffer, BACKUP_FILE, append = TRUE)
    message(sprintf("[%d/%d] Flushed %d rows to disk | failed so far: %d",
                    done_count, total, nrow(buffer), length(failed)))
    buffer <- tibble(href = character(), inn = character())
  } else {
    pct <- round(100 * done_count / total)
    message(sprintf("[%d/%d %d%%] batch %d done | buffer: %d | failed: %d",
                    done_count, total, pct, batch_i, nrow(buffer), length(failed)))
  }
}

# ── Summary ───────────────────────────────────────────────────────────────────
message("\n=== Done ===")
message(sprintf("  Scraped:  %d", done_count))
message(sprintf("  Failed:   %d", length(failed)))
if (length(failed) > 0) {
  message("  Failed hrefs saved to: failed_inn_hrefs.txt")
  writeLines(failed, "failed_inn_hrefs.txt")
}

