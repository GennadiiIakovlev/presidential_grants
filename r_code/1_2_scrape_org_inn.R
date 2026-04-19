pacman::p_load(rvest, tidyverse, magrittr, readr, tibble)

data_dir <- "data/data_large"
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

dataset_appl <- read_csv(file.path(data_dir, "all_projects_data_backup.csv"))

backup_file_details <- file.path(data_dir, "application_inn_backup.csv")
existing_detail_hrefs <- if (file.exists(backup_file_details)) {
  read_csv(backup_file_details, col_types = cols(href = col_character())) %>%
    pull(href) %>%
    unique()
} else {
  character()
}

# Pre-filter to only hrefs not yet in the backup — avoids iterating 300K rows
hrefs_to_scrape <- dataset_appl %>%
  filter(!is.na(href), href != "NA", !href %in% existing_detail_hrefs) %>%
  pull(href) %>%
  unique()

message(sprintf(
  "%d hrefs already scraped, %d remaining to scrape.",
  length(existing_detail_hrefs), length(hrefs_to_scrape)
))

# Function to parse the details from each href
parse_application_details <- function(href, backup_file_details) {
  # Construct the URL
  url <- paste0("https://xn--80afcdbalict6afooklqi5o.xn--p1ai", href)
  message("Fetching details from URL: ", url)
  
  # Parse the HTML content
  parsed_html <- tryCatch({
    read_html(url)
  }, error = function(e) {
    message("Error fetching details from ", url, ": ", e$message)
    return(NULL)
  })
  
  if (is.null(parsed_html)) {
    message("Skipping due to download error for href: ", href)
    return(tibble())
  }
  
  # Extract the ИНН value.
  # This XPath looks for an <li> element whose child <span> with class
  # 'winner-info__list-item-title' exactly equals "ИНН" (after trimming)
  # and then selects the corresponding <span> with class "winner-info__list-item-text".
  inn <- parsed_html %>%
    html_elements(xpath = "//li[.//span[contains(@class, 'winner-info__list-item-title') and normalize-space(text())='ИНН']]//span[contains(@class, 'winner-info__list-item-text')]") %>%
    html_text(trim = TRUE)
  
  # If multiple elements are found, take the first; if none, set to "NA"
  inn <- if (length(inn) > 0) inn[1] else "NA"
  
  # Create a tibble with the href and extracted ИНН details
  data <- tibble(
    href = href,
    inn = inn
  )
  
  # Write to backup CSV file
  message("Writing details for href: ", href, " to backup file.")
  write_csv(data, backup_file_details, append = TRUE)
  existing_detail_hrefs <<- union(existing_detail_hrefs, data$href)
  
  return(data)
}

# Create the backup file and write headers if it doesn't exist
if (!file.exists(backup_file_details)) {
  message("Creating details backup file and writing headers to ", backup_file_details)
  # Create an empty tibble with the appropriate columns (now "inn" instead of "details")
  empty_data_details <- tibble(
    href = character(),
    inn = character()
  )
  
  # Write the empty tibble as the header
  write_csv(empty_data_details, backup_file_details)
} else {
  message("Details backup file already exists: ", backup_file_details)
}

# Iterate over only the unscraped hrefs
for (href in hrefs_to_scrape) {
  parse_application_details(href, backup_file_details)
  message("Finished processing href: ", href, "\n-----------------------------------------")
}

