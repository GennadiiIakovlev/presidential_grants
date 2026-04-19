

library(rvest)

library(tidyverse)

data_dir <- "data/data_large"
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
cache_dir <- file.path(data_dir, "cache_pages")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

# Path to the backup file
backup_file <- file.path(data_dir, "all_projects_data_backup.csv")
existing_hrefs <- if (file.exists(backup_file)) {
  read_csv(backup_file, col_types = cols(href = col_character())) %>%
    pull(href) %>%
    unique()
} else {
  character()
}

# Function to read and parse page data
parse_page <- function(page_number, backup_file) {
  # Define the cache file name
  cache_file <- file.path(cache_dir, paste0("cache_page_", page_number, ".rds"))
  
  # Check if cache file already exists
  if (file.exists(cache_file)) {
    message("Loading cached data for page ", page_number)
    data <- readRDS(cache_file)
  } else {
    # Construct the URL
    url <- paste0("https://xn--80afcdbalict6afooklqi5o.xn--p1ai/public/application/cards?page=", page_number)
    message("Downloading page ", page_number, " from URL: ", url)
    
    # Parse the HTML content
    parsed_html <- tryCatch({
      read_html(url)
    }, error = function(e) {
      message("Error downloading page ", page_number, ": ", e$message)
      return(NULL)
    })
    
    if (is.null(parsed_html)) {
      message("Skipping page ", page_number, " due to download error.")
      return(NULL)  # Return NULL in case of download failure
    }
    
    # Extract all rows with class "table__row cards-item-row"
    rows <- parsed_html %>% html_elements("a[href*='public/application/item']")
    
    if (length(rows) == 0) {
      message("No rows found on page ", page_number)
      return(tibble())  # Return empty tibble if no rows are found
    }
    
    # Extract data from each row
    data <- rows %>%
      map_dfr(~ {
        tibble(
          href = .x %>% html_attr("href") %>% coalesce("NA"),
          title = .x %>% html_element(".projects__title") %>% html_text(trim = TRUE) %>% coalesce("NA"),
          contest = .x %>% html_element(".contest") %>% html_text(trim = TRUE) %>% coalesce("NA"),
          direction = .x %>% html_element(".direction") %>% html_text(trim = TRUE) %>% coalesce("NA"),
          project_price = .x %>% html_element(".projects__price") %>% html_text(trim = TRUE) %>% coalesce("NA"),
          fond_price = .x %>% html_element(".projects__price--fond") %>% html_text(trim = TRUE) %>% coalesce("NA"),
          city = .x %>% html_element(".projects__descr div:first-child") %>% html_text(trim = TRUE) %>% coalesce("NA"),
          project_status = .x %>% html_element(".projects__type span") %>% html_text(trim = TRUE) %>% coalesce("NA"),
          application = .x %>% html_element(".projects__str-no-wrap") %>% html_text(trim = TRUE) %>% coalesce("NA")
        )
      })
    
    # Save to cache
    message("Caching data for page ", page_number)
    saveRDS(data, cache_file)
  }
  
  if (!is.null(data) && nrow(data) > 0) {
    data <- data %>% filter(!href %in% existing_hrefs)
  }

  # Write to backup CSV file
  if (!is.null(data) && nrow(data) > 0) {
    message("Writing data from page ", page_number, " to backup file: ", backup_file)
    write_csv(data, backup_file, append = TRUE)
    existing_hrefs <<- union(existing_hrefs, data$href)
  } else {
    message("No data to write for page ", page_number)
  }
  
  return(data)
}

# Create the backup file and write headers if it doesn't exist
if (!file.exists(backup_file)) {
  message("Creating backup file and writing headers to ", backup_file)
  # Create an empty tibble with the appropriate columns
  empty_data <- tibble(
    href = character(),
    title = character(),
    contest = character(),
    direction = character(),
    project_price = character(),
    fond_price = character(),
    city = character(),
    project_status = character(),
    application = character()
  )
  
  # Write the empty tibble as the header
  write_csv(empty_data, backup_file)
} else {
  message("Backup file already exists: ", backup_file)
}

# Iterate over pages 1 to 7968 and write data after each page to backup file
for (page_number in 1:7968) {
  message("Starting processing for page ", page_number, "...")
  parse_page(page_number, backup_file)
  message("Finished processing for page ", page_number, "\n-----------------------------------------")
}




# dataset_appl<-read.csv('all_projects_data_backup.csv')
# 
# text_html <- read_html("https://xn--80afcdbalict6afooklqi5o.xn--p1ai/public/application/item?id=7180bc68-c5e3-4fbd-ae11-9bc5d9dbb354")
# 
# text_html %>%
#   html_elements("section.winner__details") %>%
#   html_text()
# Read the dataset containing hrefs
dataset_appl <- read_csv(backup_file)

backup_file_details <- file.path(data_dir, "application_details_backup.csv")
existing_detail_hrefs <- if (file.exists(backup_file_details)) {
  read_csv(backup_file_details, col_types = cols(href = col_character())) %>%
    pull(href) %>%
    unique()
} else {
  character()
}

# Function to parse the details from each href
parse_application_details <- function(href, backup_file_details) {
  if (href %in% existing_detail_hrefs) {
    message("Skipping already processed href: ", href)
    return(tibble())
  }

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
  
  # Extract text from the "section.winner__details" element
  details <- parsed_html %>%
    html_elements("section.winner__details") %>%
    html_text(trim = TRUE) %>%
    coalesce("NA")
  
  # Return tibble with the href and extracted details
  data <- tibble(
    href = href,
    details = details
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
  # Create an empty tibble with the appropriate columns
  empty_data_details <- tibble(
    href = character(),
    details = character()
  )
  
  # Write the empty tibble as the header
  write_csv(empty_data_details, backup_file_details)
} else {
  message("Details backup file already exists: ", backup_file_details)
}

# Iterate over each href in the dataset to extract details
for (href in dataset_appl$href) {
  if (href != "NA") {
    parse_application_details(href, backup_file_details)
  } else {
    message("Skipping invalid href: NA")
  }
  
  message("Finished processing href: ", href, "\n-----------------------------------------")
}



