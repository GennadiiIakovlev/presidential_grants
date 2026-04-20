# Helpers for OpenAI ChatGPT batch requests.
pacman::p_load(dplyr, purrr, progress, stringr, readr, httr, tibble)

api_ours <- Sys.getenv("OPENAI_API_KEY")
if (!nzchar(api_ours)) stop("Set OPENAI_API_KEY before sourcing this file.")

ask_chatgpt <- function(prompt,
                        column_text,
                        model = "gpt-4o-mini",
                        api_key = api_ours) {
  
  if (identical(api_key, "")) stop("OPENAI_API_KEY is empty. Set it via Sys.setenv().")
  
  resp <- httr::POST(
    url = "https://api.openai.com/v1/chat/completions",
    httr::add_headers(Authorization = paste("Bearer", api_key)),
    httr::content_type_json(),
    encode = "json",
    body = list(
      model = model,
      messages = list(
        list(role = "system", content = prompt),
        list(role = "user",   content = column_text)
      )
    )
  )
  
  httr::stop_for_status(resp)
  
  parsed <- httr::content(resp, as = "parsed", type = "application/json", encoding = "UTF-8")
  stringr::str_trim(parsed$choices[[1]]$message$content)
}

########################################################################
########################################################################
# Function: process_row2
########################################################################
########################################################################

#' Process Each Row with ChatGPT API and Extract CSV Data
#'
#' This function processes each row of a data frame by making a request to the ChatGPT API.
#' It combines a given prompt text with text from a specified column in the row, 
#' sends this combined text to the ChatGPT API, and extracts a CSV portion from the API's response.
#' The function also adds additional columns to the extracted CSV data if provided.
#'
#' @param row A single row from a data frame.
#' @param column_text The name of the column in `row` containing the text to be appended to `prompt_text`.
#' @param prompt_text The initial text to be combined with `column_text` for the ChatGPT API request.
process_row2 <- function(row, column_text, column_id, prompt_text, 
                         progress_bar, 
                         keep_columns = c('reference_svo', 'rationale_svo_projects', 'projects_attitude_svo', 
                                          'tasks_relation_svo', 'relation_svo', 'patriotic_values', 
                                          'rationale_patriotic_values'),
                         model = "gpt-4o-mini",
                         show_input_output = FALSE) {
  # Update progress bar
  progress_bar$tick()
  
  # Call the ChatGPT API
  api_response <- ask_chatgpt(prompt_text, row[[column_text]], model = model)
  
  # Optionally display the API response
  if (show_input_output) {
    cat("API Response:\n", api_response, "\n")
  }
  
  # Extract the CSV content
  csv_data <- str_extract(api_response, "(?s)```csv\\s*(.*?)\\s*```")
  # Remove only the backticks and the "```csv" label, not the quotes inside the CSV itself
  csv_data <- str_replace_all(csv_data, "```csv\\s*|\\s*```", "")
  
  # Check if we found valid CSV data
  if (is.na(csv_data) || csv_data == "") {
    cat("No valid CSV data found in response for ID:", row[[column_id]], "\n")
    return(NULL)
  }
  
  # Read CSV data as character columns
  df_temp <- tryCatch({
    read_delim(
      I(csv_data),                  # treat string as the CSV content
      delim = ";",
      col_names = TRUE,
      show_col_types = FALSE,
      col_types = cols(.default = col_character())  # <---- force character
    )
  }, error = function(e) {
    cat("Error reading CSV data for ID:", row[[column_id]], "-", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(df_temp)) {
    return(NULL)
  }
  
  # Optional: Clean column names if needed
  colnames(df_temp) <- gsub('^"|"$', '', colnames(df_temp)) 
  colnames(df_temp) <- trimws(colnames(df_temp))
  
  # Add the ID column
  df_temp[[column_id]] <- row[[column_id]]
  
  # Select relevant columns
  relevant_cols <- intersect(names(df_temp), c(keep_columns, column_id))
  df_temp <- df_temp %>% select(all_of(relevant_cols))
  
  return(df_temp)
}

########################################################################
########################################################################
# Function: iterate_rows2
########################################################################
########################################################################

#' Iterate Over Rows of a Data Frame and Process Each Row
#'
#' This function iteratively processes each row of an input data frame using the `process_row` function.
#' It applies the ChatGPT API to extract CSV data from each row, and combines the results into a single data frame.
#' The function supports checkpointing by saving interim results to a file, allowing for resumption in case of interruption.
#'
#' @param df_input A data frame containing the data to be processed.
#' @param column_text The name of the column in `df_input` containing the text to be appended to `prompt_text`.
#' @param prompt_text The initial text to be combined with `column_text` for the ChatGPT API request.

iterate_rows2 <- function(df_input, column_text, column_id, prompt_text, 
                          checkpoint_path = "path/to/your/checkpoint.csv", 
                          model = "gpt-4o-mini",
                          errors_path = "path/to/your/error_log.csv",
                          show_input_output = FALSE) {
  id_errors <- c()
  
  existing_results <- if (file.exists(checkpoint_path)) {
    tryCatch(read.csv(checkpoint_path, stringsAsFactors = FALSE), error = function(e) tibble())
  } else {
    tibble()
  }
  
  print("No problems with checkpoint_path. Continue.")
  
  if (nrow(existing_results) > 0 && !column_id %in% names(df_input)) {
    stop("Column '", column_id, "' not found in df_input.")
  }
  
  new_rows <- if (nrow(existing_results) > 0 && column_id %in% names(existing_results)) {
    existing_ids <- existing_results[[column_id]]
    df_input %>% filter(!(.data[[column_id]] %in% existing_ids))
  } else {
    df_input
  }
  
  pb <- progress_bar$new(total = nrow(new_rows), format = "[:bar] :percent :elapsed/:est_elapsed")
  
  results <- existing_results
  
  for (i in seq_len(nrow(new_rows))) {
    tryCatch({
      row_to_process <- new_rows[i, ]
      
      # Process each row with ChatGPT
      result <- process_row2(row_to_process, column_text, column_id, prompt_text, pb, model = model, show_input_output = show_input_output)
      if (!is.null(result)) {
        results <- bind_rows(results, result)
      }
      
      write.csv(results, checkpoint_path, row.names = FALSE) 
      
    }, error = function(e) {
      cat("Error on row", i, ": ", e$message, "\n")
      id_errors <- c(id_errors, new_rows[i, column_id])
      write.csv(id_errors, errors_path, row.names = FALSE) 
    })
  }
  
  write.csv(results, checkpoint_path, row.names = FALSE)
  return(results)
}



########################################################################
########################################################################
# Function: process_row_target_audience
########################################################################
########################################################################

#' Process Each Row with ChatGPT API and Extract CSV Data
#'
#' This function processes each row of a data frame by making a request to the ChatGPT API.
#' It combines a given prompt text with text from a specified column in the row, 
#' sends this combined text to the ChatGPT API, and extracts a CSV portion from the API's response.
#' The function also adds additional columns to the extracted CSV data if provided.
#'
#' @param row A single row from a data frame.
#' @param column_text The name of the column in `row` containing the text to be appended to `prompt_text`.
#' @param prompt_text The initial text to be combined with `column_text` for the ChatGPT API request.
process_row_target_audience <- function(row, 
                                        column_text, 
                                        column_id, 
                                        prompt_text, 
                                        progress_bar, 
                                        keep_columns = c('target_age', 'target_old', 'target_young', 'target_disability'),
                                        model = "gpt-4o",
                                        show_input_output = FALSE) {
  # Update progress bar
  progress_bar$tick()
  
  # Call the ChatGPT API (assumes you have a function 'ask_chatgpt' defined)
  api_response <- ask_chatgpt(prompt_text, row[[column_text]], model = model)
  
  # Optionally display the API response
  if (show_input_output) {
    cat("API Response:\n", api_response, "\n")
  }
  
  # Try to extract CSV content wrapped in a markdown block
  csv_data <- str_extract(api_response, "(?s)```csv\\s*(.*?)\\s*```")
  csv_data <- str_replace_all(csv_data, "```csv\\s*|\\s*```", "")
  
  # If no markdown block was found but there are semicolons, use the whole response
  if (is.na(csv_data) || csv_data == "") {
    if (grepl(";", api_response)) {
      csv_data <- api_response
    } else {
      cat("No valid CSV data found in response for ID:", row[[column_id]], "\n")
      return(NULL)
    }
  }
  
  # Clean and trim the CSV text
  csv_data_clean <- trimws(csv_data)
  
  # Split into lines to decide if there's a header row
  lines <- unlist(strsplit(csv_data_clean, "\n"))
  
  df_temp <- tryCatch({
    if (length(lines) == 1) {
      # Single line: assume no header row.
      tmp <- read_delim(I(csv_data_clean),
                        delim = ";",
                        col_names = FALSE,
                        col_types = cols(.default = col_character()),
                        trim_ws = TRUE)
      tmp
    } else {
      # Otherwise, assume there is a header row.
      read_delim(I(csv_data_clean),
                 delim = ";",
                 col_names = TRUE,
                 col_types = cols(.default = col_character()),
                 trim_ws = TRUE)
    }
  }, error = function(e) {
    cat("Error processing CSV for ID:", row[[column_id]], "-", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(df_temp)) {
    return(NULL)
  }
  
  # Add the ID column
  df_temp[[column_id]] <- row[[column_id]]
  
  # Select only the relevant columns (plus the ID)
  relevant_cols <- intersect(names(df_temp), c(keep_columns, column_id))
  df_temp <- df_temp %>% select(all_of(relevant_cols))
  
  return(df_temp)
}
      

########################################################################
########################################################################
# Function: iterate_rows_target_audience
########################################################################
########################################################################

#' Iterate Over Rows of a Data Frame and Process Each Row
#'
#' This function iteratively processes each row of an input data frame using the `process_row` function.
#' It applies the ChatGPT API to extract CSV data from each row, and combines the results into a single data frame.
#' The function supports checkpointing by saving interim results to a file, allowing for resumption in case of interruption.
#'
#' @param df_input A data frame containing the data to be processed.
#' @param column_text The name of the column in `df_input` containing the text to be appended to `prompt_text`.
#' @param prompt_text The initial text to be combined with `column_text` for the ChatGPT API request.

# Function to process an individual row by sending a prompt to ChatGPT,
# extracting CSV content from the response, and returning a data frame.


# Function to iterate over rows of a data frame, process each with ChatGPT,
# and combine the results. Checkpoints are saved and errors logged.
iterate_rows_target_audience <- function(df_input, column_text, column_id, prompt_text, 
                                         checkpoint_path = "path/to/your/checkpoint.csv", 
                                         model = "gpt-4o-mini",
                                         errors_path = "path/to/your/error_log.csv",
                                         show_input_output = FALSE) {
  id_errors <- c()
  
  # Read any existing checkpoint; force all columns to be characters
  existing_results <- if (file.exists(checkpoint_path)) {
    tryCatch(
      read.csv(checkpoint_path, stringsAsFactors = FALSE, colClasses = "character"),
      error = function(e) tibble()
    )
  } else {
    tibble()
  }
  
  print("No problems with checkpoint_path. Continue.")
  
  if (nrow(existing_results) > 0 && !column_id %in% names(df_input)) {
    stop("Column '", column_id, "' not found in df_input.")
  }
  
  # Only process rows whose IDs are not already in the checkpoint
  new_rows <- if (nrow(existing_results) > 0 && column_id %in% names(existing_results)) {
    existing_ids <- existing_results[[column_id]]
    df_input %>% filter(!(.data[[column_id]] %in% existing_ids))
  } else {
    df_input
  }
  
  pb <- progress_bar$new(total = nrow(new_rows), format = "[:bar] :percent :elapsed/:est_elapsed")
  
  # Start with the existing results
  results <- existing_results
  
  for (i in seq_len(nrow(new_rows))) {
    tryCatch({
      row_to_process <- new_rows[i, ]
      
      # Process the row with ChatGPT
      result <- process_row_target_audience(row_to_process, column_text, column_id, prompt_text, pb, model = model, show_input_output = show_input_output)
      if (!is.null(result)) {
        results <- bind_rows(results, result)
      }
      
      # Force all columns to character to ensure consistent types
      results <- results %>% mutate(across(everything(), as.character))
      
      # Write an interim checkpoint
      write.csv(results, checkpoint_path, row.names = FALSE)
      
    }, error = function(e) {
      cat("Error on row", i, ": ", e$message, "\n")
      id_errors <- c(id_errors, new_rows[i, column_id])
      write.csv(id_errors, errors_path, row.names = FALSE)
    })
  }
  
  # Write final checkpoint and return the results
  write.csv(results, checkpoint_path, row.names = FALSE)
  return(results)
}

#------------------------------------------------------------
#------------------------------------------------------------
#------------------------------------------------------------
#------------------------------------------------------------
# Ollama. Modified: process_row_target_audience
#------------------------------------------------------------
# This function now uses ask_ollama from the ASKAI package to query the server.
process_row_target_audience_ollama <- function(row, 
                                               column_text, 
                                               column_id, 
                                               prompt_text, 
                                               progress_bar, 
                                               keep_columns = c('target_age', 'target_old', 'target_young', 'target_disability'),
                                               model = "mistral-small:22b-instruct-2409-fp16",
                                               show_input_output = FALSE) {
  # Update progress bar
  progress_bar$tick()
  
  # Query the server using ASKAI's ask_ollama function.
  api_response <- ask_ollama(
    system_content = prompt_text,
    user_content = row[[column_text]],
    model = model
  )
  
  # Debug: Print the raw API response
  if (show_input_output) {
    cat("Raw API response for ID", row[[column_id]], ":\n", api_response, "\n\n")
  }
  
  # Attempt to extract CSV content wrapped in triple backticks
  csv_data <- str_extract(api_response, "(?s)```csv\\s*(.*?)\\s*```")
  if (!is.na(csv_data)) {
    csv_data <- str_replace_all(csv_data, "```csv\\s*|\\s*```", "")
  } else {
    csv_data <- ""
  }
  
  # If no CSV block was found, use the entire response if it contains semicolons
  if (csv_data == "" && grepl(";", api_response)) {
    csv_data <- api_response
  }
  
  # Remove any system markers (lines starting with <|im_start|> or <|im_end|>)
  csv_lines <- unlist(strsplit(csv_data, "\n"))
  csv_lines <- csv_lines[!grepl("^<\\|im_start\\|>|^<\\|im_end\\|>", csv_lines)]
  csv_data_clean <- paste(csv_lines, collapse = "\n")
  
  # Debug: Show the cleaned CSV text
  if (show_input_output) {
    cat("Cleaned CSV data for ID", row[[column_id]], ":\n", csv_data_clean, "\n\n")
  }
  
  # Trim whitespace and check if the string is empty
  csv_data_clean <- trimws(csv_data_clean)
  if (csv_data_clean == "") {
    cat("No valid CSV data found in response for ID:", row[[column_id]], "\n")
    fallback_df <- as.data.frame(matrix(NA, nrow = 1, ncol = length(keep_columns)))
    names(fallback_df) <- keep_columns
    fallback_df$raw_response <- api_response
    fallback_df[[column_id]] <- row[[column_id]]
    return(fallback_df)
  }
  
  # Split into lines
  lines <- unlist(strsplit(csv_data_clean, "\n"))
  
  # Try parsing the CSV.
  df_temp <- tryCatch({
    if (length(lines) == 1) {
      # Single line: assume no header row.
      tmp <- read_delim(I(csv_data_clean),
                        delim = ";",
                        col_names = FALSE,
                        col_types = cols(.default = col_character()),
                        trim_ws = TRUE)
      # If the number of columns matches your expected number, assign names.
      if (ncol(tmp) == length(keep_columns)) {
        names(tmp) <- keep_columns
      }
      tmp
    } else {
      # Multiple lines: try reading with a header row.
      tmp <- read_delim(I(csv_data_clean),
                        delim = ";",
                        col_names = TRUE,
                        col_types = cols(.default = col_character()),
                        trim_ws = TRUE)
      # If the column names are not what you expect but the first column looks like keys, pivot:
      if (!all(keep_columns %in% names(tmp)) && ncol(tmp) >= 2) {
        # Check if the first column contains any of the expected keys.
        if (all(tmp[[1]] %in% keep_columns)) {
          # Use the second column as the value.
          tmp <- as.data.frame(as.list(setNames(tmp[[2]], tmp[[1]])), stringsAsFactors = FALSE)
        }
      }
      tmp
    }
  }, error = function(e) {
    cat("Error processing CSV for ID:", row[[column_id]], "-", e$message, "\n")
    return(NULL)
  })
  
  # Optionally, print parsing problems
  if (!is.null(df_temp)) {
    probs <- problems(df_temp)
    if (nrow(probs) > 0 && show_input_output) {
      cat("Parsing problems for ID", row[[column_id]], ":\n")
      print(probs)
    }
  }
  
  if (is.null(df_temp) || nrow(df_temp) == 0) {
    cat("No data parsed for ID:", row[[column_id]], "\n")
    return(NULL)
  }
  
  # Add the identifier column to the returned data frame
  df_temp[[column_id]] <- row[[column_id]]
  
  # Ensure all expected keep_columns exist; if missing, add them as NA.
  for (col in keep_columns) {
    if (!col %in% names(df_temp)) {
      df_temp[[col]] <- NA
    }
  }
  
  # Select only the desired columns (plus the identifier).
  # This works whether the columns came in with the expected names or were pivoted.
  relevant_cols <- unique(c(keep_columns, column_id, if ("raw_response" %in% names(df_temp)) "raw_response"))
  df_temp <- df_temp %>% select(all_of(relevant_cols))
  
  return(df_temp)
}



#------------------------------------------------------------
# Modified: iterate_rows_target_audience
#------------------------------------------------------------
# This function iterates over rows of your data frame, uses the above function
# to query the server via ASKAI for each row, and combines the results.
iterate_rows_target_audience_ollama  <- function(df_input, column_text, column_id, prompt_text, 
                                         checkpoint_path = "path/to/your/checkpoint.csv", 
                                         model = "mistral-large:123b-instruct-2411-q8_0",
                                         errors_path = "path/to/your/error_log.csv",
                                         show_input_output = FALSE) {
  id_errors <- c()
  
  # Load any existing checkpoint if available.
  existing_results <- if (file.exists(checkpoint_path)) {
    tryCatch(
      read.csv(checkpoint_path, stringsAsFactors = FALSE, colClasses = "character"),
      error = function(e) tibble()
    )
  } else {
    tibble()
  }
  
  print("No problems with checkpoint_path. Continue.")
  
  if (nrow(existing_results) > 0 && !column_id %in% names(df_input)) {
    stop("Column '", column_id, "' not found in df_input.")
  }
  
  # Filter to only the new rows (i.e. those not yet processed)
  new_rows <- if (nrow(existing_results) > 0 && column_id %in% names(existing_results)) {
    existing_ids <- existing_results[[column_id]]
    df_input %>% filter(!(.data[[column_id]] %in% existing_ids))
  } else {
    df_input
  }
  
  pb <- progress_bar$new(total = nrow(new_rows), format = "[:bar] :percent :elapsed/:est_elapsed")
  
  results <- existing_results
  
  for (i in seq_len(nrow(new_rows))) {
    tryCatch({
      row_to_process <- new_rows[i, ]
      
      # Process the current row via the ASKAI API.
      result <- process_row_target_audience_ollama(row_to_process, column_text, column_id, prompt_text, 
                                            pb, model = model, show_input_output = show_input_output)
      if (!is.null(result)) {
        results <- bind_rows(results, result)
      }
      
      # Force all columns to be characters for consistency.
      results <- results %>% mutate(across(everything(), as.character))
      
      # Save a checkpoint after processing each row.
      write.csv(results, checkpoint_path, row.names = FALSE)
      
    }, error = function(e) {
      cat("Error on row", i, ": ", e$message, "\n")
      id_errors <- c(id_errors, new_rows[i, column_id])
      write.csv(id_errors, errors_path, row.names = FALSE)
    })
  }
  
  # Save the final results and return.
  write.csv(results, checkpoint_path, row.names = FALSE)
  return(results)
}
process_row3 <- function(row, column_text, column_id, prompt_text, 
                         progress_bar, 
                         model = "gpt-4o-mini",
                         show_input_output = FALSE) {
  # Update progress bar
  progress_bar$tick()
  
  # Call the ChatGPT API
  api_response <- ask_chatgpt(prompt_text, row[[column_text]], model = model)
  
  # Optionally display the API response
  if (show_input_output) {
    cat("API Response:\n", api_response, "\n")
  }
  
  # Extract first number from the response
  extracted_number <- str_extract(api_response, "\\d+(\\.\\d+)?")
  
  if (is.na(extracted_number)) {
    cat("No valid number found in response for ID:", row[[column_id]], "\n")
    return(NULL)
  }
  
  # Create a data frame with ID and extracted number
  result <- tibble(!!column_id := row[[column_id]], extracted_number = as.numeric(extracted_number))
  return(result)
}

iterate_rows3 <- function(df_input, column_text, column_id, prompt_text, 
                          checkpoint_path = "path/to/your/checkpoint.csv", 
                          model = "gpt-4o-mini",
                          errors_path = "path/to/your/error_log.csv",
                          show_input_output = FALSE) {
  id_errors <- c()
  
  existing_results <- if (file.exists(checkpoint_path)) {
    tryCatch(read.csv(checkpoint_path, stringsAsFactors = FALSE), error = function(e) tibble())
  } else {
    tibble()
  }
  
  print("No problems with checkpoint_path. Continue.")
  
  if (nrow(existing_results) > 0 && !column_id %in% names(df_input)) {
    stop("Column '", column_id, "' not found in df_input.")
  }
  
  new_rows <- if (nrow(existing_results) > 0 && column_id %in% names(existing_results)) {
    existing_ids <- existing_results[[column_id]]
    df_input %>% filter(!(.data[[column_id]] %in% existing_ids))
  } else {
    df_input
  }
  
  pb <- progress_bar$new(total = nrow(new_rows), format = "[:bar] :percent :elapsed/:est_elapsed")
  
  results <- existing_results
  
  for (i in seq_len(nrow(new_rows))) {
    tryCatch({
      row_to_process <- new_rows[i, ]
      result <- process_row3(row_to_process, column_text, column_id, prompt_text, pb, model = model, show_input_output = show_input_output)
      if (!is.null(result)) {
        results <- bind_rows(results, result)
      }
      write.csv(results, checkpoint_path, row.names = FALSE)
    }, error = function(e) {
      cat("Error on row", i, ": ", e$message, "\n")
      id_errors <- c(id_errors, new_rows[i, column_id])
      write.csv(data.frame(id_errors), errors_path, row.names = FALSE)
    })
  }
  
  write.csv(results, checkpoint_path, row.names = FALSE)
  return(results)
}
