##     _    ____  _  __    _    ___
##    / \  / ___|| |/ /   / \  |_ _|
##   / _ \ \___ \| ' /   / _ \  | |
##  / ___ \ ___) | . \  / ___ \ | |
## /_/   \_\____/|_|\_\/_/   \_\___|
##
## Fearless (1.1.122024)

#' ASKAI: An R Interface to Ollama API
#' 
#' @description
#' ASKAI (Fearless 1.1.122024) provides a streamlined interface to interact with Ollama models
#' through R. It offers two main functions with varying levels of parameter control for
#' different use cases.
#' 
#' @author Eduardo Tamaki <eduardo.rtamaki@gmail.com>
#' @author Levente Littvay <levi@littvay.com>
#' 
#' @details
#' Package: ASKAI
#' Type: Package
#' Version: 1.1.122024
#' Date: 2024-12-20
#' License: MIT

## PREAMBLE --------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman") 

pacman::p_load(
  httr,       # Working with URLs and HTTP (1.4.7)
  jsonlite,   # JSON parser and generator (1.8.9)
  logger,     # Logging functionality
  pbapply,    # Progress bars
  crayon      # Colored terminal output
)

## GLOBAL CONFIGURATION -------------------------------------------------------
.askai_env <- new.env()
.askai_env$API_ENDPOINT <- "http://10.147.17.252:11434/api/generate"
.askai_env$LOG_DIR <- "~/.askai/logs"
.askai_env$DEBUG <- FALSE

# Initialize logging
if (!dir.exists(.askai_env$LOG_DIR)) dir.create(.askai_env$LOG_DIR, recursive = TRUE)
log_appender(appender_file(file.path(.askai_env$LOG_DIR, "askai.log")))

## AVAILABLE MODELS ----------------------------------------------------------
.askai_models <- list(
  "mistral-large:123b-instruct-2411-q8_0" = list(
    size = "131 GB",
    description = "Large Mistral model with high accuracy"
  ),
  "mistral-large:123b-instruct-2411-q5_K_M" = list(
    size = "86 GB",
    description = "Large Mistral model with high accuracy"
  ),
  "mistral-nemo:12b-instruct-2407-fp16" = list(
    size = "24 GB",
    description = "Efficient Mistral model"
  ),
  "llama3.2-vision:11b-instruct-fp16" = list(
    size = "21 GB",
    description = "Vision-capable LLaMA model"
  ),
  "marco-o1:7b-fp16" = list(
    size = "15 GB",
    description = "Compact general-purpose model"
  ),
  "llama3.3:70b-instruct-q8_0" = list(
    size = "74 GB",
    description = "Large instruction-tuned LLaMA model"
  ),
  "dolphin-llama3:70b-v2.9-q8_0" = list(
    size = "74 GB",
    description = "Fine-tuned LLaMA model"
  ),
  "qwen2.5:32b-instruct-fp16" = list(
    size = "65 GB",
    description = "Medium-sized instruction model"
  ),
  "llama3.1:405b-instruct-q3_K_S" = list(
    size = "175 GB",
    description = "Large instruction-tuned LLaMA model"
  ),
  "mistral-large:123b-instruct-2407-q5_K_M" = list(
    size = "86 GB",
    description = "Large Mistral model with high accuracy (old)"
  ),
  
  "mistral-small:22b-instruct-2409-fp16" = list(
    size = "44 GB",
    description = "Small Mistral model with high accuracy"
  )
)

## ERROR HANDLING -----------------------------------------------------------
.askai_errors <- list(
  MODEL_NOT_FOUND = "Model '%s' not found. Use list_models() to see available models.",
  INVALID_TEMPERATURE = "Temperature must be between 0 and 1",
  INVALID_TOP_P = "Top-p must be between 0 and 1",
  INVALID_TOP_K = "Top-k must be positive",
  INVALID_MAX_TOKENS = "Max tokens must be positive",
  SERVER_ERROR = "Failed to connect to Ollama server at %s",
  INVALID_MIROSTAT = "Mirostat must be 0, 1, or 2"
)

#' Enhanced Error Handler
#' @keywords internal
handle_ollama_error <- function(error, model = NULL) {
  log_error(error$message)
  
  if (grepl("connection refused", error$message, ignore.case = TRUE)) {
    stop(sprintf(.askai_errors$SERVER_ERROR, .askai_env$API_ENDPOINT))
  }
  
  if (!is.null(model) && !validate_model(model)) {
    stop(sprintf(.askai_errors$MODEL_NOT_FOUND, model))
  }
  
  stop(error$message)
}

## HELPER FUNCTIONS ---------------------------------------------------------
#' List Available Ollama Models
#' 
#' @return A data frame containing available models and their details
#' @export
list_models <- function() {
  models_df <- data.frame(
    Model = names(.askai_models),
    Size = sapply(.askai_models, function(x) x$size),
    Description = sapply(.askai_models, function(x) x$description),
    stringsAsFactors = FALSE
  )
  row.names(models_df) <- NULL
  return(models_df)
}

#' Validate Model Selection
#' @param model Character string of the model name
#' @return Logical indicating if model exists
#' @keywords internal
validate_model <- function(model) {
  valid <- model %in% names(.askai_models)
  if (!valid) log_warn(sprintf("Invalid model selected: %s", model))
  return(valid)
}

#' Validate Numeric Parameters
#' @param value Numeric value to validate
#' @param min Minimum allowed value
#' @param max Maximum allowed value
#' @param param_name Name of the parameter for error messages
#' @return Logical indicating if value is valid
#' @keywords internal
validate_numeric <- function(value, min, max, param_name) {
  if (!is.numeric(value) || value < min || value > max) {
    log_error(sprintf("Invalid %s: %s (should be between %s and %s)", 
                      param_name, value, min, max))
    return(FALSE)
  }
  return(TRUE)
}

## PROGRESS BAR -------------------------------------------------------------
#' Show Progress Bar for API Calls
#' @param expr Expression to evaluate with progress bar
#' @keywords internal
with_progress <- function(expr) {
  message("Sending request to Ollama...")
  result <- eval(expr)
  message("Request completed.")
  return(result)
}

#' Progress Bar Settings
#' @keywords internal
set_pb_style <- function() {
  style_pb <- progress::progress_bar$new(
    format = "[:bar] :percent eta: :eta",
    total = 100,
    clear = FALSE
  )
  return(style_pb)
}

## MAIN FUNCTIONS ----------------------------------------------------------
#' Base Ollama Query Function
#' Query any Ollama model with system and user content
#' Base function, simpler and easier to use.
#' @param system_content Character string containing system instructions
#' @param user_content Character string containing user message
#' @param model Character string specifying the Ollama model name
#' @param temperature Numeric between 0-1 controlling randomness
#' @param max_tokens Integer specifying maximum response length
#' @param top_p Numeric between 0-1 for nucleus sampling
#' @param top_k Integer for top-k sampling
#' @param repeat_penalty Numeric penalty for token repetition
#' @param presence_penalty Numeric penalty for token presence
#' @param frequency_penalty Numeric penalty for token frequency
#' @param stop List of strings where the model should stop generating
#' @param seed Integer for reproducibility
#' @return Character string containing the model's response
#' @inheritParams ask_ollama_plus
#' @export
ask_ollama <- function(system_content = NULL,
                               user_content,
                               model = "mistral",
                               temperature = 0.8,
                               max_tokens = 500,
                               top_p = 0.9,
                               top_k = 40,
                               repeat_penalty = 1.1,
                               presence_penalty = 0,
                               frequency_penalty = 0,
                               stop = NULL,
                               seed = NULL) {
  
  log_info(sprintf("Querying model %s", model))
  
  # Validate inputs
  if (!validate_model(model)) stop(sprintf(.askai_errors$MODEL_NOT_FOUND, model))
  if (!validate_numeric(temperature, 0, 1, "temperature")) stop(.askai_errors$INVALID_TEMPERATURE)
  if (!validate_numeric(top_p, 0, 1, "top_p")) stop(.askai_errors$INVALID_TOP_P)
  
  # Construct the prompt based on whether system content is provided
  if (!is.null(system_content)) {
    prompt <- sprintf("<|im_start|>system\n%s<|im_end|>\n<|im_start|>user\n%s<|im_end|>",
                      system_content, user_content)
  } else {
    prompt <- sprintf("<|im_start|>user\n%s<|im_end|>", user_content)
  }
  
  # Prepare the request body with all parameters
  body <- list(
    model = model,
    prompt = prompt,
    stream = FALSE,
    temperature = temperature,
    max_tokens = max_tokens,
    top_p = top_p,
    top_k = top_k,
    repeat_penalty = repeat_penalty,
    presence_penalty = presence_penalty,
    frequency_penalty = frequency_penalty
  )
  
  # Add optional parameters only if they're provided
  if (!is.null(stop)) body$stop <- stop
  if (!is.null(seed)) body$seed <- seed
  
  # Convert body to JSON
  body_json <- jsonlite::toJSON(body, auto_unbox = TRUE)
  
  log_debug(sprintf("Request body: %s", body_json))
  
  tryCatch({
    # Make the API call with progress indicator
    log_info("Sending request to Ollama API...")
    response <- with_progress({
      httr::POST(
        url = .askai_env$API_ENDPOINT,
        body = body_json,
        encode = "json",
        httr::content_type("application/json")
      )
    })
    
    # Check if request was successful
    if (httr::http_error(response)) {
      log_error(sprintf("API request failed: %s", httr::http_status(response)$message))
      stop("API request failed: ", httr::http_status(response)$message)
    }
    
    # Parse and return the response
    content <- httr::content(response, "text", encoding = "UTF-8")
    parsed <- jsonlite::fromJSON(content)
    
    log_info("Successfully received response from Ollama API")
    return(parsed$response)
    
  }, error = function(e) {
    handle_ollama_error(e, model)
  })
}


#' Advanced Ollama Query Function
#' #' Query any Ollama model with system and user content. 
#' "Plus" function: additional parameters for a more detailed use.
#' @param system_content Character string containing system instructions
#' @param user_content Character string containing user message
#' @param model Character string specifying the Ollama model name
#' @param temperature Numeric between 0-1 controlling randomness
#' @param max_tokens Integer specifying maximum response length
#' @param top_p Numeric between 0-1 for nucleus sampling
#' @param top_k Integer for top-k sampling
#' @param repeat_penalty Numeric penalty for token repetition
#' @param presence_penalty Numeric penalty for token presence
#' @param frequency_penalty Numeric penalty for token frequency
#' @param mirostat Integer (0,1,2) for Mirostat sampling version (default: 0, disabled)
#' @param mirostat_tau Numeric target entropy (default: 5.0)
#' @param mirostat_eta Numeric learning rate (default: 0.1)
#' @param num_ctx Integer for context window size
#' @param num_predict Integer for number of tokens to predict
#' @param num_threads Integer for number of threads to use during computation
#' @param num_gpu Integer for number of GPUs to use during computation
#' @param stop List of strings where the model should stop generating
#' @param seed Integer for reproducibility
#' @param raw_prompt Logical: if TRUE, sends prompt without template formatting
#' @param format String specifying response format (e.g., "json")
#' @param images List of base64-encoded images for multimodal models
#' @return Character string containing the model's response
#' @inheritParams ask_ollama
#' @export
ask_ollama_plus <- function(system_content = NULL,
                                    user_content,
                                    model = "mistral",
                                    temperature = 0.8,
                                    max_tokens = 500,
                                    top_p = 0.9,
                                    top_k = 40,
                                    repeat_penalty = 1.1,
                                    presence_penalty = 0,
                                    frequency_penalty = 0,
                                    mirostat = 0,
                                    mirostat_tau = 5.0,
                                    mirostat_eta = 0.1,
                                    num_ctx = NULL,
                                    num_predict = NULL,
                                    num_threads = NULL,
                                    num_gpu = NULL,
                                    stop = NULL,
                                    seed = NULL,
                                    raw_prompt = FALSE,
                                    format = NULL,
                                    images = NULL) {
  
  log_info(sprintf("Advanced query to model %s", model))
  
  # Validate inputs
  if (!validate_model(model)) stop(sprintf(.askai_errors$MODEL_NOT_FOUND, model))
  if (!validate_numeric(temperature, 0, 1, "temperature")) stop(.askai_errors$INVALID_TEMPERATURE)
  if (!validate_numeric(mirostat, 0, 2, "mirostat")) stop(.askai_errors$INVALID_MIROSTAT)
  
  # Construct the prompt based on whether system content is provided and raw_prompt setting
  if (raw_prompt) {
    prompt <- user_content
  } else if (!is.null(system_content)) {
    prompt <- sprintf("<|im_start|>system\n%s<|im_end|>\n<|im_start|>user\n%s<|im_end|>",
                      system_content, user_content)
  } else {
    prompt <- sprintf("<|im_start|>user\n%s<|im_end|>", user_content)
  }
  
  # Prepare the request body with all parameters
  body <- list(
    model = model,
    prompt = prompt,
    stream = FALSE,
    temperature = temperature,
    max_tokens = max_tokens,
    top_p = top_p,
    top_k = top_k,
    repeat_penalty = repeat_penalty,
    presence_penalty = presence_penalty,
    frequency_penalty = frequency_penalty,
    mirostat = mirostat,
    mirostat_tau = mirostat_tau,
    mirostat_eta = mirostat_eta
  )
  
  # Add optional parameters only if they're provided
  if (!is.null(num_ctx)) body$num_ctx <- num_ctx
  if (!is.null(num_predict)) body$num_predict <- num_predict
  if (!is.null(num_threads)) body$num_threads <- num_threads
  if (!is.null(num_gpu)) body$num_gpu <- num_gpu
  if (!is.null(stop)) body$stop <- stop
  if (!is.null(seed)) body$seed <- seed
  if (!is.null(format)) body$format <- format
  if (!is.null(images)) body$images <- images
  
  # Convert body to JSON
  body_json <- jsonlite::toJSON(body, auto_unbox = TRUE)
  
  log_debug(sprintf("Request body: %s", body_json))
  
  tryCatch({
    # Make the API call with progress indicator
    log_info("Sending request to Ollama API...")
    response <- with_progress({
      httr::POST(
        url = .askai_env$API_ENDPOINT,
        body = body_json,
        encode = "json",
        httr::content_type("application/json")
      )
    })
    
    # Check if request was successful
    if (httr::http_error(response)) {
      log_error(sprintf("API request failed: %s", httr::http_status(response)$message))
      stop("API request failed: ", httr::http_status(response)$message)
    }
    
    # Parse and return the response
    content <- httr::content(response, "text", encoding = "UTF-8")
    parsed <- jsonlite::fromJSON(content)
    
    log_info("Successfully received response from Ollama API")
    return(parsed$response)
    
  }, error = function(e) {
    handle_ollama_error(e, model)
  })
}


## PACKAGE STARTUP ---------------------------------------------------------
.onLoad <- function(libname, pkgname) {
  packageStartupMessage(crayon::blue("ASKAI - Fearless (1.0.122024)"))
  packageStartupMessage(crayon::green("Loading required packages..."))
}

## PACKAGE CLEANUP --------------------------------------------------------
.onUnload <- function(libpath) {
  log_info("Unloading ASKAI package")
}

## EXAMPLES ---------------------------------------------------------------
#' #' @examples
#' #' \dontrun{
#' #' # List available models
#'  models <- list_models()
#'  print(models)
#' #' 
#' #' # Basic usage
#' ask_ollama(
#'   system_content = "You are a helpful AI assistant.",
#'   user_content = "Tell me in one word who's Crimea is?",
#'   model = "mistral-large:123b-instruct-2411-q8_0",
#'   temperature = 1,
#'   max_tokens = 1000,
#'   stop = list("##", "End"),
#'   seed = 42
#' )
  #' 
#' # Advanced usage
#' 
#' ask_ollama_plus(
#' # Content parameters
#' system_content = "You are a multilingual political science expert, specializing in analyzing international relations through a quantitative lens.",
#' user_content = "Analyze the current state of EU-China relations in under 100 words.",
#' model = "mistral-nemo:12b-instruct-2407-fp16",
#' # Core generation parameters
#' temperature = 0.7,            # Slightly lower for more focused responses
#' max_tokens = 300,            # Comfortable limit for the analysis
#' 
#' # Sampling parameters
#' top_p = 0.92,                # Nucleus sampling threshold
#' top_k = 50,                  # Consider top 50 tokens
#' 
#' # Penalty parameters
#' repeat_penalty = 1.15,       # Slightly higher to avoid repetition
#' presence_penalty = 0.1,      # Small penalty for token presence
#' frequency_penalty = 0.1,     # Small penalty for token frequency
#' 
#' # Mirostat parameters (advanced sampling control)
#' mirostat = 2,               # Using Mirostat 2.0
#' mirostat_tau = 5.0,         # Target entropy
#' mirostat_eta = 0.1,         # Learning rate
#' 
#' # Resource utilization parameters
#' num_ctx = 4096,             # Context window size
#' num_predict = 100,          # Limit prediction to 100 tokens
#' 
#' 
#' # Generation control parameters
#' stop = list("##", "Analysis:", "End"),  # Stop markers
#' seed = 42,                  # For reproducibility
#' raw_prompt = FALSE,         # Use template formatting
#' format = "json"             # Get response in JSON format
#' )