  ###############################################################################
  # 0) Packages & Global Options
  ###############################################################################
  
  pacman::p_load(dplyr, tidyr, purrr, stringr, readr, lubridate, Amelia)
  
  
  ###############################################################################
  # 1) Helper: NGO Cleaning Function (used for both NGO sources)
  ###############################################################################
  
  clean_ngo <- function(df) {
    df %>%
      mutate(
        registration_date = dmy(registration_date),
        org_age           = as.numeric(Sys.Date() - registration_date) / 365.25,
        org_name          = str_squish(org_name),
        region            = str_squish(region),
        badges            = str_squish(as.character(badges)),
        capitals          = ifelse(region %in% c("Москва", "Санкт-Петербург"), 1, 0),
        ogrn              = as.numeric(ogrn),
        inn               = as.numeric(inn),
        supported_projects_numeric = as.numeric(supported_projects_numeric),
        sum_grants        = as.numeric(sum_grants_numeric)
      ) %>%
      # Hard‑coded badge dummies
      mutate(
        badge_president_grants = ifelse(
          str_detect(
            badges,
            stringr::fixed("Победитель конкурса Фонда президентских грантов", ignore_case = TRUE)
          ), 1, 0
        ),
        badge_president_foundation = ifelse(
          str_detect(
            badges,
            stringr::fixed("Победитель конкурса Президентского фонда культурных инициатив", ignore_case = TRUE)
          ), 1, 0
        ),
        badge_regional_grants = ifelse(
          str_detect(
            badges,
            stringr::fixed("Победитель регионального конкурса грантов для НКО", ignore_case = TRUE)
          ), 1, 0
        ),
        badge_top_project = ifelse(
          str_detect(
            badges,
            stringr::fixed("Проект(ы) в числе 100 лучших", ignore_case = TRUE)
          ), 1, 0
        ),
        badge_no_reporting = ifelse(
          str_detect(
            badges,
            stringr::fixed("На сайте Минюста России отсутствуют сведения", ignore_case = TRUE)
          ), 1, 0
        ),
        badge_project_failure = ifelse(
          str_detect(
            badges,
            stringr::fixed("Организация не реализовала проект", ignore_case = TRUE)
          ), 1, 0
        ),
        badge_liquidation = ifelse(
          str_detect(
            badges,
            stringr::fixed("Организация находится в процессе ликвидации", ignore_case = TRUE)
          ), 1, 0
        ),
        badge_foreign_agent = ifelse(
          str_detect(badges, stringr::fixed("Иностранный агент", ignore_case = TRUE)), 1, 0
        ),
        # Extract inclusion date for foreign agents (if present)
        inclusion_date = str_extract(
          badges,
          "(?<=Иностранный агент\\. Дата решения о включении: )\\d{2}\\.\\d{2}\\.\\d{4}"
        ),
        fa_start_date = dmy(inclusion_date)
      ) %>%
      # Remove raw / redundant columns if they exist
      dplyr::select(-any_of(c(
        "raw_details", "raw_region_registration", "whole_card", "href",
        "org_link_full", "page_number", "sum_grants_numeric"
      ))) %>%
      # One row per INN
      distinct(inn, .keep_all = TRUE)
  }
  
  ###############################################################################
  # 2) Read & Clean NGO Data (Main + by‑INN) and Combine
  ###############################################################################
  
  ngo_main <- read_csv("data/data_large/all_ngos_data_backup.csv")%>%
    clean_ngo()
  
  missing_inn_orgs <- read_csv("data/data_large/all_ngos_data_by_inn.csv") %>%
    clean_ngo()
  
  ngo_cleaned <- bind_rows(ngo_main, missing_inn_orgs) %>%
    distinct(inn, .keep_all = TRUE)
  
  ###############################################################################
  # 3) Read & Clean Projects / Applications Data
  ###############################################################################
  
  applications <- read_csv("data/data_large/all_projects_data_backup.csv") %>%
    rename(
      region             = city,
      competition        = contest,
      sum_requested      = project_price,
      total_project_cost = application,
      cofinancing        = fond_price
    ) %>%
    mutate(
      # Clean money fields: drop currency symbols and decimals
      cofinancing         = str_remove_all(cofinancing, "[₽\\s]") %>%
        str_replace_all(",", ".") %>%
        sub("\\..*", "", .),
      sum_requested       = str_remove_all(sum_requested, "[₽\\s]") %>%
        str_replace_all(",", ".") %>%
        sub("\\..*", "", .),
      total_project_cost  = str_remove_all(total_project_cost, "[₽\\s]") %>%
        str_replace_all(",", ".") %>%
        sub("\\..*", "", .),
      # application gets the last number from total_project_cost
      application         = gsub(".*?(\\d+)$", "\\1", total_project_cost)
    ) %>%
    # Convert competition strings into approximate dates
    mutate(
      year_extracted = as.numeric(str_extract(competition, "\\d{4}")),
      date = case_when(
        str_detect(competition, "Второй конкурс") ~ as.Date(
          paste(year_extracted, "07", "01", sep = "-")
        ),
        str_detect(competition, "Первый конкурс") ~ as.Date(
          paste(year_extracted, "01", "01", sep = "-")
        ),
        TRUE ~ as.Date(paste(year_extracted, "12", "01", sep = "-"))
      )
    ) %>%
    mutate(
      sum_requested      = as.numeric(sum_requested),
      cofinancing        = as.numeric(cofinancing),
      total_project_cost = as.numeric(total_project_cost),
      application        = as.numeric(application)
    ) %>%
    # Example outlier removal for Ленинградская область in last 6 months
    filter(
      !(
        region == "Ленинградская область" &
          date >= max(date, na.rm = TRUE) - months(6) &
          sum_requested > quantile(sum_requested, 0.99, na.rm = TRUE)
      )
    ) %>%
    mutate(
      project_status = dplyr::recode(
        project_status,
        "проект не получил поддержку" = 0,
        "победитель конкурса"         = 1,
        "на независимой экспертизе"   = NA_real_
      )
    )
  
  ###############################################################################
  # 4) Read INN Data & Join to Applications
  ###############################################################################
  
  inn <- read_csv("data/data_large/application_inn_backup.csv") %>%
    mutate(href = tolower(href)) %>%
    distinct(href, .keep_all = TRUE)

  applications <- applications %>%
    mutate(href = tolower(href)) %>%
    left_join(inn, by = "href")
  
  ###############################################################################
  # 5) Join NGO Data x  (by INN)
  ###############################################################################
  
  applications <- applications %>%
    mutate(inn = as.character(inn)) %>%
    left_join(
      ngo_cleaned %>% mutate(inn = as.character(inn)),
      by = "inn"
    ) %>%
    mutate(
      region = coalesce(region.x, region.y)
    ) %>%
    select(-any_of(c("region.x", "region.y")))
  
  ###############################################################################
  # 6) Read & Clean Contacts Data (Website Info) & Merge
  ###############################################################################
  
  contacts <- read_csv("data/data_large/application_contacts_backup.csv") %>%
    mutate(
      website_raw = str_extract(details, "(?<=Веб-сайт:).*"),
      website_raw = str_squish(website_raw),
      org_website = case_when(
        is.na(website_raw) ~ 0L,
        str_detect(website_raw, regex("нет", ignore_case = TRUE)) ~ 0L,
        TRUE ~ 1L
      )
    ) %>%
    group_by(href) %>%
    slice(1) %>%
    ungroup()
  
  applications <- applications %>%
    left_join(contacts %>% dplyr::select(href, org_website), by = "href")
  
  ###############################################################################
  # 7) Read & Clean Target Audience Data & Merge
  ###############################################################################
  
  target_audience <- read_csv("data/data_large/target_audience_2025-03-07large123b.csv") %>%
    dplyr::select(-raw_response) %>%                     # Drop irrelevant column
    rename(href = column_id) %>%                 # Key to match `href`
    mutate(href = as.character(href)) %>%
    distinct(href, .keep_all = TRUE) %>%         # Remove duplicates if any
    mutate(across(c(target_age, target_old, target_young, target_disability), as.numeric)) %>%
    mutate(target_age = if_else(target_age > 95 | target_age < 0, NA_real_, target_age)) %>%
    group_by(href) %>%
    arrange(rowSums(is.na(across(everything())))) %>%
    summarise(across(everything(), ~ reduce(.x, coalesce)), .groups = "drop")
  
  presi_variables <- applications %>%
    mutate(href = as.character(href)) %>%
    left_join(target_audience, by = "href")
  
  presi_variables <- presi_variables %>%
    mutate(
      target_age_raw = target_age,  # keep original
      target_age = case_when(
        target_age == -1 ~ NA_real_,
        target_age > 100 & target_age < 1950 ~ 99,
        target_age > 1950 ~ (2025 - target_age),
        TRUE ~ target_age
      )
    )
  
  ###############################################################################
  # 8) Attach reparsed budget period data
  ###############################################################################

  budget_period <- read_csv("data/data_large/reparsed_budget_period.csv", show_col_types = FALSE) %>%
    mutate(href = as.character(href)) %>%
    select(
      href,
      sum_requested,
      cofinancing,
      total_expenses,
      implementation_period,
      implementation_start,
      implementation_end,
      status_code,
      fetch_error,
      parsed_at
    ) %>%
    distinct(href, .keep_all = TRUE) %>%
    mutate(
      sum_requested = suppressWarnings(as.numeric(sum_requested)),
      cofinancing = suppressWarnings(as.numeric(cofinancing)),
      total_expenses = suppressWarnings(as.numeric(total_expenses)),
      implementation_start = as.Date(implementation_start),
      implementation_end = as.Date(implementation_end),
      status_code = suppressWarnings(as.integer(status_code)),
      fetch_error = as.character(fetch_error),
      parsed_at = as.POSIXct(parsed_at)
    )

  presi_variables <- presi_variables %>%
    select(-any_of(c(
      "sum_requested",
      "cofinancing",
      "cofinancing_share",
      "leverage",
      # "total_expenses",
      "implementation_period",
      "implementation_start",
      "implementation_end",
      "proj_budget"
    ))) %>%
    left_join(budget_period, by = "href") %>%
    mutate(
      implementation_length_months = case_when(
        !is.na(implementation_start) & !is.na(implementation_end) & implementation_end >= implementation_start ~
          time_length(interval(implementation_start, implementation_end), "months"),
        TRUE ~ NA_real_
      ),
      proj_budget = total_expenses
    )

  message(
    "Budget period join: ",
    sum(!is.na(presi_variables$implementation_start)),
    " rows with implementation dates."
  )

  budget_period_fields <- presi_variables %>%
    select(
      href,
      total_expenses,
      implementation_period,
      implementation_start,
      implementation_end,
      implementation_length_months
    )
  
  presi_variables <- presi_variables %>%
    mutate(
      cofinancing_share = if_else(total_expenses > 0,
                                  (cofinancing / total_expenses) * 100,
                                  NA_real_)
    )
  
  ###############################################################################
  # 9) Prepare for Imputation (types, bounds, Amelia)
  ###############################################################################
  
  presi_variables <- presi_variables %>%
    mutate(
      cofinancing         = as.numeric(cofinancing),
      application         = as.numeric(application),
      sum_grants          = as.numeric(sum_grants),
      org_age             = as.numeric(org_age),
      badge_foreign_agent = if_else(is.na(badge_foreign_agent), 0, badge_foreign_agent)
    )
  
  # Subset for imputation
  impute_data <- presi_variables %>%
    select(any_of(c(
      "href", "title",
      "competition", "direction", "region", "project_status",
      "sum_requested", "cofinancing", "org_age",
      "supported_projects", "sum_grants",
      "target_age", "target_old", "target_young", "target_disability",
      "badge_president_grants",
      "badge_president_foundation",
      "badge_regional_grants",
      "badge_top_project",
      "badge_no_reporting",
      "badge_project_failure",
      "badge_liquidation",
      "badge_foreign_agent",
      "org_website"
    )))
  
  # Bounds: variable index, type (1 = lower, 2 = upper), value
  bounds_mat <- rbind(
    c(which(names(impute_data) == "sum_requested"), 1, 0),    # sum_requested >= 0
    c(which(names(impute_data) == "cofinancing"),   1, 0),    # cofinancing >= 0
    c(which(names(impute_data) == "org_age"),       1, 0),    # org_age >= 0
    c(which(names(impute_data) == "sum_grants"),    1, 0),    # sum_grants >= 0
    c(which(names(impute_data) == "supported_projects"), 1, 0),
    c(which(names(impute_data) == "target_age"),    1, 0),    # target_age in [0,110]
    c(which(names(impute_data) == "target_age"),    2, 110)
  )
  
  amelia_fit <- amelia(
    x      = impute_data,
    m      = 1,  # single completed dataset
    idvars = c("href", "project_status", "title", "region", "competition", "direction", "cofinancing"),
    noms   = c(
      "badge_president_grants",
      "badge_president_foundation",
      "badge_regional_grants",
      "badge_top_project",
      "badge_no_reporting",
      "badge_project_failure",
      "badge_liquidation",
      "badge_foreign_agent"
    ),
    logs   = c("sum_requested", "org_age", "sum_grants", "target_age"),
    bounds = bounds_mat
  )
  
  # Ensure dummy variables remain 0/1 after imputation
  # NOTE: reference_svo and patriotic_values were removed from the pipeline;
  # keep this list in sync with the noms= argument passed to amelia() above.
  dummy_vars <- c(
    "badge_president_grants",
    "badge_president_foundation",
    "badge_regional_grants",
    "badge_top_project",
    "badge_no_reporting",
    "badge_project_failure",
    "badge_liquidation",
    "badge_foreign_agent",
    "org_website"
  )
  
  for (i in seq_along(amelia_fit$imputations)) {
    amelia_fit$imputations[[i]][dummy_vars] <-
      lapply(amelia_fit$imputations[[i]][dummy_vars], round)
  }
  
  # Use first imputed dataset as working version
  presi_variables <- amelia_fit$imputations[[1]]

  presi_variables <- presi_variables %>%
    left_join(budget_period_fields, by = "href") %>%
    mutate(proj_budget = total_expenses)
  
  # Rescale monetary variables
  presi_variables <- presi_variables %>%
    mutate(
      sum_requested_100k = sum_requested / 100000,
      sum_grants_100k    = sum_grants    / 100000
    )
  
  glimpse(presi_variables)
  
  ###############################################################################
  # 10) Attach Traditional Values Scores (30k + 100k) and Quality Scores
  ###############################################################################
  
  # Traditional values models
  
  d30 <- read.csv(
    "data/data_large/30k_processed_trad_vals.csv",
    stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8"
  ) %>%
    transmute(
      href = trimws(as.character(href)),
      trad_vals_30k = suppressWarnings(as.integer(traditional_values))
    ) %>%
    arrange(is.na(trad_vals_30k)) %>%
    distinct(href, .keep_all = TRUE)
  
  
  d100 <- read.csv(
    "data/data_large/100k_processed_trad_vals.csv",
    stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8"
  ) %>%
    rename(href = column_author_id, trad_vals = model_output) %>%
    transmute(
      href = trimws(as.character(href)),
      trad_vals_100k = suppressWarnings(as.integer(trad_vals))
    ) %>%
    arrange(is.na(trad_vals_100k)) %>%
    distinct(href, .keep_all = TRUE)
  
  presi_variables <- presi_variables %>%
    mutate(href = trimws(as.character(href))) %>%
    left_join(d30,  by = "href") %>%
    left_join(d100, by = "href") %>%
    mutate(
      trad_vals = coalesce(trad_vals_100k, trad_vals_30k),
      trad_vals = suppressWarnings(as.integer(trad_vals))
    ) %>%
    select(-trad_vals_30k, -trad_vals_100k)
  ###############################################################################
  # 10.x) Application quality: keep existing -> appl_quality_qwen4_trained,
  #      then attach new appl_quality from appl_qual_120b.csv
  ###############################################################################

  # 1) Preserve the old quality column (if present)
  presi_variables <- presi_variables %>%
    rename(appl_quality_qwen4_trained = dplyr::any_of("appl_quality"))
  
  # 2) Read the new file: require EXACT column name appl_quality
  qual120b <- readr::read_csv(
    "data/data_large/appl_qual_120b.csv",
    show_col_types = FALSE
  ) %>%
    mutate(href = normalize_href(href)) %>%
    dplyr::select(href, appl_quality) %>%                 # EXACTLY appl_quality
    mutate(appl_quality = suppressWarnings(as.numeric(appl_quality))) %>%
    filter(!is.na(href) & href != "") %>%
    group_by(href) %>%
    summarise(appl_quality = dplyr::first(na.omit(appl_quality)), .groups = "drop")
  
  # 3) Join into main
  presi_variables <- presi_variables %>%
    mutate(href = normalize_href(href)) %>%
    left_join(qual120b, by = "href")
  
  # Drop implausible funding-request outliers
  presi_variables <- presi_variables %>%
    filter(sum_requested <= 1e9 | is.na(sum_requested))

  ###############################################################################
  # 11) Patch presi_granti_imputed with last competition (1H2025) outcomes
  #     Key = href (Id(key) is href)
  #     Run this RIGHT BEFORE you write/save presi_granti_imputed
  ###############################################################################
  
  normalize_href <- function(x) {
    x <- as.character(x)
    x <- stringr::str_trim(x)
    x
  }
  
  last_comp_2025h1 <- read.csv(
    "data/data_large/application_results_2025_r1.csv", sep=',' ) %>%
    mutate(
      href = normalize_href(href),
      project_status = dplyr::recode(
        status_text,
        "проект не получил поддержку" = 0,
        "победитель конкурса"         = 1,
        "на независимой экспертизе"   = NA_real_,
        .default = NA_real_
      )
    ) %>%
    dplyr::select(href,project_status) %>%
    group_by(href) %>%
    ungroup()

  
  presi_variables <- presi_variables %>%
    left_join(
      last_comp_2025h1 %>%
        dplyr::rename(project_status_new = project_status),
      by = "href"
    ) %>%
    mutate(
      project_status = dplyr::if_else(!is.na(project_status_new), project_status_new, project_status)
    ) %>%
    dplyr::select(-project_status_new)
  
  
  
  ###############################################################################
  # 11) Has-trad indicator
  ###############################################################################

  presi_variables <- presi_variables %>%
    mutate(has_trad = as.integer(trad_vals > 0))

  ###############################################################################
  # 12) Co-financing cleaning
  ###############################################################################

  presi_variables <- presi_variables %>%
    mutate(
      cofinancing_raw = proj_budget - sum_requested,
      cofinancing_share_raw = ifelse(
        sum_requested > 0 & !is.na(sum_requested),
        100 * (proj_budget - sum_requested) / sum_requested,
        NA_real_
      )
    ) %>%
    filter(
      !is.na(sum_requested),
      !is.na(proj_budget),
      sum_requested > 0,
      proj_budget >= sum_requested,
      cofinancing_share_raw >= 0,
      cofinancing_share_raw <= 10000
    ) %>%
    mutate(
      cofinancing = cofinancing_raw,
      cofinancing_share = cofinancing_share_raw
    ) %>%
    select(-cofinancing_raw, -cofinancing_share_raw)

  ###############################################################################
  # 13) Save Final Analysis Dataset
  ###############################################################################

  write_csv(presi_variables, "data/data_large/presi_variables_imputed.csv")
