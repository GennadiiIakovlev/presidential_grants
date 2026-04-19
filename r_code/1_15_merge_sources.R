  ###############################################################################
  # 0) Packages & Global Options
  ###############################################################################
  
  pacman::p_load(
    MASS, dplyr, tidyr, purrr, progress, tidyverse, stringi, stringr, readr, httr,
    tibble, lubridate, margins, car, pscl, detectseparation, ggplot2, ggrepel,
    zoo, writexl, stringdist, fuzzyjoin, readxl, openxlsx, caret, stats, scales,
    tm, wordcloud, RColorBrewer, quanteda, quanteda.textstats, readtext, Hmisc,
    Rcpp, naniar, psych, lavaan, xtable, mice, tidymodels, UpSetR, Amelia, dotwhisker
  )
  
  
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
  ############################################################################
  # Application quality model outputs
  #############################################################################
  # qual <- read.csv("data/data_large/qualty_qwen4bft.csv", sep = ",")
  # 
  # presi_variables <- presi_variables %>%
  #   left_join(qual, by = "href")
  # 
  ###############################################################################
  # 10.x) Application quality: keep existing -> appl_quality_qwen4_trained,
  #      then attach new appl_quality from appl_qual_120b.csv
  ###############################################################################
  }
  
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
  
  # keep your existing filter
  presi_variables <- presi_variables %>%
    filter(sum_requested <= 1e9 | is.na(sum_requested))
  
  
  ############################################################################
  #Getting rid of outliers with errors 
  presi_variables<- presi_variables %>% 
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
  
  
  
  # ###############################################################################
  # # 11) Factor / Date Variables, Competition Index, has_trad
  # ###############################################################################
   presi_variables<-read.csv("data/data_large/presi_variables_imputed.csv")
  # 
  # presi_variables <- presi_variables %>%
  #   filter(!str_detect(competition, stringr::fixed("Специальный", ignore_case = TRUE))) %>%
  #   filter(!str_detect(competition, stringr::fixed("2025", ignore_case = TRUE)))
  # 
  # 
  # 
  # # Direction: base category and youth shortening
  # presi_variables <- presi_variables %>%
  #   mutate(
  #     direction = as.character(direction),
  #     direction = case_when(
  #       direction == "Поддержка молодежных проектов, реализация которых охватывает виды деятельности, предусмотренные статьей 31.1 Федерального закона от 12 января 1996 г. № 7-ФЗ «О некоммерческих организациях»" ~ "Youth projects",
  #       TRUE ~ direction
  #     ),
  #     direction = factor(direction)
  #   )
  # 
  # presi_variables$direction <- relevel(
  #   presi_variables$direction,
  #   ref = "Поддержка проектов в области культуры и искусства"
  # )
  # 
  # # Date (recompute, since Amelia dropped earlier date column)
  # presi_variables <- presi_variables %>%
  #   mutate(
  #     year_extracted = as.numeric(str_extract(competition, "\\d{4}")),
  #     date = case_when(
  #       str_detect(competition, "Второй конкурс") ~ as.Date(
  #         paste(year_extracted, "07", "01", sep = "-")
  #       ),
  #       str_detect(competition, "Первый конкурс") ~ as.Date(
  #         paste(year_extracted, "01", "01", sep = "-")
  #       ),
  #       TRUE ~ as.Date(paste(year_extracted, "12", "01", sep = "-"))
  #     )
  #   )
  # 
  # # Ordered competition factor and numeric index
  # ordered_competitions <- c(
  #   "Первый конкурс 2017",  "Второй конкурс 2017",
  #   "Первый конкурс 2018",  "Второй конкурс 2018",
  #   "Первый конкурс 2019",  "Второй конкурс 2019",
  #   "Первый конкурс 2020",  "Второй конкурс 2020",
  #   "Первый конкурс 2021",  "Второй конкурс 2021",
  #   "Первый конкурс 2022",  "Второй конкурс 2022",
  #   "Первый конкурс 2023",  "Второй конкурс 2023",
  #   "Первый конкурс 2024",  "Второй конкурс 2024",
  #   "Первый конкурс 2025",  "Специальный конкурс 2020",
  #   "Специальный конкурс 2022"
  # )
  # 
   presi_variables <- presi_variables %>%
     mutate(
       # competition = factor(competition, levels = ordered_competitions, ordered = TRUE),
       # competition_index = as.numeric(competition),
       has_trad = as.integer(trad_vals > 0)
     )
   
   presi_variables <- presi_variables %>%
     # Base calcs
     mutate(
       cofinancing_raw = proj_budget - sum_requested,
       cofinancing_share_raw = ifelse(sum_requested > 0 & !is.na(sum_requested), 
                                      100 * (proj_budget - sum_requested) / sum_requested, 
                                      NA_real_)
     ) %>%
     # Clean: drop NAs, negatives, implausibles (>10000% share)
     filter(
       !is.na(sum_requested),
       !is.na(proj_budget),
       sum_requested > 0,
       proj_budget >= sum_requested,  # Logical: total >= requested
       cofinancing_share_raw >= 0,
       cofinancing_share_raw <= 10000  # Cap crazy outliers
     ) %>%
     mutate(
       # Final clean columns
       cofinancing = cofinancing_raw,
       cofinancing_share = cofinancing_share_raw
     ) %>%
     select(-cofinancing_raw, -cofinancing_share_raw)
   
   # Verify: no NA/negatives
   cat("Cleaned N:", nrow(presi_variables), "\n")
   summary(presi_variables$cofinancing)
   summary(presi_variables$cofinancing_share)
   
   # Quick check
   table(presi_variables$cofinancing < 0, useNA = "always")
   table(is.na(presi_variables$cofinancing_share), useNA = "always")
   
   library(dplyr)
   library(ggplot2)
   library(naniar)
   library(Hmisc)
   library(corrplot)
   library(scales)
   
   # 1) Winsorize extreme outliers (top 1%)
   top_p <- 0.99
   presi_win <- presi_variables %>%
     mutate(
       across(c(sum_requested, proj_budget, cofinancing), 
              ~ pmin(pmax(., 0), quantile(., top_p, na.rm = TRUE))),
       cofinancing_share_win = pmin(pmax(cofinancing_share, 0), quantile(cofinancing_share, top_p, na.rm = TRUE))
     )
   
   # 2) Enhanced summaries
   money_vars <- c("sum_requested", "proj_budget", "cofinancing", "cofinancing_share", 
                   "cofinancing_share_win")
   print("Winsorized (99th percentile) summaries:")
   presi_win %>% select(all_of(money_vars)) %>% descr()
   
   # 3) Improved plots: log-scale + density + rug for skew
   p_money <- presi_win %>%
     pivot_longer(all_of(money_vars), names_to = "var", values_to = "value") %>%
     ggplot(aes(value)) +
     geom_histogram(aes(y = after_stat(density)), bins = 80, fill = "steelblue", alpha = 0.7) +
     geom_rug(alpha = 0.3, sides = "b") +
     facet_wrap(~ var, scales = "free_y", ncol = 2) +
     scale_x_log10(labels = comma_format(accuracy = 1)) +
     labs(title = "Monetary Variables: Density Plots (Log Scale, Rug for Density)",
          subtitle = paste("Winsorized at", top_p*100, "percentile; extreme right skew mitigated")) +
     theme_minimal(base_size = 11)
   print(p_money)
   ggsave("improved_money_distributions.png", p_money, dpi = 300, width = 12, height = 8)
   
   # 4) Co-financing by outcome (winners vs losers)
   p_outcome <- presi_win %>%
     filter(!is.na(project_status)) %>%
     ggplot(aes(x = factor(project_status), y = cofinancing_share_win)) +
     geom_violin(alpha = 0.6, fill = "lightblue") +
     geom_boxplot(width = 0.2, alpha = 0.8) +
     stat_summary(fun = "mean", geom = "point", size = 3, color = "red") +
     labs(title = "Co-financing Share: Winners vs Losers",
          x = "Project Status (0=No, 1=Yes)", y = "Co-financing Share (%) Winsorized") +
     theme_minimal()
   print(p_outcome)
   
   # 5) Outlier count & examples
   outliers <- presi_variables %>%
     filter(cofinancing > quantile(cofinancing, top_p, na.rm = TRUE) | 
              cofinancing_share > quantile(cofinancing_share, top_p, na.rm = TRUE)) %>%
     select(href, title, cofinancing, cofinancing_share, project_status)
   cat("Outliers (>", top_p*100, "percentile):", nrow(outliers), "cases\n")
   print(head(outliers, 10))
   
   # 6) Correlation matrix (winsorized)
   num_win <- sapply(presi_win, is.numeric)
   cormat <- cor(presi_win[, num_win], use = "pairwise.complete.obs")
   corrplot(cormat, method = "color", type = "upper", order = "hclust", 
            tl.cex = 0.7, title = "Winsorized Correlation Matrix")
   
   # 7) Recommendation for modeling
   cat("\nRecommendations:\n")
   cat("- Use log(cofinancing + 1) or winsorized in regressions\n")
   cat("- cofinancing_share_win stable (mean", round(mean(presi_win$cofinancing_share_win, na.rm=T), 2), "%)\n")
   cat("- Winners:", sum(presi_win$project_status == 1, na.rm=T), 
       "Losers:", sum(presi_win$project_status == 0, na.rm=T), "\n")
   
  # describe(presi_variables$has_trad)
  # 
  # 
  # 
  # Hmisc::describe(presi_variables$has_trad)
  # 
  # ###############################################################################
  # # 12) Basic Diagnostics: Structure, Missingness, Descriptive Stats
  # ###############################################################################
  # 
  # cat("Number of rows:", nrow(presi_variables), "\n")
  # cat("Number of columns:", ncol(presi_variables), "\n")
  # 
  # glimpse(presi_variables)
  # 
  # # Missingness overview
  # naniar::pct_miss_var(presi_variables)
  # 
  # vis_miss(presi_variables, warn_large_data = FALSE) +
  #   ggtitle("Missingness Pattern in presi_variables")
  # 
  # # Descriptive stats per variable
  # numeric_cols_to_plot <- c(
  #   "sum_requested",
  #   "sum_grants",
  #   "cofinancing",
  #   "total_project_cost",
  #   "application",
  #   "org_age",
  #   "target_age"
  # )
  # 
  # for (colname in numeric_cols_to_plot) {
  #   if (colname %in% names(presi_variables)) {
  #     p <- ggplot(presi_variables, aes(x = .data[[colname]])) +
  #       geom_histogram(bins = 30) +
  #       labs(
  #         title = paste("Distribution of", colname),
  #         x = colname,
  #         y = "Count"
  #       )
  #     print(p)
  #   }
  # }
  # 
  # # Summary table for LaTeX
  # numeric_idx <- vapply(presi_variables, is.numeric, logical(1))
  # 
  # descriptive_stats <- data.frame(
  #   Variable = names(presi_variables),
  #   Mean     = sapply(presi_variables, function(x) if (is.numeric(x)) mean(x, na.rm = TRUE) else NA_real_),
  #   Median   = sapply(presi_variables, function(x) if (is.numeric(x)) median(x, na.rm = TRUE) else NA_real_),
  #   SD       = sapply(presi_variables, function(x) if (is.numeric(x)) sd(x, na.rm = TRUE) else NA_real_),
  #   Min      = sapply(presi_variables, function(x) if (is.numeric(x)) min(x, na.rm = TRUE) else NA_real_),
  #   Max      = sapply(presi_variables, function(x) if (is.numeric(x)) max(x, na.rm = TRUE) else NA_real_)
  # )
  # 
  # latex_table <- xtable(
  #   descriptive_stats,
  #   caption = "Descriptive Statistics for presi_variables Dataset",
  #   label   = "tab:desc_stats"
  # )
  # 
  # print(latex_table, include.rownames = FALSE, caption.placement = "top")
  # 
  # ###############################################################################
  # # 13) Save Final Analysis Dataset
  # ###############################################################################
    presi_variables<-read.csv("data/data_large/presi_variables_imputed.csv")
  
     write_csv(presi_variables, "data/data_large/presi_variables_imputed.csv")
  # 
  # ###############################################################################
  # # 14) Exploratory Plots for Traditional Values
  # ###############################################################################
  # 
  # # Age vs probability of traditional values (any trad_vals == 1)
  # 
  # presi_variables %>%
  #   filter(!is.na(target_age), target_age <= 95) %>%
  #   ggplot(aes(target_age, has_trad)) +
  #   geom_jitter(height = .02, width = 0, alpha = .12) +
  #   geom_smooth(method = "loess", formula = y ~ x,
  #               se = FALSE, span = .4, linewidth = 1.2) +
  #   scale_y_continuous(labels = percent_format(1), limits = c(0, 1)) +
  #   labs(x = "Age", y = "Probability of traditional values") +
  #   theme_minimal(base_size = 13)
  # 
  # # Trad share across competition index
  # presi_variables %>%
  #   filter(!is.na(competition_index), !is.na(has_trad)) %>%
  #   ggplot(aes(competition_index, has_trad)) +
  #   geom_jitter(height = .02, width = 0, alpha = .05, size = 0.6) +
  #   geom_smooth(
  #     method = "glm",
  #     method.args = list(family = "binomial"),
  #     se = FALSE,
  #     linewidth = 1.2
  #   ) +
  #   scale_y_continuous(
  #     labels = percent_format(accuracy = 1),
  #     breaks  = seq(0, 0.25, by = 0.05)
  #   ) +
  #   coord_cartesian(ylim = c(0, 0.25)) +
  #   labs(
  #     x = "Semesters since 2017",
  #     y = "Probability of traditional values"
  #   ) +
  #   theme_minimal(base_size = 13)
  # 
  # # Collapsed bar: P(trad) by age
  # presi_variables %>%
  #   filter(target_age <= 95) %>%
  #   mutate(trad_values_bin = if_else(trad_vals > 0, 1L, 0L)) %>%
  #   group_by(target_age) %>%
  #   summarise(
  #     prob_trad = mean(trad_values_bin, na.rm = TRUE),
  #     n         = n(),
  #     .groups   = "drop"
  #   ) %>%
  #   ggplot(aes(x = target_age, y = prob_trad)) +
  #   geom_col(width = 1, colour = "grey30") +
  #   scale_y_continuous(
  #     labels = percent_format(accuracy = 1),
  #     expand = expansion(mult = c(0, .02))
  #   ) +
  #   labs(
  #     x = "Target age",
  #     y = "Probability of any traditional values",
  #     title = "Trad‑values prevalence by age"
  #   ) +
  #   theme_minimal()
  # 
  # ###############################################################################
  # # 15) Main Logistic Regression: Effect of Application Quality
  # ###############################################################################
  # 
  # # Build modelling subset (avoid global na.omit)
  # 
  # presi_model <- presi_variables %>%
  #   mutate(
  #     cofinancing_share = (sum_requested / cofinancing) * 100
  #   ) %>%
  #   filter(!is.na(project_status)) %>%
  #   drop_na(
  #     appl_quality,
  #     supported_projects,
  #     cofinancing_share,
  #     org_age,
  #     badge_president_grants,
  #     badge_president_foundation,
  #     badge_regional_grants,
  #     badge_top_project,
  #     badge_no_reporting,
  #     badge_project_failure,
  #     org_website,
  #     target_age,
  #     target_disability,
  #     has_trad,
  #     direction,
  #     competition_index,
  #     region
  #   )
  # 
  # hist(presi_model$appl_quality)
  # Hmisc::describe(presi_model$appl_quality)
  # 
  # final_model <- glm(
  #   project_status ~ appl_quality +
  #     supported_projects +
  #     cofinancing_share +
  #     org_age +
  #     badge_president_grants +
  #     badge_president_foundation +
  #     badge_regional_grants +
  #     badge_top_project +
  #     badge_no_reporting +
  #     badge_project_failure +
  #     org_website +
  #     as.numeric(target_age) +
  #     target_disability +
  #     has_trad +
  #     factor(direction) +
  #     competition_index +
  #     factor(region),
  #   data   = presi_model,
  #   family = binomial
  # )
  # 
  # summary(final_model)
  # car::vif(final_model)
  # 
  # # Marginal effects
  # marginal_effects <- margins(final_model)
  # summary(marginal_effects)
  # 
  # # Pseudo R^2
  # pseudo_rsqs <- pR2(final_model)
  # print(pseudo_rsqs)
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # #############################################################################
  # #THE END OF REGRESSION ANALYSISS AND THE BEGINNING OF ADDTIONAL TESTS########
  #   #############################################################################
  # ###############################################################################
  # # 16) Parametric Bootstrap Plot for Application Quality Effect
  # ###############################################################################
  # 
  # # 1) Grid for appl_quality and average predicted probability
  # 
  # xr_all <- range(presi_model$appl_quality, na.rm = TRUE)
  # grid_x <- seq(xr_all[1], xr_all[2], length.out = 150)
  # 
  # avg <- sapply(grid_x, function(x) {
  #   tmp <- presi_model
  #   tmp$appl_quality <- x
  #   mean(predict(final_model, newdata = tmp, type = "response"), na.rm = TRUE)
  # })
  # 
  # avg_df <- data.frame(appl_quality = grid_x, prob = avg)
  # 
  # # 2) 95% CI ribbon via parametric bootstrap of coefficients
  # set.seed(123)
  # B    <- 400
  # beta <- coef(final_model)
  # V    <- vcov(final_model)
  # X    <- model.matrix(final_model)
  # col_appl <- which(colnames(X) == "appl_quality")
  # 
  # X0 <- X
  # X0[, col_appl] <- 0  # offset with appl_quality set to 0
  # 
  # boot <- replicate(B, {
  #   b     <- MASS::mvrnorm(1, beta, V)
  #   off   <- as.vector(X0 %*% b)
  #   slope <- b[col_appl]
  #   sapply(grid_x, function(x) mean(plogis(off + x * slope)))
  # })
  # 
  # avg_df$lo <- apply(boot, 1, quantile, 0.025)
  # avg_df$hi <- apply(boot, 1, quantile, 0.975)
  # 
  # # 3) Binned observed win rates (calibration points)
  # 
  # binned <- presi_model %>%
  #   transmute(x = appl_quality, y = project_status) %>%
  #   filter(!is.na(x), !is.na(y)) %>%
  #   mutate(bin = scales::cut_number(x, 20)) %>%
  #   group_by(bin) %>%
  #   summarise(x = mean(x), y = mean(y), n = n(), .groups = "drop")
  # 
  # # 4) Central 80% shading and a 40‑point contrast
  # 
  # p10 <- quantile(presi_model$appl_quality, 0.10, na.rm = TRUE)
  # p90 <- quantile(presi_model$appl_quality, 0.90, na.rm = TRUE)
  # 
  # x0 <- as.numeric(p10)
  # x1 <- pmin(pmax(x0 + 40, xr_all[1]), xr_all[2])
  # 
  # p0 <- approx(avg_df$appl_quality, avg_df$prob, xout = x0)$y
  # p1 <- approx(avg_df$appl_quality, avg_df$prob, xout = x1)$y
  # 
  # ix0   <- which.min(abs(grid_x - x0))
  # ix1   <- which.min(abs(grid_x - x1))
  # delta <- boot[ix1, ] - boot[ix0, ]
  # ci40  <- quantile(delta, c(.025, .975))
  # lab   <- sprintf("+40 pts: %+0.1f pp (95%% CI %+0.1f, %+0.1f)",
  #                  100 * (p1 - p0), 100 * ci40[1], 100 * ci40[2])
  # 
  # # 5) Plot
  # 
  # ggplot(avg_df, aes(appl_quality, prob)) +
  #   geom_rect(
  #     data = data.frame(xmin = p10, xmax = p90, ymin = -Inf, ymax = Inf),
  #     aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
  #     inherit.aes = FALSE, alpha = 0.06
  #   ) +
  #   geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.2) +
  #   geom_line(linewidth = 1) +
  #   geom_point(data = binned, aes(x = x, y = y, size = n),
  #              inherit.aes = FALSE, alpha = 0.6) +
  #   scale_size_continuous(range = c(1.5, 4), guide = "none") +
  #   geom_segment(aes(x = x0, xend = x1, y = p0, yend = p1), linetype = 2) +
  #   geom_point(aes(x = x0, y = p0)) +
  #   geom_point(aes(x = x1, y = p1)) +
  #   geom_label(aes(x = x1, y = p1, label = lab), hjust = 0, vjust = -0.5) +
  #   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  #   labs(
  #     x = "Application quality",
  #     y = "Average predicted chance of winning",
  #     title = "Predicted probability vs. application quality",
  #     subtitle = "Solid: model average; points: binned observed win rates; ribbon: 95% parametric bootstrap",
  #     caption = "Shaded area = central 80% of application quality (data support)"
  #   ) +
  #   theme_minimal(base_size = 12) +
  #   theme(panel.grid.minor = element_blank())
  # 
  # beta_appl <- coef(final_model)["appl_quality"]
  # cat(sprintf("OR for +40 points in appl_quality: %.3f\n", exp(40 * beta_appl)))
  # 
  # ###############################################################################
  # # 17) Competition‑level Lagged Crowd‑ing Variable (Alternative Spec)
  # ###############################################################################
  # 
  # # Summarise number of applications per (direction, competition_index)
  # 
  # direction_competition_counts <- presi_variables %>%
  #   group_by(direction, competition_index) %>%
  #   summarise(n_apps_dir_competition = n(), .groups = "drop") %>%
  #   arrange(direction, competition_index) %>%
  #   group_by(direction) %>%
  #   mutate(n_apps_dir_competition_lag = lag(n_apps_dir_competition, 1)) %>%
  #   ungroup()
  # 
  # presi_variables_lagged <- presi_variables %>%
  #   left_join(
  #     direction_competition_counts %>%
  #       select(direction, competition_index, n_apps_dir_competition_lag),
  #     by = c("direction", "competition_index")
  #   ) %>%
  #   mutate(
  #     n_apps_dir_competition_lag = ifelse(is.na(n_apps_dir_competition_lag), 0, n_apps_dir_competition_lag),
  #     n_apps_dir_competition_lag_k = n_apps_dir_competition_lag / 1000
  #   )
  # 
  # model_with_lag <- glm(
  #   project_status ~
  #     sum_requested_100k +
  #     supported_projects +
  #     sum_grants_100k +
  #     org_age +
  #     badge_president_grants +
  #     badge_president_foundation +
  #     badge_regional_grants +
  #     badge_top_project +
  #     badge_no_reporting +
  #     badge_project_failure +
  #     org_website +
  #     as.numeric(target_age) +
  #     target_disability +
  #     factor(direction) +
  #     factor(competition) +
  #     factor(region) +
  #     n_apps_dir_competition_lag_k,
  #   family = binomial,
  #   data   = presi_variables_lagged
  # )
  # 
  # summary(model_with_lag)
  # 
  # # Tidy without region dummies
  # model_tidy_masked <- broom::tidy(model_with_lag) %>%
  #   dplyr::filter(!str_detect(term, "factor\\(region\\)"))
  # 
  # print(model_tidy_masked, n = 100)
  # 
  # ###############################################################################
  # # 18) Environment / Animal‑protection Direction Share over Time
  # ###############################################################################
  # 
  # target_dir <- "Охрана окружающей среды и защита животных"
  # 
  # stage_order <- c("1st", "2nd", "Special", "3rd", "4th", "Other")
  # 
  # share_df <- presi_variables %>%
  #   filter(!is.na(competition), !is.na(direction), !is.na(year_extracted)) %>%
  #   mutate(direction = str_squish(as.character(direction))) %>%
  #   group_by(competition, year_extracted) %>%
  #   summarise(share = mean(direction == target_dir, na.rm = TRUE), .groups = "drop") %>%
  #   mutate(
  #     stage_en = case_when(
  #       str_detect(competition, regex("Первый", ignore_case = TRUE)) ~ "1st",
  #       str_detect(competition, regex("Второй", ignore_case = TRUE)) ~ "2nd",
  #       str_detect(competition, regex("Третий", ignore_case = TRUE)) ~ "3rd",
  #       str_detect(competition, regex("Четв",   ignore_case = TRUE)) ~ "4th",
  #       str_detect(competition, regex("Спец",   ignore_case = TRUE)) ~ "Special",
  #       TRUE ~ "Other"
  #     ),
  #     stage_en = factor(stage_en, levels = stage_order),
  #     x_label  = paste(stage_en, year_extracted, sep = "\n")
  #   ) %>%
  #   arrange(year_extracted, stage_en) %>%
  #   mutate(x_label = factor(x_label, levels = unique(x_label)))
  # 
  # upper <- min(1, max(0.2, max(share_df$share, na.rm = TRUE) * 1.1))
  # 
  # ggplot(share_df, aes(x = x_label, y = share)) +
  #   geom_segment(aes(xend = x_label, y = 0, yend = share)) +
  #   geom_point(size = 2) +
  #   scale_y_continuous(labels = percent_format(accuracy = 1),
  #                      limits = c(0, upper), expand = expansion(mult = c(0, 0.05))) +
  #   labs(
  #     title    = "Share of \"Environment Protection & Animal Welfare\" by Competition",
  #     subtitle = "Compact labels; ordered by year",
  #     x        = "Competition (stage\nyear)",
  #     y        = "Share of projects"
  #   ) +
  #   theme_minimal(base_size = 12) +
  #   theme(axis.text.x = element_text(vjust = 1, hjust = 0.5),
  #         panel.grid.minor = element_blank())
  # 
  # ###############################################################################
  # # 19) Export Subset of Winners with Websites (for external scraping etc.)
  # ###############################################################################
  # 
  # # Build a subset where: trad‑values present, org has website, and direction is not
  # # "Сохранение исторической памяти".
  # 
  # presi_subset <- presi_variables %>%
  #   filter(
  #     has_trad == 1,
  #     org_website == 1,
  #     as.character(direction) != "Сохранение исторической памяти"
  #   ) %>%
  #   mutate(href = str_trim(href))
  # 
  # # Website lookup table from contacts
  # contacts_web <- read_csv("data/data_large/application_contacts_backup.csv") %>%
  #   mutate(
  #     href        = str_trim(href),
  #     website_raw = str_squish(str_extract(details, "(?<=Веб-сайт:).*")),
  #     website     = str_extract(
  #       website_raw,
  #       regex("(https?://[^\\s,;\\)\\]]+|www\\.[^\\s,;\\)\\]]+|vk\\.com/[^\\s,;\\)\\]]+)", ignore_case = TRUE)
  #     ),
  #     website = case_when(
  #       is.na(website) ~ NA_character_,
  #       str_detect(website, "^https?://") ~ website,
  #       TRUE ~ str_c("https://", website)
  #     )
  #   ) %>%
  #   group_by(href) %>%
  #   slice(1) %>%
  #   ungroup()
  # 
  # presi_subset <- presi_subset %>%
  #   left_join(contacts_web, by = "href") %>%
  #   arrange(
  #     desc(coalesce(str_detect(website, regex("\\bvk\\.com", ignore_case = TRUE)), FALSE)),
  #     website
  #   )
  # 
  # write_csv(presi_subset, "data/data_large/presi_subset_iskren.csv")
  # 
  # ###############################################################################
  # # END OF SCRIPT
  # ###############################################################################
  # 
  # library(dplyr)
  # library(purrr)
  # library(broom)
  # library(ggplot2)
  # 
  # # 0) Lookup table: which competition_index corresponds to which year
  # comp_year <- presi_model %>%
  #   distinct(competition_index, year_extracted)
  # 
  # # 1) Compute SEs for appl_quality and cofinancing_share by competition_index
  # se_time <- presi_model %>%
  #   group_by(competition_index) %>%
  #   group_modify(~ {
  #     m <- glm(
  #       project_status ~ appl_quality +
  #         supported_projects +
  #         cofinancing_share +
  #         org_age +
  #         badge_president_grants +
  #         badge_president_foundation +
  #         badge_regional_grants +
  #         badge_top_project +
  #         badge_no_reporting +
  #         badge_project_failure +
  #         org_website +
  #         as.numeric(target_age) +
  #         target_disability +
  #         has_trad +
  #         factor(direction) +
  #         factor(region),
  #       data   = .x,
  #       family = binomial
  #     )
  #     
  #     broom::tidy(m) %>%
  #       filter(term %in% c("appl_quality", "cofinancing_share")) %>%
  #       select(term, std.error)      # <-- only term & std.error; group_modify adds competition_index
  #   }) %>%
  #   ungroup() %>%
  #   # 2) Attach the year for each competition_index
  #   left_join(comp_year, by = "competition_index")
  # 
  # # Quick sanity check
  # # head(se_time)
  # # names(se_time)
  # 
  # # 3) Plot SE of appl_quality over YEARS
  # se_time %>%
  #   filter(term == "appl_quality") %>%
  #   ggplot(aes(x = year_extracted, y = std.error, group = 1)) +
  #   geom_line() +
  #   geom_point() +
  #   labs(
  #     x     = "Year",
  #     y     = "Standard error of appl_quality",
  #     title = "SE of application quality coefficient over time"
  #   ) +
  #   theme_minimal()
  # 
  # # 4) Plot SE of cofinancing_share over YEARS
  # se_time %>%
  #   filter(term == "cofinancing_share") %>%
  #   ggplot(aes(x = year_extracted, y = std.error, group = 1)) +
  #   geom_line() +
  #   geom_point() +
  #   labs(
  #     x     = "Year",
  #     y     = "Standard error of cofinancing_share",
  #     title = "SE of cofinancing coefficient over time"
  #   ) +
  #   theme_minimal()
  # 
  # library(dplyr)
  # library(ggplot2)
  # 
  # # 0) Lookup: which competition_index corresponds to which year
  # comp_year <- presi_model %>%
  #   distinct(competition_index, year_extracted)
  # 
  # # 1) Standard deviations by competition_index
  # sd_time <- presi_model %>%
  #   group_by(competition_index) %>%
  #   summarise(
  #     sd_appl_quality       = sd(appl_quality, na.rm = TRUE),
  #     sd_cofinancing_share  = sd(cofinancing_share, na.rm = TRUE),
  #     .groups = "drop"
  #   ) %>%
  #   left_join(comp_year, by = "competition_index")
  # 
  # # optional check:
  # # head(sd_time)
  # 
  # # 2) Plot SD of appl_quality over YEARS
  # ggplot(sd_time, aes(x = year_extracted, y = sd_appl_quality, group = 1)) +
  #   geom_line() +
  #   geom_point() +
  #   labs(
  #     x     = "Year",
  #     y     = "SD of appl_quality",
  #     title = "Standard deviation of application quality over time"
  #   ) +
  #   theme_minimal()
  # 
  # # 3) Plot SD of cofinancing_share over YEARS
  # ggplot(sd_time, aes(x = year_extracted, y = sd_cofinancing_share, group = 1)) +
  #   geom_line() +
  #   geom_point() +
  #   labs(
  #     x     = "Year",
  #     y     = "SD of cofinancing_share",
  #     title = "Standard deviation of cofinancing share over time"
  #   ) +
  #   theme_minimal()
  # 
  # 
  # library(dplyr)
  # library(tidyr)
  # library(ggplot2)
  # 
  # # 1) Mean application quality and cofinancing share by YEAR
  # means_time <- presi_model %>%
  #   group_by(year_extracted) %>%
  #   summarise(
  #     mean_appl_quality      = mean(appl_quality, na.rm = TRUE),
  #     mean_cofinancing_share = mean(cofinancing_share, na.rm = TRUE),
  #     .groups = "drop"
  #   )
  # 
  # # (optional) check
  # # print(means_time)
  # 
  # # 2) Long format for a single combined plot
  # means_long <- means_time %>%
  #   pivot_longer(
  #     cols      = c(mean_appl_quality, mean_cofinancing_share),
  #     names_to  = "variable",
  #     values_to = "mean_value"
  #   )
  # 
  # # 3) Plot both over time
  # ggplot(means_long, aes(x = year_extracted, y = mean_value, color = variable, group = variable)) +
  #   geom_line() +
  #   geom_point() +
  #   labs(
  #     x     = "Year",
  #     y     = "Mean value",
  #     color = "Variable",
  #     title = "Application quality and cofinancing share over time"
  #   ) +
  #   theme_minimal()
  # # Application quality over time
  # ggplot(means_time, aes(x = year_extracted, y = mean_appl_quality)) +
  #   geom_line() +
  #   geom_point() +
  #   labs(
  #     x = "Year",
  #     y = "Mean application quality",
  #     title = "Application quality over time"
  #   ) +
  #   theme_minimal()
  # 
  # # Cofinancing share over time
  # ggplot(means_time, aes(x = year_extracted, y = mean_cofinancing_share)) +
  #   geom_line() +
  #   geom_point() +
  #   labs(
  #     x = "Year",
  #     y = "Mean cofinancing share",
  #     title = "Cofinancing share over time"
  #   ) +
  #   theme_minimal()
  # 
