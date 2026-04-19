  # Author: GI, following EK script


# This script is to request many api calls to the openai chatgpt
# updated: makes it ignore errors skipping them, so that we can go through them again later
# do not read in text as UTF-8 otherwise instead of readable text you will have hmlt5 letters like <U0838383> instead of Hungarian "i"
library(pacman)
source("r_code/lib_functions.R", encoding = "utf-8")
source("r_code/lib_askai_ollama.R")
pacman::p_load(ggplot2,zoo,httr,dplyr,writexl,stringr,stringdist,fuzzyjoin,
               readxl,lubridate,openxlsx,stringr)

input_path<-("data/data_large/application_texts_unique.csv")
input_path2<-("data/data_large/all_projects_data_backup.csv")
# input_path<-("data/data_large/presi_variables_imputed.csv")
output_path<-("data/data_results")
#text dataset
df1 <- read.csv(input_path, sep=';', encoding = "UTF-8")
df2 <- read.csv(input_path2, sep=',', encoding = "UTF-8")

# df2 <- read.csv2(input_path2,  encoding = "UTF-8")

# Perform a left join to add data from df1 to df2 based on the "href" column, delete duplicates
df2 <- df2 %>%
left_join(df1, by = "href") %>%
distinct(href, .keep_all = TRUE)
# df<-unique_application_texts
#temporary - removing the missing variables in df2, eg those without texts
pre_na_rows <- nrow(df2)
df <- na.omit(df2)
message("Dropped ", pre_na_rows - nrow(df), " rows with missing values after join.")
# Sample randomly N rows out of the df via dplyr, define random seed
# Subset the data frame for dates between 2015 and 2023 
# for a children subproject, since it does not need to store things directly, we just read in the file 
# df<-read.csv("data/data_clean/children_10_all.csv")
#removing the already done thingis
processed_paths <- c(
  "data/checkpoints/iteration_checkpoint07052025.csv",
  "data/checkpoints/iteration_checkpoint08052025.csv",
  "data/checkpoints/iteration_checkpoint08052025-2.csv",
  "data/checkpoints/iteration_checkpoint09052025.csv",
  "data/data_large/30k_processed_trad_vals.csv",
  "data/data_large/100k_processed_trad_vals.csv"
)
processed_tables <- lapply(processed_paths, function(path) {
  if (file.exists(path)) read.csv(path) else NULL
})
processed_tables <- Filter(Negate(is.null), processed_tables)
processed_hrefs <- unique(unlist(lapply(processed_tables, function(tbl) {
  if ("column_id" %in% names(tbl)) {
    tbl$column_id
  } else if ("href" %in% names(tbl)) {
    tbl$href
  } else {
    character()
  }
})))

if (length(processed_hrefs) > 0) {
  before_processed <- nrow(df)
  df <- df[!df$href %in% processed_hrefs, ]
  message("Removed ", before_processed - nrow(df), " rows already processed.")
}

set.seed(490) 
glimpse(df)
df <- df %>%
  mutate(year = parse_number(competition))

df_sample <- df %>% sample_n(50000)







# wipe every db from memory but df_sample
rm(list = setdiff(ls(), "df_sample"))
###############################################
# ChatGPT-4o classification
###############################################


# Define a prompt for api calls

prompt_text <- "You are a political scientist analyzing a grant application in Russia in 2022-2024 by an NGO. Provide only a CSV table. Follow these steps:
  
  СВО
1.1. Does the project mention Специальная Военная Операция or СВО? Reply only with 0 or 1
1.2. Reason extensively whether the project voice is neutral or affirmative towards the СВО?
1.3. Take a look at the step 1.2 and reply only with neutral (0) or affirmative(1).
1.4. Reason extensively whether the activities of the project and its target group are related to СВО? 
1.5. Take a look at the step 1.4 and reply only with not related (0) or related(1).
Values
2.1 Does the project use Russian patriotic values? Reply only with 0 or 1.
2.2.If the answer in 2.1 was positive, cite patriotic values extensively and justify why they are patriotic. 


Presentation of Findings:
  Present the results in a CSV format with columns: 'reference_svo', 'rationale_svo_projects', 'projects_attitude_svo', 'tasks_relation_svo', ''relation_svo', 'patriotic_values', 'rationale_patriotic_values'.
Use a semicolon as a delimiter, and enclose all values in quotation marks."


# prompt_text <- "You are a political scientist analyzing a grant application in Russia in 2017-2024 by an NGO. Provide only a CSV table. Follow these steps:
#   
# 1.1 Does the project reference directly to government's projects or laws? Reply only with 0 or 1
# 1.2 If the answer in 1.1 was positive, cite those mentions extensively and justify why they align. 
# 
#   
# Presentation of Findings:
#   Present the results in a CSV format with columns: 'reference_gov', 'rationale_gov'.
# Use a semicolon as a delimiter, and enclose all values in quotation marks."


# Rename columns so that they match the function

df_sample <- df_sample %>%
  rename(column_text=details,
         column_id=href,
  )

checkpoint_filter_path <- "data/checkpoints/iteration_checkpoint11032025_2.csv"
if (file.exists(checkpoint_filter_path)) {
  data_dun <- read.csv(checkpoint_filter_path)
  pre_checkpoint <- nrow(df_sample)
  df_sample1 <- df_sample %>%
    filter(!(column_id %in% data_dun$column_id))
  message("Removed ", pre_checkpoint - nrow(df_sample1), " rows already in checkpoint.")
} else {
  df_sample1 <- df_sample
}

# !!!!!! IMPORTANT !!!!!!
# 1. df_input should be of correct column formats, dates column should be of date class
# 2. checkpoint_path should not contain a csv with no columns: either no old csv or and existing but with columns


new_results <- iterate_rows2(df_input = df_sample1, 
                             column_text = "column_text", 
                             prompt_text = prompt_text,
                             column_id = "column_id",
                             
                             # here it stores temporary results in case of error and then starts from the same datapoint if you run again
                             checkpoint_path = "data/checkpoints/iteration_checkpoint11032025_3.csv",
                             
                             # this is default path to ids with errors
                             errors_path = "data/checkpoints/id_errors.csv",
                             
                             # this is if you wanna in every iteration to print ai output and then recognized df from ai to better monitor where the errors are
                             show_input_output = TRUE,
                             
                             # model = "gpt-4o")
                             model = "gpt-5-mini-2025-08-07")
                             

write.csv2(new_results, "data/data_results/15000_texts_processed_chat4o.csv", row.names = FALSE)
#add other independent variables from df.


print(cleaned_data)

write.csv2(cleaned_data, "data/data_results/15000_texts_processed_chat4o1632025.csv", row.names = FALSE)


file_paths <- c("data/checkpoints/iteration_checkpoint11032025_2.csv",
                "data/checkpoints/iteration_checkpoint11032025_3.csv",
                "data/checkpoints/iteration_checkpoint11032025.csv")

# library(dplyr)
# library(readr)
# 
# # Define file paths
# file_paths <- c("data/checkpoints/iteration_checkpoint11032025_2.csv",
#                 "data/checkpoints/iteration_checkpoint11032025_3.csv",
#                 "data/checkpoints/iteration_checkpoint11032025.csv")
# 
# # Define a consistent column specification
# col_types_spec <- cols(
#   rationale_svo_projects       = col_character(),
#   projects_attitude_svo        = col_character(),  # force as character
#   tasks_relation_svo           = col_character(),
#   relation_svo                 = col_character(),  # or col_double() if all should be numeric
#   patriotic_values            = col_character(),
#   rationale_patriotic_values   = col_character(),
#   column_id                    = col_character(),  # adjust if missing in some files
#   reference_svo                = col_double()
# )
# 
# # Function to read CSV with the defined column types
# read_with_spec <- function(file) {
#   read_csv(file, col_types = col_types_spec, show_col_types = FALSE)
# }
# 
# # Read and bind rows
# combined_data <- file_paths %>%
#   lapply(read_with_spec) %>%
#   bind_rows()
# 
# # View the combined data
# print(combined_data)
# 
# 
# # Read, convert, and bind rows
# combined_data <- file_paths %>%
#   lapply(read_and_convert) %>%
#   bind_rows()


merged_data <- new_results%>%
  left_join(df2, by = c("column_id"='href'))

write.csv2(merged_data, "data/data_results/1500_texts_processed_chat4omini_withIVs.csv", row.names = FALSE)
# write_xlsx(merged_data, "data/data_results/500_texts_processed_chat4omini_withIVs.xlsx")
# 
#   write.csv2(new_results, "data/data_results/50_texts_processed_chat4omini.csv", row.names = FALSE)
# write.csv2(df_sample, "data/data_results/50_texts.csv", row.names = FALSE)
write.csv2(df_sample, "data/1500_raw.csv", row.names = FALSE)

# Write new_results to Excel
write.xlsx(new_results, "data/data_results/1500_texts_processed_chat4omini.xlsx")

# Write df_sample to Excel
# write.xlsx(df_sample, "data/data_results/50_texts.xlsx")





###############################################
# ChatGPT-4o TARGET AUDIENCE classification
###############################################
matches <- str_match(
  df_sample$details,
  "Целевые группы\\s*\\n(.*?)(\\n{2,}|$)"
)
df_sample$target_groups <- matches[, 2]
print(df_sample$target_groups)
# Define a prompt for api calls


# prompt_text <- "You are a political scientist analyzing target groups of a project in Russia in 2017-2024 by an NGO. Provide only a CSV table. Follow these steps:
# 1. What is the mean age of the target groups? Reply only with a number.
# 2. Does the target group of a project include пенсионеры?  Reply only with 0 or 1.
# 3. Does the target group of a project include any несовершеннолетние, e.g. школьники, дети?  Reply only with 0 or 1.
# 4. Does the target group of a project include any people with disabilities? Reply only with 0 or 1.
#   
# Presentation of Findings:
# You response will be automatically parsed. Present the results in a CSV format with columns: 'target_age', 'target_old', 'target_young', 'target_disability'.
# Use a semicolon as a delimiter, and enclose all values in quotation marks."
prompt_text <- "You are a political scientist analyzing target groups of a project in Russia in 2017-2024 by an NGO. Provide only a CSV table. Follow these steps:

1. Does the target group of a project include пенсионеры?  Reply only with 0 or 1.
2. Does the target group of a project include any несовершеннолетние, e.g. школьники, дети?  Reply only with 0 or 1.
3. Does the target group of a project include ветераны?  Reply only with 0 or 1.
3.1 If the target group of a project includes ветераны, desctibe breifly what ветераны does it refer to.
4. Does the target group of a project include матери, семьи?  Reply only with 0 or 1.
Presentation of Findings:
You response will be automatically parsed. Present the results in a CSV format with columns: 'target_old', 'target_young', 'target_veterans', 'target_veterans_description, 'target_mothers'.
Use a semicolon as a delimiter, and enclose all values in quotation marks."


# Rename columns so that they match the function

df_sample <- df_sample %>%
  rename(column_text = target_groups,
         column_id = href) %>%
  drop_na(column_text)

# !!!!!! IMPORTANT !!!!!!!!
# 1. df_input should be of correct column formats, dates column should be of date class
# 2. checkpoint_path should not contain a csv with no columns: either no old csv or and existing but with columns


# new_results <- iterate_rows_target_audience(df_input = df_sample, 
#                              column_text = "column_text", 
#                              prompt_text = prompt_text,
#                              column_id = "column_id",
#                              
#                              # here it stores temporary results in case of error and then starts from the same datapoint if you run again
#                              checkpoint_path = "data/checkpoints/iteration_checkpoint_target.csv",
#                              
#                              # this is default path to ids with errors
#                              errors_path = "data/checkpoints/id_errors_target.csv",
#                              
#                              # this is if you wanna in every iteration to print ai output and then recognized df from ai to better monitor where the errors are
#                              show_input_output = TRUE,
#                              
#                              model = "gpt-4o")
# 


new_results <- iterate_rows_target_audience_ollama(df_input = df_sample, 
                                            column_text = "column_text", 
                                            prompt_text = prompt_text,
                                            column_id = "column_id",
                                            
                                            # here it stores temporary results in case of error and then starts from the same datapoint if you run again
                                             checkpoint_path = "data/checkpoints/iteration_checkpoint_target_ollama_lrg.csv",
                                            
                                            # this is default path to ids with errors
                                            errors_path = "data/checkpoints/id_errors_target_ollama.csv",
                                            
                                            # this is if you wanna in every iteration to print ai output and then recognized df from ai to better monitor where the errors are
                                            show_input_output = TRUE,
                                            
                                            model = "mistral-large:123b-instruct-2411-q8_0")

  


# if there is any writing in target_age, make it NA
new_results <- new_results %>%
  mutate(target_age = ifelse(grepl("[[:alpha:]]", target_age), NA, target_age))%>%
  left_join(df_sample %>% select(column_id, column_text),
            by = c("column_id" = "column_id"))

 write_xlsx(new_results, paste0("data/data_results/target_audience_", Sys.Date(), "large123b.xlsx"))

write.csv(new_results, paste0("data/data_results/target_audience_", Sys.Date(), "large123b.csv"))



# If i stored to a wrong checkpoint, here are the lines for a quick fix
# new_results2<-slice(new_results,17419:25152)
# new_results2<-new_results2%>%
#   select(-1)
#  checkpoint<-read.csv("data/data_temp/checkpoints/iteration_checkpoint_hu_13052024.csv", sep=';')
# checkpoint2<-bind_rows(checkpoint,new_results2)
#   write.csv2(checkpoint2,"data/data_temp/checkpoints/iteration_checkpoint_hu_13052024.csv")
# 

# write.csv2(df_sample, "data/uk/1st_batch_500_uk_08052024.csv", row.names = FALSE)
# write.csv2(new_results, "data/data_results/match_and_gold/4o_test_hu_15052024.csv", row.names = FALSE)


##############################################
# OPTIONAL: Translate to English via custom function
# only translate those speeches that you need for manual check!

##############################################
# # Translate to English via custom function
# dict_eng <- transform_column_ai(column = df_sample$speeches,
#                                 token_limit = 1000, 
#                                 transform = "Translate to English.",
#                                 model = "gpt-3.5-turbo-16k", 
#                                 api = Sys.getenv("OPENAI_API_KEY"),
#                                 saved_df_path = "data/data_temp/translation/df_eng.csv")
# 
# 
# 
# dict_eng$original %in% df_sample$speeches
# 
# df_sample_upd <- left_join(df_sample, 
#                            dict_eng,
#                            by = c("speeches" = "original")) %>%
#   rename(speeches_eng = transformed)
# 
# write.table(df_sample,
#             "data/data_temp/speeches_random_50_part_2.csv",
#             row.names = FALSE,
#             sep = ";")
# ###############################################
# ###############################################

##############################################
# ChatGPT-4o classification
###############################################


# Define a prompt for api calls

prompt_text <- "Вы политолог и проводите анализ текста, чтобы определить уровень вовлеченности заявок на проекты гражданского общества в патриотические, традиционные и консервативные ценности российского государства. Эти проекты были представлены на российские государственные конкурсы на финансирование организаций гражданского общества. Используйте температурную шкалу уровеня вовлеченности в ценности
0 - проект не включает никаких ссылок на традиционные, патриотические или консервативные ценности во всем тексте
1 - проект упоминает традиционные, патриотические или консервативные ценности только два или три раза в тексте, но социальная значимость, цель, задачи проекта не тесно связаны с этими ценностями
2 - проект упоминает традиционные, патриотические или консервативные ценности более трех раз в тексте, но цель и задачи проекта не связаны с этими ценностями
3 - либо цель, либо задачи проекта включают отсылки к традиционным, патриотическим или консервативным ценностям, но проект не направлен на их конкретную реализацию
4- как цель, так и задачи проекта включают отсылки к традиционным, патриотическим или консервативным ценностям и проект направлен на их конкретную реализацию
Я отправляю вам текст заявки на проект. Пожалуйста, присвойте тексту номер в соответствии со значениями вышеприведенной шкалы. Ваш ответ будет обработан автоматически -- ответьте одной только цифрой, и больше ничем."


# Rename columns so that they match the function
df_sample<-df
df_sample <- df_sample %>%
  rename(column_text=details,
         column_id=href,
  )

# !!!!!! IMPORTANT !!!!!!!!
# 1. df_input should be of correct column formats, dates column should be of date class
# 2. checkpoint_path should not contain a csv with no columns: either no old csv or and existing but with columns


new_results <- iterate_rows3(df_input = df_sample, 
                             column_text = "column_text", 
                             prompt_text = prompt_text,
                             column_id = "column_id",
                             
                             # here it stores temporary results in case of error and then starts from the same datapoint if you run again
                             checkpoint_path = "data/checkpoints/iteration_checkpoint09052025.csv",
                             
                             # this is default path to ids with errors
                             errors_path = "data/checkpoints/id_errors.csv",
                             
                             # this is if you wanna in every iteration to print ai output and then recognized df from ai to better monitor where the errors are
                             show_input_output = TRUE,
                             
                             model = "gpt-5-mini-2025-08-07")
 new_results<-df_dun_3
new_results<-new_results%>%
  rename(traditional_values=extracted_number)%>%
rename(href=column_id)

new_results_f<-df%>%
  merge(new_results, by='href')
# glimpse(new_results)
# glimpse(df)
# 
# hist(new_results$traditional_values)
# glipmse(new_results$traditional_values)
new_results_f<-new_results_f%>%
  na.omit()
 write.csv(new_results_f, "data/data_large/30k_processed_trad_vals.csv", row.names = FALSE)
 new_results_f_s<-new_results_f%>%
 select(-c(details, details_norm))
 #add other independent variables from df.
# hist(new_results_f$traditional_values)

hist(new_results_f$target_age)

# df <- df[!df$href %in% df_sample$column_id, ]
