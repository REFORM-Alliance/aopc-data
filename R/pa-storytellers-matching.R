rm(list = ls())


####Read in Libraries####
library(data.table)
library(tidyverse)
library(here)
library(janitor)
library(scales)
library(fuzzyjoin)
library(fastLink)
library(stringdist)


####Read in Data####
##Case and Disposition Data
case_disposition_data_df <- 
  "data-raw" %>% 
  here() %>% 
  list.files(full.names = TRUE) %>% 
  str_subset("CaseandDispositionData.txt$") %>% 
  fread(header = TRUE, stringsAsFactors = FALSE) %>% 
  clean_names() %>% 
  mutate(across(where(is_character), 
                ~.x %>% 
                  na_if("")))

##PA Subscribed List
pa_subscribed_list <- 
  "data-raw" %>% 
  here() %>% 
  list.files(full.names = TRUE) %>% 
  str_subset("pa-subscribed-list-jan2026") %>% 
  fread(header = TRUE, stringsAsFactors = FALSE) %>% 
  clean_names() %>% 
  mutate(across(where(is_character), 
                ~.x %>% 
                  str_trim(side = "both") %>% 
                  na_if(""))) 

##Docket Entry Data
docket_entry_df <- 
  "data-raw" %>% 
  here() %>% 
  list.files(full.names = TRUE) %>% 
  str_subset("DocketEntryData.txt$") %>% 
  fread(header = TRUE, stringsAsFactors = FALSE) %>% 
  clean_names() %>% 
  mutate(across(where(is_character), 
                ~.x %>% 
                  na_if(""))) %>% 
  arrange(filed_date)

##Identify Early Termination/Termination
probation_termination_docket_entries <- 
  docket_entry_df %>% 
  pull(official_docket_entry) %>% 
  unique() %>%
  str_subset("Probation") %>% 
  str_subset("Termin") %>% 
  str_subset("Denying|Request|Response|Brief|Scheduling|^Motion for Early Termination of Probation$|^Petition for Probation Review for Early Termination$", negate = TRUE)

##Sentence Data
sentence_data_df <- 
  "data-raw" %>% 
  here() %>% 
  list.files(full.names = TRUE) %>% 
  str_subset("SentenceData.txt$") %>% 
  fread(header = TRUE, stringsAsFactors = FALSE) %>% 
  clean_names() %>% 
  mutate(across(where(is_character), 
                ~.x %>% 
                  na_if("")), 
         clean_sentence_date = 
           sentence_date %>% 
           as.Date()) %>% 
  filter(!(sentence_type == "No Further Penalty")) %>% 
  group_by(across(-c("sentence_date"))) %>% 
  dplyr::summarize(sentence_date = 
                     sentence_date %>% 
                     min(na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(sentencing_event_type, sentence_period) %>% 
  group_by(across(-c("sentencing_event_type", "sentence_date", "program_type", "sentence_period", "effective_sentence_date"))) %>% 
  dplyr::summarize(across(c("sentencing_event_type", "program_type", "sentence_period"), 
                          ~.x %>% 
                            paste(collapse = "; ")), 
                   sentence_date = 
                     sentence_date %>% 
                     min(na.rm = TRUE), 
                   effective_sentence_date = 
                     effective_sentence_date %>% 
                     na.omit() %>% 
                     paste(collapse = "; ")) %>% 
  ungroup()


####Clean Data####
##Case and Disposition Data
defendant_df <- 
  case_disposition_data_df %>% 
  dplyr::select(starts_with("defendant"), docket_number, county) %>% 
  distinct() %>% 
  left_join(sentence_data_df %>% 
              filter(sentence_type == "Probation") %>% 
              dplyr::select(docket_number) %>% 
              distinct() %>% 
              mutate(probation_flag = 1), 
            by = "docket_number") %>% 
  left_join(docket_entry_df %>% 
              filter(official_docket_entry %in% probation_termination_docket_entries) %>% 
              mutate(probation_termination_flag = 1, 
                     early_term_act_44_flag = 
                       ifelse(official_docket_entry %in% str_subset(probation_termination_docket_entries, "Early|Act 44"), 1, 0)) %>%
              dplyr::select(docket_number, ends_with("flag")) %>% 
              distinct(), 
            by = "docket_number") %>% 
  group_by(across(c(starts_with("defendant"), county))) %>% 
  dplyr::summarize(docket_number = 
                     docket_number %>% 
                     paste(collapse = "; "), 
                   across(c(ends_with("_flag")), 
                          ~.x %>% 
                            replace_na(0) %>% 
                            max(na.rm = TRUE))) %>% 
  ungroup() %>% 
  separate_wider_delim(defendant_name, delim = ", ", names = c("defendant_last_name", "defendant_first_name"), too_many = "merge", too_few = "align_start", cols_remove = FALSE) %>% 
  filter(defendant_state == "PA") %>% 
  mutate(across(c("defendant_first_name", "defendant_last_name", "defendant_name", "county", "defendant_city"), 
                ~.x %>% 
                  str_trim(side = "both") %>% 
                  str_to_title() %>% 
                  str_replace_all("[.]", "") %>% 
                  na_if("Unknown") %>% 
                  na_if("-") %>% 
                  str_replace_all(",$", "") %>% 
                  str_replace_all("^Phila$", "Philadelphia") %>% 
                  str_replace_all("Mc ", "Mc") %>% 
                  str_replace_all("Confuluence", "Confluence") %>% 
                  str_replace_all("Columbis", "Columbus") %>% 
                  str_replace_all("lll", "ll") %>% 
                  str_replace_all("Coatesvilee", "Coatesville") %>% 
                  str_replace_all(" Pa$", "") %>% 
                  str_replace_all("  ", " ") %>% 
                  str_replace_all("Hghts", "Heights") %>% 
                  str_replace_all("Cliften", "Clifton") %>% 
                  str_replace_all("Height$", "Heights") %>% 
                  str_replace_all("Hts$", "Heights") %>% 
                  str_replace_all("Towship$", "Township") %>% 
                  str_replace_all("Twp$", "Township") %>% 
                  str_replace_all("Twnp$", "Township")),
         name = 
           case_when(is.na(defendant_last_name) == TRUE ~ defendant_first_name, 
                     is.na(defendant_first_name) == TRUE ~ defendant_last_name, 
                     is.na(defendant_last_name) == FALSE & is.na(defendant_first_name) == FALSE ~ paste0(defendant_first_name, " ", defendant_last_name), 
                     TRUE ~ NA)) %>% 
  group_by(name, defendant_dob) %>% 
  mutate(person_id = cur_group_id()) %>% 
  ungroup()

##PA List
pa_subscribed_clean <- 
  pa_subscribed_list %>% 
  dplyr::select(nationbuilder_id, external_id, first_name, middle_name, last_name, suffix, full_name, primary_city, primary_county, 
                starts_with("email"), contains("phone_number"), mobile_number) %>% 
  distinct() %>% 
  mutate(across(c("first_name", "last_name", "middle_name", "full_name", "primary_city", "primary_county"), 
                ~.x %>% 
                  str_trim(side = "both") %>% 
                  str_to_title() %>% 
                  str_replace_all("[.]", "") %>% 
                  na_if("Unknown") %>% 
                  na_if("-") %>% 
                  str_replace_all(",$", "") %>% 
                  str_replace_all("^Phila$", "Philadelphia") %>% 
                  str_replace_all("Mc ", "Mc") %>% 
                  str_replace_all("Confuluence", "Confluence") %>% 
                  str_replace_all("Columbis", "Columbus") %>% 
                  str_replace_all("lll", "ll") %>% 
                  str_replace_all("Coatesvilee", "Coatesville") %>% 
                  str_replace_all(" Pa$", "") %>% 
                  str_replace_all("  ", " ") %>% 
                  str_replace_all("Hghts", "Heights") %>% 
                  str_replace_all("Cliften", "Clifton") %>% 
                  str_replace_all("Height$", "Heights") %>% 
                  str_replace_all("Hts$", "Heights") %>% 
                  str_replace_all("Towship$", "Township") %>% 
                  str_replace_all("Twp$", "Township") %>% 
                  str_replace_all("Twnp$", "Township"))) %>% 
  dplyr::select(-suffix)


####Matching####
##First Round - Name and County
first_round_matches <- 
  pa_subscribed_clean %>%
  rename("name" = "full_name") %>% 
  rename_with(~.x %>% 
                str_replace_all("primary_", "")) %>% 
  filter(is.na(county) == FALSE) %>% 
  left_join(defendant_df %>% 
              rename("city" = "defendant_city"), 
            by = c("county", "name")) %>% 
  filter(is.na(docket_number) == FALSE) %>% 
  dplyr::select(c(ends_with("_id"), name, county, ends_with("_flag"), contains("email"), contains("phone_number"), mobile_number)) %>% 
  dplyr::select(-probation_flag) %>% 
  mutate(across(ends_with("_flag"), 
                ~.x %>% 
                  as.logical())) %>% 
  rename_with(~.x %>% 
                str_replace_all("_flag$", "")) %>% 
  group_by(nationbuilder_id) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(!(count > 1)) %>% 
  dplyr::select(-count)

##Second Round - Just Name
second_round_matches <- 
  pa_subscribed_clean %>%
  rename("name" = "full_name") %>% 
  rename_with(~.x %>% 
                str_replace_all("primary_", "")) %>% 
  anti_join(first_round_matches %>% 
              dplyr::select(nationbuilder_id) %>% 
              distinct(), 
            by = "nationbuilder_id") %>% 
  left_join(defendant_df %>% 
              rename("city" = "defendant_city"), 
            by = c("name")) %>% 
  filter(is.na(docket_number) == FALSE) %>% 
  dplyr::select(c(ends_with("_id"), name, starts_with("county"), ends_with("_flag"), contains("email"), contains("phone_number"), mobile_number)) %>% 
  dplyr::select(-probation_flag) %>% 
  mutate(across(ends_with("_flag"), 
                ~.x %>% 
                  as.logical())) %>% 
  rename_with(~.x %>% 
                str_replace_all("_flag$", "")) %>% 
  group_by(nationbuilder_id) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(!(count > 1)) %>% 
  dplyr::select(-count) %>% 
  rename("county" = "county.x", 
         "county_court_data" = "county.y")

##Third Round - No Middle Initials and County
third_round_matches <- 
  pa_subscribed_clean %>%
  rename("name" = "full_name") %>% 
  rename_with(~.x %>% 
                str_replace_all("primary_", "")) %>% 
  anti_join(first_round_matches %>% 
              dplyr::select(nationbuilder_id) %>% 
              bind_rows(second_round_matches %>% 
                          dplyr::select(nationbuilder_id)) %>% 
              distinct(), 
            by = "nationbuilder_id") %>% 
  filter(is.na(name) == FALSE, 
         is.na(county) == FALSE) %>% 
  left_join(defendant_df %>% 
              rename("city" = "defendant_city") %>% 
              filter(is.na(name) == FALSE, 
                     is.na(county) == FALSE) %>% 
              mutate(name = 
                       name %>% 
                       str_replace_all(" [A-Z] ", " ")), 
            by = c("county", "name")) %>% 
  filter(is.na(docket_number) == FALSE) %>% 
  dplyr::select(c(ends_with("_id"), name, starts_with("county"), ends_with("_flag"), contains("email"), contains("phone_number"), mobile_number)) %>% 
  dplyr::select(-probation_flag) %>% 
  mutate(across(ends_with("_flag"), 
                ~.x %>% 
                  as.logical())) %>% 
  rename_with(~.x %>% 
                str_replace_all("_flag$", "")) %>% 
  group_by(nationbuilder_id) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(!(count > 1)) %>% 
  dplyr::select(-count)

##Fourth Round - No Middle Initials and County
fourth_round_matches <- 
  pa_subscribed_clean %>%
  rename("name" = "full_name") %>% 
  rename_with(~.x %>% 
                str_replace_all("primary_", "")) %>% 
  anti_join(first_round_matches %>% 
              dplyr::select(nationbuilder_id) %>% 
              bind_rows(second_round_matches %>% 
                          dplyr::select(nationbuilder_id)) %>% 
              bind_rows(third_round_matches %>% 
                          dplyr::select(nationbuilder_id)) %>% 
              distinct(), 
            by = "nationbuilder_id") %>% 
  filter(is.na(name) == FALSE) %>% 
  left_join(defendant_df %>% 
              rename("city" = "defendant_city") %>% 
              filter(is.na(name) == FALSE) %>% 
              mutate(name = 
                       name %>% 
                       str_replace_all(" [A-Z] ", " ")), 
            by = c("name")) %>% 
  filter(is.na(docket_number) == FALSE) %>% 
  dplyr::select(c(ends_with("_id"), name, starts_with("county"), ends_with("_flag"), contains("email"), contains("phone_number"), mobile_number)) %>% 
  dplyr::select(-probation_flag) %>% 
  mutate(across(ends_with("_flag"), 
                ~.x %>% 
                  as.logical())) %>% 
  rename_with(~.x %>% 
                str_replace_all("_flag$", "")) %>% 
  group_by(nationbuilder_id) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(!(count > 1)) %>% 
  dplyr::select(-count) %>% 
  rename("county" = "county.x", 
         "county_court_data" = "county.y")

##Total Matches
total_matches <- 
  first_round_matches %>% 
  bind_rows(second_round_matches) %>%
  bind_rows(third_round_matches) %>%
  bind_rows(fourth_round_matches)


####Write Data####
##Total Matches
fwrite(total_matches, here("data", "pa-subscribers", "pa-subscriber-matches-4-rounds.csv"), row.names = FALSE)

