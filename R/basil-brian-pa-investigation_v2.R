rm(list = ls())


####Read in Libraries####
library(data.table)
library(tidyverse)
library(here)
library(janitor)
library(scales)


####Read in Data####
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

##Calendar Data
calendar_df <- 
  "data-raw" %>% 
  here() %>% 
  list.files(full.names = TRUE) %>% 
  str_subset("CalendarData.txt$") %>% 
  fread(header = TRUE, stringsAsFactors = FALSE) %>% 
  clean_names() %>% 
  mutate(across(where(is_character), 
                ~.x %>% 
                  na_if(""))) 

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
  # mutate(sentence_period_extra = 
  #          ifelse(sentence_period == "Other", NA, sentence_period)) %>% 
  # group_by(across(-c("sentencing_event_type", "sentence_date", "program_type", "sentence_period", "effective_sentence_date"))) %>% 
  # tidyr::fill(sentence_period_extra, .direction = "updown") %>% 
  # ungroup() %>% 
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

####Early Termination Data####
##Probation Termination Docket Entries
probation_termination_docket_entries <- 
  docket_entry_df %>% 
  pull(official_docket_entry) %>% 
  unique() %>%
  str_subset("Probation") %>% 
  str_subset("Termin") %>% 
  str_subset("Denying|Request|Response|Brief|Scheduling|^Motion for Early Termination of Probation$|^Petition for Probation Review for Early Termination$", negate = TRUE)

##Early Term by Date/Month
early_term_by_county_month <- 
  docket_entry_df %>% 
  filter(official_docket_entry %in% probation_termination_docket_entries) %>%
  mutate(early_term = 
           ifelse(official_docket_entry %in% str_subset(probation_termination_docket_entries, "Early|Act 44"), 1, 0), 
         early_term_violation = 
           ifelse(official_docket_entry == "Probation/Parole Violation Held - Probation/Parole Terminated", 1, 0), 
         natural_term = 
           ifelse(official_docket_entry == "Probation Terminated - Natural Expiration", 1, 0)) %>% 
  dplyr::select(official_docket_entry, docket_number, filed_date, early_term, early_term_violation, natural_term) %>% 
  distinct() %>% 
  mutate(year = year(filed_date), 
         month = month(filed_date), 
         date = 
           paste0(year, "-", month, "-01") %>% 
           as.Date(),
         post_act_44_flag = ifelse(filed_date >= as.Date("2024-06-11"), 1, 0)) %>% 
  left_join(case_disposition_data_df %>% 
              dplyr::select(county, docket_number) %>% 
              distinct(), 
            by = "docket_number") %>%
  
  filter(county == "Montgomery") %>% 
  
  group_by(official_docket_entry, county, docket_number, post_act_44_flag) %>% 
  filter(date == max(date, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(official_docket_entry, date, county, post_act_44_flag) %>% 
  dplyr::summarize(total_count = n(), 
                   across(c(early_term, early_term_violation, natural_term), 
                          ~.x %>% 
                            sum(na.rm = TRUE))) %>% 
  ungroup() %>% 
  arrange(county) %>% 
  mutate(possible_early_term = 
           total_count - (early_term + early_term_violation + natural_term)) %>% 
  {
    df <- .
    
    df %>% 
      right_join(df %>% 
                   dplyr::select(date, county, official_docket_entry) %>% 
                   distinct() %>% 
                   expand.grid() %>% 
                   distinct(), 
                 by = c("date", "county", "official_docket_entry")) %>% 
      mutate(across(where(is.numeric), 
                    ~.x %>% 
                      replace_na(0)))
  } %>% 
  arrange(county, date, desc(total_count))

##Allegheny DF By Date and Docket Entry
allegheny_by_date_docket_entry <-
  early_term_by_county_month %>% 
  filter(county == "Allegheny") %>% 
  mutate(date = 
           date %>% 
           as.character()) %>% 
  arrange(desc(date)) %>% 
  {
    df <- .
    df %>% 
      bind_rows(df %>% 
                  group_by(official_docket_entry, county) %>% 
                  dplyr::summarize(total_count = 
                                     total_count %>% 
                                     sum(na.rm = TRUE)) %>% 
                  ungroup() %>% 
                  mutate(date = "Total"))
  } %>%
  dplyr::select(official_docket_entry, date, total_count) %>% 
  pivot_wider(names_from = "date", 
              values_from = "total_count") %>% 
  relocate("Total", .after = "official_docket_entry") %>% 
  arrange(desc(Total))

fwrite(allegheny_by_date_docket_entry, here("data", "allegheny_by_date_docket_entry_type.csv"), row.names = FALSE)

##Look at Duplicate Sentences 
dups_df <- 
  sentence_data_df %>% 
  filter(sentence_type == "Probation") %>% 
  group_by(docket_number, sentence_type, clean_sentence_date) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1) %>% 
  arrange(docket_number, originating_offense_sequence_number, offense_sequence_number, sentence_date) 

dups_df2 <- 
  sentence_data_df %>% 
  filter(sentence_type == "Probation") %>% 
  group_by(docket_number, originating_offense_sequence_number, offense_sequence_number, sentence_type, clean_sentence_date) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1) %>% 
  arrange(docket_number, originating_offense_sequence_number, offense_sequence_number, sentence_date) 



##Clean Up Sentence Data
sentence_data_df_clean <- 
  sentence_data_df





