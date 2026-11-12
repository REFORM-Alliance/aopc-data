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
                  na_if(""))) 


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
           as.Date()) %>% 
  filter(date >= as.Date("2025-01-01")) %>%
  left_join(case_disposition_data_df %>% 
              dplyr::select(county, docket_number) %>% 
              distinct(), 
            by = "docket_number") %>% 
  group_by(official_docket_entry, county, docket_number) %>% 
  filter(date == max(date, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(date, county) %>% 
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
                   dplyr::select(date, county) %>% 
                   distinct() %>% 
                   expand.grid() %>% 
                   distinct(), 
                 by = c("date", "county")) %>% 
      mutate(across(where(is.numeric), 
                    ~.x %>% 
                      replace_na(0)))
  } %>% 
  arrange(county, date)

##Montgomery County
montgomery_df <- 
  docket_entry_df %>% 
  # filter(official_docket_entry %in% probation_termination_docket_entries) %>%
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
           as.Date()) %>% 
  left_join(case_disposition_data_df %>% 
              dplyr::select(county, docket_number) %>% 
              distinct(), 
            by = "docket_number") %>% 
  filter(date >= as.Date("2025-06-23"), county == "Montgomery") %>%
  group_by(official_docket_entry, county, docket_number) %>% 
  filter(date == max(date, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(date, county) %>% 
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
                   dplyr::select(date, county) %>% 
                   distinct() %>% 
                   expand.grid() %>% 
                   distinct(), 
                 by = c("date", "county")) %>% 
      mutate(across(where(is.numeric), 
                    ~.x %>% 
                      replace_na(0)))
  } %>% 
  arrange(county, date)

##Function to Pull Cases w/ Sentence by Docket Entry
pull_docket_entry_type <- function(docket_entry_name, full_docket = TRUE){
  df_raw <- 
    docket_entry_df %>% 
    filter(official_docket_entry %in% docket_entry_name) %>%
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
             as.Date()) %>% 
    # filter(date >= as.Date("2025-06-01")) %>%
    left_join(case_disposition_data_df %>% 
                dplyr::select(county, docket_number) %>% 
                distinct(), 
              by = "docket_number")
  
  if(full_docket == TRUE){
    df <- 
      docket_entry_df %>% 
      filter(docket_number %in%
               c(df_raw %>% 
                   pull(docket_number) %>% 
                   unique())) %>% 
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
               as.Date()) %>% 
      left_join(case_disposition_data_df %>% 
                  dplyr::select(county, docket_number) %>% 
                  distinct(), 
                by = "docket_number") %>% 
      arrange(county, docket_number, date)
  }else if(full_docket == FALSE){
    df <- 
      df_raw
  }
  
  df_sentence <- 
    df %>% 
    group_by(docket_number) %>% 
    mutate(docket_id = 1:n()) %>% 
    ungroup() %>% 
    left_join(sentence_data_df %>% 
                mutate(effective_sentence_date = 
                         sentence_date %>% 
                         as.Date(),
                       minimum_release_date = 
                         effective_sentence_date + days(min_days) + months(min_months) + years(min_years), 
                       maximum_release_date = 
                         effective_sentence_date + days(max_days) + months(max_months) + years(max_years)) %>% 
                dplyr::select(docket_number, sentence_type, effective_sentence_date, sentencing_event_type, ends_with("release_date")) %>% 
                arrange(docket_number, effective_sentence_date) %>% 
                group_by(docket_number) %>% 
                mutate(sentence_seq = 1:n()) %>% 
                ungroup(), 
              by = c("docket_number"), 
              relationship = "many-to-many") %>% 
    group_by(docket_number) %>% 
    mutate(min_sentence_date = 
             effective_sentence_date %>% 
             min(na.rm = TRUE), 
           filed_date_clean = 
             filed_date %>% 
             as.Date(),
           keep_filter = 
             case_when(filed_date_clean <= min_sentence_date & sentence_seq == 1 ~ 1,
                       filed_date_clean >= effective_sentence_date & filed_date_clean <= maximum_release_date ~ 1, 
                       TRUE ~ 0)) %>% 
    ungroup() %>% 
    filter(keep_filter == 1) %>% 
    dplyr::select(-keep_filter) 
  
  return(df_sentence)
}

##Pull Examples
df_ex <-
  "Order Granting Early Release" %>% 
  pull_docket_entry_type(full_docket = TRUE) 
  




