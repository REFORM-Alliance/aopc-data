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
# %>% 
#   append("Order Granting Motion for Leave")

##Docket Entries by Use
docket_entries_by_count <- 
  docket_entry_df %>% 
  group_by(official_docket_entry) %>% 
  dplyr::summarize(count = n()) %>% 
  ungroup() %>% 
  arrange(desc(count)) %>% 
  mutate(in_use = 
           ifelse(official_docket_entry %in% probation_termination_docket_entries, 1, 0) %>% 
           replace_na(0), 
         count = 
           count %>% 
           comma()) 

##Write Docket Entries by Use
fwrite(docket_entries_by_count, file = here("data", "docket_entries_by_count.csv"), row.names = FALSE)

##Early Term by Date/Month
early_term_by_county_month2 <- 
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
  filter(date >= as.Date("2025-06-01")) %>%
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


##PSR Dockets in Allegheny
psrs_allegheny <- 
  docket_entry_df %>% 
  filter(official_docket_entry == "Probation Status Report") %>% 
  # mutate(early_term = 
  #          ifelse(official_docket_entry %in% str_subset(probation_termination_docket_entries, "Early|Act 44"), 1, 0), 
  #        early_term_violation = 
  #          ifelse(official_docket_entry == "Probation/Parole Violation Held - Probation/Parole Terminated", 1, 0), 
  #        natural_term = 
  #          ifelse(official_docket_entry == "Probation Terminated - Natural Expiration", 1, 0)) %>% 
  # dplyr::select(official_docket_entry, docket_number, filed_date) %>% 
  # distinct() %>% 
  mutate(year = year(filed_date), 
         month = month(filed_date), 
         date = 
           paste0(year, "-", month, "-01") %>% 
           as.Date()) %>% 
  filter(date >= as.Date("2025-06-01")) %>%
  left_join(case_disposition_data_df %>% 
              dplyr::select(county, docket_number) %>% 
              distinct(), 
            by = "docket_number") %>% 
  group_by(official_docket_entry, county, docket_number) %>% 
  filter(date == max(date, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(date, county) %>% 
  dplyr::summarize(total_count = n()) %>% 
  ungroup() %>% 
  arrange(county) %>% 
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


psrs_allegheny_2 <- 
  docket_entry_df %>% 
  filter(official_docket_entry == "Probation Status Report") %>% 
  dplyr::select(docket_number) %>% 
  distinct() %>% 
  left_join(docket_entry_df, by = "docket_number") %>% 
  arrange(docket_number, filed_date) %>% 
  mutate(probation_status_report_flag = 
           ifelse(official_docket_entry == "Probation Status Report", 1, NA)) %>% 
  group_by(docket_number) %>% 
  tidyr::fill(probation_status_report_flag, .direction = "down") %>% 
  ungroup() %>% 
  filter(official_docket_entry != "Probation Status Report" & 
           probation_status_report_flag == 1) %>% 
  filter(!(official_docket_entry %in% probation_termination_docket_entries)) %>% 
  group_by(official_docket_entry) %>% 
  dplyr::summarize(count = n()) %>% 
  ungroup() %>% 
  arrange(desc(count))

revocation_hearing_held_df <- 
  docket_entry_df %>% 
  filter(official_docket_entry == "Revocation Hearing Held") %>% 
  dplyr::select(docket_number) %>% 
  distinct() %>% 
  left_join(docket_entry_df, by = "docket_number") %>% 
  arrange(docket_number, filed_date) %>% 
  mutate(probation_status_report_flag = 
           ifelse(official_docket_entry == "Revocation Hearing Held", 1, NA)) %>% 
  group_by(docket_number) %>% 
  tidyr::fill(probation_status_report_flag, .direction = "down") %>% 
  ungroup() %>% 
  filter(official_docket_entry != "Revocation Hearing Held" & 
           probation_status_report_flag == 1) %>% 
  filter(!(official_docket_entry %in% probation_termination_docket_entries)) %>% 
  group_by(official_docket_entry) %>% 
  dplyr::summarize(count = n()) %>% 
  ungroup() %>% 
  arrange(desc(count))
  
 
##Early Termination vs. Normal Termination
early_vs_normal_df <- 
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
  filter(date >= as.Date("2025-06-01")) %>%
  left_join(case_disposition_data_df %>% 
              dplyr::select(county, docket_number) %>% 
              distinct(), 
            by = "docket_number") %>% 
  group_by(official_docket_entry, county, docket_number) %>% 
  filter(date == max(date, na.rm = TRUE)) %>% 
  ungroup() 

 

