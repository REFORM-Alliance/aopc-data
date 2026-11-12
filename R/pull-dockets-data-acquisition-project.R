rm(list = ls())


####Read in Libraries####
library(data.table)
library(tidyverse)
library(here)
library(janitor)
library(scales)
library(DBI)
library(RPostgres)
library(dbplyr)


####Setup####
##Connect to DB
con <- dbConnect(
  RPostgres::Postgres(),
  host = "reform-cjis-rds-cluster.cluster-cl8mgigamxxo.us-east-1.rds.amazonaws.com",
  port = 5432,
  user = "postgres",
  password = "Reform12345!"
)

on.exit(dbDisconnect(con), add = TRUE)

##County
county_of_interest <- "Chester"

##Seed
set.seed(654321)

####Read in Raw Data####
##Docket Entry Data
docket_entry_df <- 
  con %>% 
  tbl(in_schema("aopc_data", "docket_entry_df")) %>%
  collect() 

# ##Calendar Data
# calendar_df <- 
#   con %>% 
#   tbl(in_schema("aopc_data", "calendar_df")) %>%
#   collect() 

##Case and Disposition Data
case_disposition_data_df <- 
  con %>% 
  tbl(in_schema("aopc_data", "case_disposition_data_df")) %>%
  collect() 

# ##Sentence Data
# sentence_data_df <- 
#   con %>% 
#   tbl(in_schema("aopc_data", "sentence_data_df")) %>%
#   collect() 


####Docket Pulls####
##Cases that had a Gagnon or Violation hearing
tv_reincarceration_gagnon_violation_cases <- 
  docket_entry_df %>% 
  left_join(case_disposition_data_df %>% 
              dplyr::select(county, docket_number, defendant_name, defendant_dob) %>% 
              distinct(), 
            by = "docket_number") %>% 
  relocate(county) %>% 
  filter(county == county_of_interest) %>%
  group_by(defendant_name, defendant_dob) %>% 
  mutate(number_of_dockets_per_person = 
           docket_number %>% 
           n_distinct()) %>% 
  ungroup() %>% 
  arrange(docket_number, filed_date) %>% 
  mutate(guilty_plea_flag = 
           case_when(str_detect(official_docket_entry, "^Guilty Plea") == TRUE ~ 1, 
                     TRUE ~ 0)) %>% 
  group_by(docket_number) %>% 
  mutate(guilty_plea_count = 
           guilty_plea_flag %>% 
           cumsum()) %>% 
  ungroup() %>% 
  filter(guilty_plea_count > 0 & official_docket_entry == "Hearing Notice") %>% 
  mutate(filed_date_clean = 
           filed_date %>% 
           as.Date(), 
         hearing_notice_before_cutoff = 
           ifelse(filed_date_clean < as.Date("2024-06-11"), 1, 0), 
         hearing_notice_after_cutoff = 
           ifelse(filed_date_clean >= as.Date("2024-06-11"), 1, 0)) %>% 
  group_by(county, docket_number, number_of_dockets_per_person) %>% 
  dplyr::summarize(across(starts_with("hearing_notice_"), 
                          ~.x %>% 
                            sum(na.rm = TRUE))) %>% 
  ungroup() %>% 
  filter(!(hearing_notice_before_cutoff > 0 & hearing_notice_after_cutoff > 0)) %>% 
  filter(number_of_dockets_per_person == 1) %>% 
  dplyr::select(-number_of_dockets_per_person)

##Before June 11, 2024
dockets_before_cutoff_date <- 
  tv_reincarceration_gagnon_violation_cases %>% 
  filter(hearing_notice_before_cutoff > 0) %>% 
  dplyr::select(county, docket_number) %>% 
  slice_sample(n = 100)
  
##After June 11, 2024
dockets_after_cutoff_date <- 
  tv_reincarceration_gagnon_violation_cases %>% 
  filter(hearing_notice_after_cutoff > 0) %>% 
  dplyr::select(county, docket_number) %>% 
  slice_sample(n = 100)


####Write Data to CSVs####
##Before Cutoff Date File
fwrite(dockets_before_cutoff_date, file = here("data", "chester_county_dockets_before_effective_date.csv"), row.names = FALSE)

##After Cutoff Date File
fwrite(dockets_after_cutoff_date, file = here("data", "chester_county_dockets_after_effective_date.csv"), row.names = FALSE)














