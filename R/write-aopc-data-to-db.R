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


####Connect to DB####
con <- dbConnect(
  RPostgres::Postgres(),
  host = "reform-cjis-rds-cluster.cluster-cl8mgigamxxo.us-east-1.rds.amazonaws.com",
  port = 5432,
  user = "postgres",
  password = "Reform12345!"
)

on.exit(dbDisconnect(con), add = TRUE)


####Utility Functions####
##Schema Exists
schema_exists <- function(con, schema_name) {
  sql <- glue::glue_sql(
    "SELECT schema_name FROM information_schema.schemata WHERE schema_name = {schema}",
    schema = schema_name,
    .con = con
  )
  res <- dbGetQuery(con, sql)
  nrow(res) > 0
}

##Qualify Table
qualify_table <- function(schema, tbl) {
  DBI::SQL(paste0(DBI::dbQuoteIdentifier(con, schema), ".", DBI::dbQuoteIdentifier(con, tbl)))
}

##Safe Insert Table into DB
safe_insert_table <- function(con, df, table_name, schema){
  if(schema_exists(con, schema) == FALSE){
    dbExecute(con, paste0("CREATE SCHEMA IF NOT EXISTS ", schema))
  }
  
  if(dbExistsTable(con, DBI::Id(schema = schema, table = table_name)) == FALSE){
    dbWriteTable(con, DBI::Id(schema = schema, table = table_name), df, overwrite = TRUE)
  }else{
    temp_table <- paste0("temp_", table_name)
    new_table <- paste0("new_", table_name)
    table_id <- DBI::Id(schema = schema, table = table_name)
    temp_table_id <- DBI::Id(schema = schema, table = temp_table)
    new_table_id  <- DBI::Id(schema = schema, table = new_table)
    dbWriteTable(con, temp_table_id, df, temporary = FALSE, overwrite = TRUE)
    
    append_unique_sql <- 
      con %>% 
      tbl(table_id) %>% 
      union_all(con %>% 
                  tbl(temp_table_id)) %>% 
      distinct() %>% 
      sql_render()
    
    dbExecute(con, paste0("CREATE TABLE ", qualify_table(schema, new_table), " AS ", append_unique_sql))
    dbExecute(con, paste0("DROP TABLE IF EXISTS ", qualify_table(schema, temp_table), ";"))
    dbExecute(con, paste0("DROP TABLE IF EXISTS ", qualify_table(schema, table_name), ";"))
    dbExecute(con, paste0("ALTER TABLE ", qualify_table(schema, new_table), " RENAME TO ", DBI::dbQuoteIdentifier(con, table_name)))
  }
}


####Read in Raw Data####
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
           as.Date())


####Write Tables into DB####
##Docket Entry DF
safe_insert_table(con = con, df = docket_entry_df, table_name = "docket_entry_df", schema = "aopc_data")
rm(docket_entry_df)

##Calendar Data
safe_insert_table(con = con, df = calendar_df, table_name = "calendar_df", schema = "aopc_data")
rm(calendar_df)

##Case and Disposition Data
safe_insert_table(con = con, df = case_disposition_data_df, table_name = "case_disposition_data_df", schema = "aopc_data")
rm(case_disposition_data_df)

##Sentence Data
safe_insert_table(con = con, df = sentence_data_df, table_name = "sentence_data_df", schema = "aopc_data")
rm(sentence_data_df)



