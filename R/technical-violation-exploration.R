rm(list = ls())


####Read in Libraries####
library(data.table)
library(tidyverse)
library(here)
library(janitor)
library(scales)


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
                  na_if("") %>% 
                  na_if("99999"))) 
  
##Sentence Data
sentence_data_raw <- 
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
  ungroup()

sentence_data_df <- 
  sentence_data_raw %>% 
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
##Sentencing Event Types
sentencing_event_types <- 
  sentence_data_raw %>% 
  pull(sentencing_event_type) %>% 
  unique() %>% 
  sort() %>% 
  str_subset("Probation|Technical")

##Early Term by Date/Month
tv_incarc_by_month_county <- 
  sentence_data_raw %>% 
  arrange(docket_number, clean_sentence_date) %>% 
  # filter(docket_number == "CP-01-CR-0000125-2020") %>%
  group_by(docket_number, originating_offense_sequence_number, offense_sequence_number) %>% 
  mutate(sentence_num_by_seq = 1:n()) %>% 
  ungroup() %>% 
  left_join(case_disposition_data_df %>% 
              # filter(docket_number == "CP-01-CR-0000125-2020") %>%
              mutate(case_disposition_date_clean = 
                       case_disposition_date %>% 
                       as.Date()) %>% 
              arrange(docket_number, case_disposition_date_clean) %>% 
              group_by(docket_number, offense_sequence_number) %>% 
              mutate(sentence_num_by_seq = 1:n()) %>% 
              ungroup() %>% 
              mutate(original_sentence_flag = 1), 
            by = c("docket_number", "offense_sequence_number", "sentence_num_by_seq")) %>% 
  mutate(original_sentence_flag = 
           original_sentence_flag %>% 
           replace_na(0)) %>% 
  group_by(docket_number) %>% 
  tidyr::fill(county, .direction = "downup") %>% 
  ungroup() %>% 
  filter(original_sentence_flag == 0)  %>% 
  mutate(sentence_month = 
           sentence_date %>% 
           month(),
         sentence_year = 
           sentence_date %>% 
           year(),
         across(c(starts_with("min_"), starts_with("max_")), 
                ~.x %>% 
                  replace_na(0)),
         min_sentence = 
           years(min_years) + months(min_months) + days(min_days),
         max_sentence = 
           years(max_years) + months(max_months) + days(max_days),
         across(ends_with("_sentence"),
                ~(year(.x) * 12 + month(.x) + day(.x) / 30.44))) %>% 
  group_by(sentence_month, sentence_year, county) %>% 
  mutate(total_sentences = n(), 
         total_probationers = 
           docket_number %>% 
           n_distinct()) %>% 
  ungroup() %>% 
  filter(sentence_type == "Confinement") %>% 
  group_by(sentence_month, sentence_year, total_sentences, total_probationers, county) %>% 
  dplyr::summarize(total_sentences_confinement = n(), 
                   total_probationers_confinement = 
                     docket_number %>% 
                     n_distinct(), 
                   across(ends_with("_sentence"),
                          list(
                            mean = 
                              ~.x %>% 
                              mean(na.rm = TRUE),
                            max = 
                              ~.x %>% 
                              max(na.rm = TRUE) %>% 
                              suppressWarnings(),
                            min = 
                              ~.x %>% 
                              min(na.rm = TRUE) %>% 
                              suppressWarnings(), 
                            median = 
                              ~.x %>% 
                              median(na.rm = TRUE),
                            sum = 
                              ~.x %>% 
                              sum(na.rm = TRUE)),
                          .names = "{.col}_{.fn}_confinement")) %>% 
  ungroup() %>% 
  mutate(sentence_year_month = as.Date(paste0(sentence_year, "-", sentence_month, "-01"))) %>% 
  dplyr::select(-c(sentence_month, sentence_year)) %>% 
  arrange(sentence_year_month) %>% 
  relocate(sentence_year_month, .before = 1) %>% 
  mutate(across(where(is.numeric), 
                ~.x %>% 
                  replace_na(0))) %>% 
  mutate(imprisonment_rate_by_probationer = 
           total_probationers_confinement/total_probationers, 
         imprisonment_rate_by_sentence = 
           total_sentences_confinement/total_sentences,
         year = 
           sentence_year_month %>% 
           year(), 
         month = 
           sentence_year_month %>% 
           month()) %>% 
  complete(year, month = 1:12, county) %>% 
  mutate(across(where(is.numeric), 
                ~.x %>% 
                  replace_na(0)), 
         sentence_year_month = 
           as.Date(paste0(year, "-", month, "-01"))) %>% 
  dplyr::select(-c(year, month)) %>% 
  relocate(county, .after = "sentence_year_month")
  
# tv_incarc_by_month_county <- 
#   sentence_data_raw %>% 
#   # filter(sentencing_event_type %in% sentencing_event_types) %>% 
#   left_join(case_disposition_data_df %>% 
#               dplyr::select(docket_number, county) %>% 
#               distinct(), 
#             by = "docket_number") %>% 
#   mutate(sentence_month = 
#            sentence_date %>% 
#            month(),
#          sentence_year = 
#            sentence_date %>% 
#            year(),
#          across(c(starts_with("min_"), starts_with("max_")), 
#                 ~.x %>% 
#                   replace_na(0)),
#          min_sentence = 
#            years(min_years) + months(min_months) + days(min_days),
#          max_sentence = 
#            years(max_years) + months(max_months) + days(max_days),
#          across(ends_with("_sentence"),
#                 ~(year(.x) * 12 + month(.x) + day(.x) / 30.44))) %>% 
#   group_by(sentence_month, sentence_year, county) %>% 
#   mutate(total_sentences = n(), 
#          total_probationers = 
#            docket_number %>% 
#            n_distinct()) %>% 
#   ungroup() %>% 
#   filter(sentence_type == "Confinement") %>% 
#   group_by(sentence_month, sentence_year, total_sentences, total_probationers, county) %>% 
#   dplyr::summarize(total_sentences_confinement = n(), 
#                    total_probationers_confinement = 
#                      docket_number %>% 
#                      n_distinct(), 
#                    across(ends_with("_sentence"),
#                           list(
#                             mean = 
#                               ~.x %>% 
#                               mean(na.rm = TRUE),
#                             max = 
#                               ~.x %>% 
#                               max(na.rm = TRUE) %>% 
#                               suppressWarnings(),
#                             min = 
#                               ~.x %>% 
#                               min(na.rm = TRUE) %>% 
#                               suppressWarnings(), 
#                             median = 
#                               ~.x %>% 
#                               median(na.rm = TRUE),
#                             sum = 
#                               ~.x %>% 
#                               sum(na.rm = TRUE)),
#                           .names = "{.col}_{.fn}_confinement")) %>% 
#   ungroup() %>% 
#   mutate(sentence_year_month = as.Date(paste0(sentence_year, "-", sentence_month, "-01"))) %>% 
#   dplyr::select(-c(sentence_month, sentence_year)) %>% 
#   arrange(sentence_year_month) %>% 
#   relocate(sentence_year_month, .before = 1) %>% 
#   mutate(across(where(is.numeric), 
#                 ~.x %>% 
#                   replace_na(0))) %>% 
#   mutate(imprisonment_rate_by_probationer = 
#            total_probationers_confinement/total_probationers, 
#          imprisonment_rate_by_sentence = 
#            total_sentences_confinement/total_sentences,
#          year = 
#            sentence_year_month %>% 
#            year(), 
#          month = 
#            sentence_year_month %>% 
#            month()) %>% 
#   complete(year, month = 1:12, county) %>% 
#   mutate(across(where(is.numeric), 
#                 ~.x %>% 
#                   replace_na(0)), 
#          sentence_year_month = 
#            as.Date(paste0(year, "-", month, "-01"))) %>% 
#   dplyr::select(-c(year, month)) %>% 
#   relocate(county, .after = "sentence_year_month")

##By Month
tv_incarc_by_month <- 
  sentence_data_raw %>% 
  arrange(docket_number, clean_sentence_date) %>% 
  # filter(docket_number == "CP-01-CR-0000125-2020") %>%
  group_by(docket_number, originating_offense_sequence_number, offense_sequence_number) %>% 
  mutate(sentence_num_by_seq = 1:n()) %>% 
  ungroup() %>% 
  left_join(case_disposition_data_df %>% 
              # filter(docket_number == "CP-01-CR-0000125-2020") %>%
              mutate(case_disposition_date_clean = 
                       case_disposition_date %>% 
                       as.Date()) %>% 
              arrange(docket_number, case_disposition_date_clean) %>% 
              group_by(docket_number, offense_sequence_number) %>% 
              mutate(sentence_num_by_seq = 1:n()) %>% 
              ungroup() %>% 
              mutate(original_sentence_flag = 1), 
            by = c("docket_number", "offense_sequence_number", "sentence_num_by_seq")) %>% 
  mutate(original_sentence_flag = 
           original_sentence_flag %>% 
           replace_na(0)) %>% 
  # group_by(docket_number) %>% 
  # tidyr::fill(county, .direction = "downup") %>% 
  # ungroup() %>% 
  filter(original_sentence_flag == 0)  %>% 
  mutate(sentence_month = 
           sentence_date %>% 
           month(),
         sentence_year = 
           sentence_date %>% 
           year(),
         across(c(starts_with("min_"), starts_with("max_")), 
                ~.x %>% 
                  replace_na(0)),
         min_sentence = 
           years(min_years) + months(min_months) + days(min_days),
         max_sentence = 
           years(max_years) + months(max_months) + days(max_days),
         across(ends_with("_sentence"),
                ~(year(.x) * 12 + month(.x) + day(.x) / 30.44))) %>% 
  group_by(sentence_month, sentence_year) %>% 
  mutate(total_sentences = n(), 
         total_probationers = 
           docket_number %>% 
           n_distinct()) %>% 
  ungroup() %>% 
  filter(sentence_type == "Confinement") %>% 
  group_by(sentence_month, sentence_year, total_sentences, total_probationers) %>% 
  dplyr::summarize(total_sentences_confinement = n(), 
                   total_probationers_confinement = 
                     docket_number %>% 
                     n_distinct(), 
                   across(ends_with("_sentence"),
                          list(
                            mean = 
                              ~.x %>% 
                              mean(na.rm = TRUE),
                            max = 
                              ~.x %>% 
                              max(na.rm = TRUE) %>% 
                              suppressWarnings(),
                            min = 
                              ~.x %>% 
                              min(na.rm = TRUE) %>% 
                              suppressWarnings(), 
                            median = 
                              ~.x %>% 
                              median(na.rm = TRUE),
                            sum = 
                              ~.x %>% 
                              sum(na.rm = TRUE)),
                          .names = "{.col}_{.fn}_confinement")) %>% 
  ungroup() %>% 
  mutate(sentence_year_month = as.Date(paste0(sentence_year, "-", sentence_month, "-01"))) %>% 
  dplyr::select(-c(sentence_month, sentence_year)) %>% 
  arrange(sentence_year_month) %>% 
  relocate(sentence_year_month, .before = 1) %>% 
  mutate(across(where(is.numeric), 
                ~.x %>% 
                  replace_na(0))) %>% 
  mutate(imprisonment_rate_by_probationer = 
           total_probationers_confinement/total_probationers, 
         imprisonment_rate_by_sentence = 
           total_sentences_confinement/total_sentences,
         year = 
           sentence_year_month %>% 
           year(), 
         month = 
           sentence_year_month %>% 
           month()) %>% 
  complete(year, month = 1:12) %>% 
  mutate(across(where(is.numeric), 
                ~.x %>% 
                  replace_na(0)), 
         sentence_year_month = 
           as.Date(paste0(year, "-", month, "-01"))) %>% 
  dplyr::select(-c(year, month)) 


# tv_incarc_by_month <- 
#   sentence_data_raw %>% 
#   filter(sentencing_event_type %in% sentencing_event_types) %>% 
#   left_join(case_disposition_data_df %>% 
#               dplyr::select(docket_number, county) %>% 
#               distinct(), 
#             by = "docket_number") %>% 
#   mutate(sentence_month = 
#            sentence_date %>% 
#            month(),
#          sentence_year = 
#            sentence_date %>% 
#            year(),
#          across(c(starts_with("min_"), starts_with("max_")), 
#                 ~.x %>% 
#                   replace_na(0)),
#          min_sentence = 
#            years(min_years) + months(min_months) + days(min_days),
#          max_sentence = 
#            years(max_years) + months(max_months) + days(max_days),
#          across(ends_with("_sentence"),
#                 ~(year(.x) * 12 + month(.x) + day(.x) / 30.44))) %>% 
#   group_by(sentence_month, sentence_year) %>% 
#   mutate(total_sentences = n(), 
#          total_probationers = 
#            docket_number %>% 
#            n_distinct()) %>% 
#   ungroup() %>% 
#   filter(sentence_type == "Confinement") %>% 
#   group_by(sentence_month, sentence_year, total_sentences, total_probationers) %>% 
#   dplyr::summarize(total_sentences_confinement = n(), 
#                    total_probationers_confinement = 
#                      docket_number %>% 
#                      n_distinct(), 
#                    across(ends_with("_sentence"),
#                           list(
#                             mean = 
#                               ~.x %>% 
#                               mean(na.rm = TRUE),
#                             max = 
#                               ~.x %>% 
#                               max(na.rm = TRUE) %>% 
#                               suppressWarnings(),
#                             min = 
#                               ~.x %>% 
#                               min(na.rm = TRUE) %>% 
#                               suppressWarnings(), 
#                             median = 
#                               ~.x %>% 
#                               median(na.rm = TRUE),
#                             sum = 
#                               ~.x %>% 
#                               sum(na.rm = TRUE)),
#                           .names = "{.col}_{.fn}_confinement")) %>% 
#   ungroup() %>% 
#   mutate(sentence_year_month = as.Date(paste0(sentence_year, "-", sentence_month, "-01"))) %>% 
#   dplyr::select(-c(sentence_month, sentence_year)) %>% 
#   arrange(sentence_year_month) %>% 
#   relocate(sentence_year_month, .before = 1) %>% 
#   mutate(across(where(is.numeric), 
#                 ~.x %>% 
#                   replace_na(0))) %>% 
#   mutate(imprisonment_rate_by_probationer = 
#            total_probationers_confinement/total_probationers, 
#          imprisonment_rate_by_sentence = 
#            total_sentences_confinement/total_sentences,
#          year = 
#            sentence_year_month %>% 
#            year(), 
#          month = 
#            sentence_year_month %>% 
#            month()) %>% 
#   complete(year, month = 1:12) %>% 
#   mutate(across(where(is.numeric), 
#                 ~.x %>% 
#                   replace_na(0)), 
#          sentence_year_month = 
#            as.Date(paste0(year, "-", month, "-01"))) %>% 
#   dplyr::select(-c(year, month))
#   

####Visualizations####
##TV Incarceration by Month
tv_incarc_viz <- 
  tv_incarc_by_month %>% 
  filter(sentence_year_month < "2025-12-01") %>% 
  dplyr::select(sentence_year_month, total_sentences, total_probationers, 
                total_sentences_confinement, total_probationers_confinement) %>% 
  pivot_longer(cols = starts_with("total_"), 
               names_to = "count_type", 
               values_to = "count") %>% 
  mutate(count_type = 
           count_type %>% 
           str_replace_all("^total_", "")) %>% 
  separate_wider_delim(cols = "count_type", delim = "_", names = c("count_type", "sentence_type"), too_few = "align_start") %>% 
  mutate(sentence_type = 
           sentence_type %>% 
           replace_na("total"), 
         across(ends_with("_type"), 
                ~.x %>% 
                  str_to_title() %>% 
                  str_replace_all("^Sentences$", "Probation Sentences"))) %>% 
  ggplot(aes(x = sentence_year_month, y = count, color = sentence_type)) + 
  geom_smooth() +
  facet_wrap(~ count_type) +
  theme_minimal() + 
  labs(color = "Sentence Type", 
       x = "Year", 
       y = "Count", 
       title = "Technical Violation Counts by Sentence Type over Time") + 
  scale_color_manual(
    values = c("Confinement" = "#E69F00", "Total" = "#0072B2"),
    name = "Sentence Type"
  )

ggsave(tv_incarc_viz, filename = here("plots", "tv_imprisonment_by_sentence_type.pdf"), width = 15, height = 10)
ggsave(tv_incarc_viz, filename = here("plots", "tv_imprisonment_by_sentence_type.png"), width = 15, height = 10)

##Imprisonment Rate Total
imprisonment_rate_viz_total <- 
  tv_incarc_by_month %>% 
  dplyr::select(sentence_year_month, starts_with("imprisonment_rate")) %>% 
  mutate(county = "Total") %>% 
  pivot_longer(cols = starts_with("imprisonment_rate"), 
               names_to = "rate_type", 
               values_to = "imprisonment_rate") %>% 
  mutate(rate_type = 
           rate_type %>% 
           str_replace_all("^imprisonment_rate_by_", "") %>% 
           str_replace_all("probationer", "probationers") %>% 
           str_replace_all("sentence", "probation sentences") %>% 
           str_to_title()) %>% 
  ggplot(aes(x = sentence_year_month, y = imprisonment_rate, color = rate_type)) + 
  geom_smooth() +
  theme_minimal() + 
  labs(color = "Rate Type", 
       x = "Year", 
       y = "Confinement Rate", 
       title = "Confinement Rate for Technical Violations by Year") + 
  scale_y_continuous(labels = scales::percent) + 
  scale_color_manual(
    values = c("Probation Sentences" = "#E69F00", "Probationers" = "#0072B2"),
    name = "Rate Type"
  )

ggsave(imprisonment_rate_viz_total, filename = here("plots", "tv_total_imprisonment_rates_by_rate_type.pdf"), width = 15, height = 10)
ggsave(imprisonment_rate_viz_total, filename = here("plots", "tv_total_imprisonment_rates_by_rate_type.png"), width = 15, height = 10)

##Imprisonment Rate by County - Line Graph
imprisonment_rate_viz <- 
  tv_incarc_by_month_county %>% 
  dplyr::select(sentence_year_month, county, starts_with("imprisonment_rate")) %>% 
  bind_rows(tv_incarc_by_month %>% 
              dplyr::select(sentence_year_month, starts_with("imprisonment_rate")) %>% 
              mutate(county = "Total")) %>% 
  pivot_longer(cols = starts_with("imprisonment_rate"), 
               names_to = "rate_type", 
               values_to = "imprisonment_rate") %>% 
  mutate(rate_type = 
           rate_type %>% 
           str_replace_all("^imprisonment_rate_by_", "") %>% 
           str_replace_all("probationer", "probationers") %>% 
           str_replace_all("sentence", "probation sentences") %>% 
           str_to_title(),
         county = 
           county %>% 
           fct_relevel("Total")) %>% 
  arrange(county) %>% 
  ggplot(aes(x = sentence_year_month, y = imprisonment_rate, color = rate_type)) + 
  geom_smooth() +
  facet_wrap(~ county, nrow = 7) + 
  theme_minimal() + 
  ylim(0, 1) +
  labs(color = "Rate Type", 
       x = "Year", 
       y = "Confinement Rate", 
       title = "Confinement Rate for Technical Violations by County and Year") + 
  scale_color_manual(
    values = c("Probation Sentences" = "#E69F00", "Probationers" = "#0072B2"),
    name = "Rate Type"
  ) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_y_continuous(labels = scales::percent)

ggsave(imprisonment_rate_viz, filename = here("plots", "tv_imprisonment_rates_by_county.pdf"), width = 15, height = 20)
ggsave(imprisonment_rate_viz, filename = here("plots", "tv_imprisonment_rates_by_county.png"), width = 15, height = 20)

##Imprison Rate by County and Year 
imprisonment_rate_by_county_year_type <- 
  tv_incarc_by_month_county %>% 
  dplyr::select(sentence_year_month, county, starts_with("imprisonment_rate")) %>% 
  bind_rows(tv_incarc_by_month %>% 
              dplyr::select(sentence_year_month, starts_with("imprisonment_rate")) %>% 
              mutate(county = "Total")) %>% 
  pivot_longer(cols = starts_with("imprisonment_rate"), 
               names_to = "rate_type", 
               values_to = "imprisonment_rate") %>% 
  mutate(rate_type = 
           rate_type %>% 
           str_replace_all("^imprisonment_rate_by_", "") %>% 
           str_replace_all("probationer", "probationers") %>% 
           str_replace_all("sentence", "probation sentences") %>% 
           str_to_title(),
         county = 
           county %>% 
           fct_relevel("Total"), 
         total_imprisonment_rate = 
           ifelse(county == "Total", imprisonment_rate, NA)) %>% 
  group_by(sentence_year_month, rate_type) %>% 
  tidyr::fill(total_imprisonment_rate, .direction = "downup") %>% 
  ungroup() %>% 
  filter(county != "Total") %>% 
  pivot_longer(cols = ends_with("imprisonment_rate"), 
               names_to = "imprisonment_rate_type", 
               values_to = "imprisonment_rate") %>% 
  arrange(county) %>% 
  mutate(imprisonment_rate_type = 
           imprisonment_rate_type %>% 
           str_replace_all("^imprisonment_rate$", "County") %>% 
           str_replace_all("^total_imprisonment_rate$", "Total")) %>% 
  group_split(rate_type, .keep = TRUE) %>% 
  map(~.x %>% 
        ggplot(aes(x = sentence_year_month, y = imprisonment_rate, color = imprisonment_rate_type)) + 
        geom_smooth() +
        facet_wrap(~ county, nrow = 7) + 
        theme_minimal() + 
        labs(color = "", 
             x = "Year", 
             y = "Confinement Rate", 
             title = paste0("Confinement Rate for Technical Violations by County and Year - Total ", unique(.x$rate_type))) + 
        scale_color_manual(
          values = c("Total" = "#E69F00", "County" = "#0072B2"),
          name = "Rate Type"
        ) + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
        scale_y_continuous(labels = scales::percent))

ggsave(imprisonment_rate_by_county_year_type[[1]], filename = here("plots", "tv_imprisonment_rates_by_county_probation_sentences.pdf"), width = 15, height = 20)
ggsave(imprisonment_rate_by_county_year_type[[2]], filename = here("plots", "tv_imprisonment_rates_by_county_probationers.pdf"), width = 15, height = 20)
ggsave(imprisonment_rate_by_county_year_type[[1]], filename = here("plots", "tv_imprisonment_rates_by_county_probation_sentences.png"), width = 15, height = 20)
ggsave(imprisonment_rate_by_county_year_type[[2]], filename = here("plots", "tv_imprisonment_rates_by_county_probationers.png"), width = 15, height = 20)
  
  
  
  
  
  
  
  
  
  
  