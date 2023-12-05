# VISIT DATA
library(here)


## UK -----------

if (country_setting == "UK") {

# Join visit_detail and provider tables
joined_visit_provider_tables <- cdm$visit_detail %>% inner_join(cdm$provider)

## Sub-setting the visit data
visit_data <- joined_visit_provider_tables %>%
  left_join(provider_cost_inputs, by = "specialty_source_value", copy=TRUE) %>%
  filter(Include == "1") %>% # Filter for only meaningful specialties
  select(person_id, provider_id, specialty_source_value, visit_detail_start_date, visit_detail_id, unit_cost) %>%
  filter(visit_detail_start_date >= study_start_date & # include only visits in the study period
           visit_detail_start_date <= study_end_date) %>%
  rename(subject_id=person_id) %>% # just renaming for consistency with cohort data
  collect()

## Keep only distinct rows - eliminate all visits in the same day with the same specialist by the same patient

visits_specialist_day <- visit_data %>% distinct(visit_detail_start_date, subject_id, specialty_source_value) %>% tally()

visit_data <- visit_data %>% distinct(visit_detail_start_date, subject_id, specialty_source_value, .keep_all=TRUE)

visit_data <- visit_data %>% 
  rename(specialty = specialty_source_value) # renaming for consistency with other countries as this variable is used for the HCRU and costs too

}

## France ---------

if (country_setting == "France") {
  
## Join visit_occurrence and provider tables - visit_detail is empty for IQVIA
joined_visit_provider_tables <- cdm$visit_occurrence %>% inner_join(cdm$provider)

## check that visit_start_date and visit_end_date are the same
check_dates <- joined_visit_provider_tables %>%
  filter(visit_start_date != visit_end_date) %>%
  tally()

check_dates <- collect(check_dates)

## Sub-setting the visit data
visit_data <- joined_visit_provider_tables %>%
  select(person_id, provider_id, specialty_concept_id, visit_start_date, visit_occurrence_id, visit_concept_id, unit_cost) %>% # in France we also have visit_concept_id 
  filter(visit_start_date >= study_start_date & # include only visits in the study period
           visit_start_date <= study_end_date) %>%
  rename(subject_id=person_id) %>% # just renaming for consistency with cohort data
  collect()

## Keep only distinct rows - eliminate all visits in the same day with the same specialist and of the same type, by the same patient
visits_specialist_day <- visit_data %>% distinct(visit_start_date, subject_id, specialty_concept_id) %>% tally()

visit_data <-visit_data %>% distinct(visit_start_date, subject_id, specialty_concept_id, visit_concept_id, .keep_all=TRUE)

visit_data <- visit_data %>% 
  rename(specialty = specialty_concept_id) # renaming for consistency with other countries as this variable is used for the HCRU and costs too


# Check table

visit_type_by_specialty <- visit_data %>%
  group_by(specialty, visit_concept_id) %>%
  summarise(visit_count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = visit_concept_id, values_from = visit_count, values_fill = list(visit_count = 0))


## Filter visit_data by concept_id

# visit_data_home <- visit_data %>% filter(visit_concept_id == 581476)
# visit_data_tele <- visit_data %>% filter(visit_concept_id == 5083)
# visit_data_office <- visit_data %>% filter(visit_concept_id == 581477)

}

## Germany -----------------
if (country_setting == "Germany") {
  
## Join visit_occurrence and provider tables - visit_detail is empty for IQVIA
joined_visit_provider_tables <- cdm$visit_occurrence %>% inner_join(cdm$provider)

## check that visit_start_date and visit_end_date are the same
check_dates <- joined_visit_provider_tables %>%
  filter(visit_start_date != visit_end_date) %>%
  tally()

check_dates <- collect(check_dates)

## Sub-setting the visit data
visit_data <- joined_visit_provider_tables %>%
  select(person_id, provider_id, specialty_concept_id, visit_start_date, visit_occurrence_id, unit_cost) %>% 
  filter(visit_start_date >= study_start_date & # include only visits in the study period
           visit_start_date <= study_end_date) %>%
  rename(subject_id=person_id) %>% # just renaming for consistency with cohort data
  collect()

## Keep only distinct rows - eliminate all visits in the same day with the same specialist by the same patient
visits_specialist_day <- visit_data %>% distinct(visit_start_date, subject_id, specialty_concept_id) %>% tally()

visit_data <-visit_data %>% distinct(visit_start_date, subject_id, specialty_concept_id, .keep_all=TRUE)

visit_data <- visit_data %>% 
  rename(specialty = specialty_concept_id) # renaming for consistency with other countries as this variable is used for the HCRU and costs too

}


## Italy -------
if (country_setting == "Italy") {
  
## Join visit_occurrence and provider tables - visit_detail is empty for IQVIA
joined_visit_provider_tables <- cdm$visit_occurrence %>% inner_join(cdm$provider)

## check that visit_start_date and visit_end_date are the same
check_dates <- joined_visit_provider_tables %>%
  filter(visit_start_date != visit_end_date) %>%
  tally()

check_dates <- collect(check_dates)

## Sub-setting the visit data
visit_data <- joined_visit_provider_tables %>%
  select(person_id, provider_id, specialty_source_value, visit_start_date, visit_occurrence_id, unit_cost) %>%
  filter(visit_start_date >= study_start_date & # include only visits in the study period
           visit_start_date <= study_end_date) %>%
  rename(subject_id=person_id) %>% # just renaming for consistency with cohort data
  collect()

## Keep only distinct rows - eliminate all visits in the same day with the same specialist by the same patient
visits_specialist_day <- visit_data %>% distinct(visit_start_date, subject_id, specialty_source_value) %>% tally()

visit_data <-visit_data %>% distinct(visit_start_date, subject_id, specialty_source_value, .keep_all=TRUE)

visit_data <- visit_data %>% 
  rename(specialty = specialty_source_value) # renaming for consistency with other countries as this variable is used for the HCRU and costs too

}

## Spain ----

if (country_setting == "Spain") {
  

## Join visit_occurrence and provider tables - visit_detail is empty for IQVIA
joined_visit_provider_tables <- cdm$visit_occurrence %>% inner_join(cdm$provider)

## check that visit_start_date and visit_end_date are the same
check_dates <- joined_visit_provider_tables %>%
  filter(visit_start_date != visit_end_date) %>%
  tally()

check_dates <- collect(check_dates)

## Sub-setting the visit data
visit_data <- joined_visit_provider_tables %>%
  select(person_id, provider_id, specialty_concept_id, visit_start_date, visit_occurrence_id, visit_concept_id, unit_cost) %>% # in France we also have visit_concept_id 
  filter(visit_start_date >= study_start_date & # include only visits in the study period
           visit_start_date <= study_end_date) %>%
  rename(subject_id=person_id) %>% # just renaming for consistency with cohort data
  collect()

## Keep only distinct rows - eliminate all visits in the same day with the same specialist and of the same type, by the same patient
visits_specialist_day <- visit_data %>% distinct(visit_start_date, subject_id, specialty_concept_id) %>% tally()

visit_data <-visit_data %>% distinct(visit_start_date, subject_id, specialty_concept_id, visit_concept_id, .keep_all=TRUE)

visit_data <- visit_data %>% 
  rename(specialty = specialty_concept_id) # renaming for consistency with other countries as this variable is used for the HCRU and costs too


# Check table

visit_type_by_specialty <- visit_data %>%
  group_by(specialty, visit_concept_id) %>%
  summarise(visit_count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = visit_concept_id, values_from = visit_count, values_fill = list(visit_count = 0))



## Filter visit_data by concept_id

# visit_data_home <- visit_data %>% filter(visit_concept_id == 581476)
# visit_data_tele <- visit_data %>% filter(visit_concept_id == 5083)
# visit_data_outp <- visit_data %>% filter(visit_concept_id == 9202) ## is this primary care?
# visit_data_inp  <- visit_data %>% filter(visit_concept_id == 9201) ## this is not primary care
# visit_data_intcare <- visit_data %>% filter(visit_concept_id == 32037) ## this is not primary care

}

## Netherlands -----
if (country_setting == "Netherlands") {
  
## First, I join visit_occurrence and note tables (visit_occurrence does not have values for provider_id, visit_detail is empty)
  
joined_visit_note_tables <- cdm$visit_occurrence %>% inner_join(cdm$note, by = "person_id")

## Second, I join the resulting table with provider table where I can find the specialty

joined_visit_provider_tables <- joined_visit_note_tables %>% inner_join(cdm$provider) # I will use the specialty_source_value that is in the provider table

## ADD A FREQUENCY TABLE FOR SPECIALTY_CONCEPT_ID and SPECIALTY_SOURCE_VALUE

## check that visit_start_date and visit_end_date are the same
check_dates <- joined_visit_provider_tables %>%
  filter(visit_start_date != visit_end_date) %>%
  tally()

check_dates <- collect(check_dates)

# check table - specialty_concept_id (as opposed to specialty_source_value which I use here)

frequency_table_concept_id <- joined_visit_provider_tables %>%
  group_by(specialty_concept_id) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  arrange(desc(Count))

frequency_table_source_value <- joined_visit_provider_tables %>%
  group_by(specialty_source_value) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  arrange(desc(Count))

combined_frequency_table <- bind_cols(frequency_table_concept_id, frequency_table_source_value)

## Sub-setting the visit data
visit_data <- joined_visit_provider_tables %>%
  select(person_id, provider_id, specialty_source_value, visit_start_date, visit_occurrence_id, visit_concept_id, visit_source_value, unit_cost) %>% # in France we also have visit_concept_id 
  filter(visit_start_date >= study_start_date & # include only visits in the study period
           visit_start_date <= study_end_date) %>%
  rename(subject_id=person_id) %>% # just renaming for consistency with cohort data
  collect()

## Keep only distinct rows - eliminate all visits in the same day with the same specialist and of the same type, by the same patient
visits_specialist_day <- visit_data %>% distinct(visit_start_date, subject_id, specialty_source_value) %>% tally()

visit_data <-visit_data %>% distinct(visit_start_date, subject_id, specialty_source_value, visit_concept_id, .keep_all=TRUE)

## Delete communications

communications <- visit_data %>% filter (visit_source_value == "Communication") %>% tally()

visit_data <-visit_data %>% 
  filter(visit_source_value != "Communication")

visit_data <- visit_data %>% 
  rename(specialty = specialty_concept_id) # renaming for consistency with other countries as this variable is used for the HCRU and costs too

# Check table - type of visit

visit_type_by_specialty <- visit_data %>%
  group_by(specialty, visit_concept_id) %>%
  summarise(visit_count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = visit_concept_id, values_from = visit_count, values_fill = list(visit_count = 0))


## Filter visit_data by concept_id

# visit_data_home <- visit_data %>% filter(visit_concept_id == 581476)
# visit_data_outp <- visit_data %>% filter(visit_concept_id == 9202) ## is this primary care?
# visit_data_inp <- visit_data %>% filter(visit_concept_id == 9201) ## this is not primary care
# visit_data_er <- visit_data %>% filter(visit_concept_id == 9203) ## this is not primary care
# visit_data_er_inp <- visit_data %>% filter(visit_concept_id == 262) ## this is not primary care
}
