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
  select(person_id, provider_id, specialty_source_value, visit_detail_start_date, visit_detail_id) %>%
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
  select(person_id, provider_id, specialty_concept_id, visit_start_date, visit_id, visit_concept_id) %>% # in France we also have visit_concept_id 
  filter(visit_start_date >= study_start_date & # include only visits in the study period
           visit_start_date <= study_end_date) %>%
  rename(subject_id=person_id) %>% # just renaming for consistency with cohort data
  collect()

## Keep only distinct rows - eliminate all visits in the same day with the same specialist and of the same type, by the same patient
visits_specialist_day <- visit_data %>% distinct(visit_detail_start_date, subject_id, specialty_source_value) %>% tally()

visit_data <-visit_data %>% distinct(visit_start_date, subject_id, specialty_concept_id, visit_concept_id, .keep_all=TRUE)

visit_data <- visit_data %>% 
  rename(specialty = specialty_concept_id) # renaming for consistency with other countries as this variable is used for the HCRU and costs too

## Filter visit_data by concept_id

visit_data_home <- visit_data %>% filter(visit_concept_id == 581476)
visit_data_tele <- visit_data %>% filter(visit_concept_id == 5083)
visit_data_office <- visit_data %>% filter(visit_concept_id == 581477)

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
  select(person_id, provider_id, specialty_concept_id, visit_start_date, visit_id) %>% 
  filter(visit_start_date >= study_start_date & # include only visits in the study period
           visit_start_date <= study_end_date) %>%
  rename(subject_id=person_id) %>% # just renaming for consistency with cohort data
  collect()

## Keep only distinct rows - eliminate all visits in the same day with the same specialist by the same patient
visits_specialist_day <- visit_data %>% distinct(visit_detail_start_date, subject_id, specialty_source_value) %>% tally()

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
  select(person_id, provider_id, specialty_source_value, visit_start_date, visit_id) %>%
  filter(visit_start_date >= study_start_date & # include only visits in the study period
           visit_start_date <= study_end_date) %>%
  rename(subject_id=person_id) %>% # just renaming for consistency with cohort data
  collect()

## Keep only distinct rows - eliminate all visits in the same day with the same specialist by the same patient
visits_specialist_day <- visit_data %>% distinct(visit_detail_start_date, subject_id, specialty_source_value) %>% tally()

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
  select(person_id, provider_id, specialty_concept_id, visit_start_date, visit_id, visit_concept_id) %>% # in France we also have visit_concept_id 
  filter(visit_start_date >= study_start_date & # include only visits in the study period
           visit_start_date <= study_end_date) %>%
  rename(subject_id=person_id) %>% # just renaming for consistency with cohort data
  collect()

## Keep only distinct rows - eliminate all visits in the same day with the same specialist and of the same type, by the same patient
visits_specialist_day <- visit_data %>% distinct(visit_detail_start_date, subject_id, specialty_source_value) %>% tally()

visit_data <-visit_data %>% distinct(visit_start_date, subject_id, specialty_concept_id, visit_concept_id, .keep_all=TRUE)

visit_data <- visit_data %>% 
  rename(specialty = specialty_concept_id) # renaming for consistency with other countries as this variable is used for the HCRU and costs too

## Filter visit_data by concept_id

visit_data_home <- visit_data %>% filter(visit_concept_id == 581476)
visit_data_tele <- visit_data %>% filter(visit_concept_id == 5083)
visit_data_outp <- visit_data %>% filter(visit_concept_id == 9202) ## is this primary care?
visit_data_inp <- visit_data %>% filter(visit_concept_id == 9201) ## this is not primary care
visit_data_intcare <- visit_data %>% filter(visit_concept_id == 32037) ## this is not primary care

}

## Netherlands -----
if (country_setting == "Netherlands") {
  
## Join visit_occurrence and provider tables - visit_detail is empty for IQVIA
joined_visit_provider_tables <- cdm$visit_occurrence %>% inner_join(cdm$provider)

## check that visit_start_date and visit_end_date are the same
check_dates <- joined_visit_provider_tables %>%
  filter(visit_start_date != visit_end_date) %>%
  tally()

check_dates <- collect(check_dates)

## Sub-setting the visit data
visit_data <- joined_visit_provider_tables %>%
  select(person_id, provider_id, specialty_concept_id, visit_start_date, visit_id, visit_concept_id, visit_source_value) %>% # in France we also have visit_concept_id 
  filter(visit_start_date >= study_start_date & # include only visits in the study period
           visit_start_date <= study_end_date) %>%
  rename(subject_id=person_id) %>% # just renaming for consistency with cohort data
  collect()

## Keep only distinct rows - eliminate all visits in the same day with the same specialist and of the same type, by the same patient
visits_specialist_day <- visit_data %>% distinct(visit_detail_start_date, subject_id, specialty_source_value) %>% tally()

visit_data <-visit_data %>% distinct(visit_start_date, subject_id, specialty_concept_id, visit_concept_id, .keep_all=TRUE)

## Delete communications

communications <- visit_data %>% filter (visit_source_value == "Communication") %>% tally()

visit_data <-visit_data %>% 
  filter(visit_source_value != "Communication")

visit_data <- visit_data %>% 
  rename(specialty = specialty_concept_id) # renaming for consistency with other countries as this variable is used for the HCRU and costs too

## Filter visit_data by concept_id

visit_data_home <- visit_data %>% filter(visit_concept_id == 581476)
visit_data_outp <- visit_data %>% filter(visit_concept_id == 9202) ## is this primary care?
visit_data_inp <- visit_data %>% filter(visit_concept_id == 9201) ## this is not primary care
visit_data_er <- visit_data %>% filter(visit_concept_id == 9203) ## this is not primary care
visit_data_er_inp <- visit_data %>% filter(visit_concept_id == 262) ## this is not primary care
}
