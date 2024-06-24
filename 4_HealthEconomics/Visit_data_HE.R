# VISIT DATA - note only UK has been fixed for now
## UK -----------

if (country_setting == "UK") {
  info(logger, "PRODUCING VISIT DATA FOR UK")
  # Join visit_detail and provider tables
  joined_visit_provider_tables <- cdm$visit_detail %>% 
    dplyr::inner_join(cdm$provider) 
  
  ## Sub-setting the visit data
  cdm[["visit_data"]] <- joined_visit_provider_tables %>%
    dplyr::inner_join(provider_cost_inputs %>% dplyr::filter(Include == "1"), by = "specialty_source_value", copy=TRUE) %>% # Filter for only meaningful specialties
    dplyr::select(person_id, provider_id, specialty_source_value, visit_detail_start_date, visit_detail_id, unit_cost, visit_detail_concept_id) %>%
    dplyr::filter(visit_detail_start_date >= study_start_date & # include only visits in the study period
                   visit_detail_start_date <= study_end_date) %>%
    dplyr::filter(visit_detail_concept_id == 581477) %>% # including only primary care
    dplyr::rename(subject_id=person_id) %>% # just renaming for consistency with cohort data
    dplyr::distinct(visit_detail_start_date, subject_id, specialty_source_value, .keep_all=TRUE) %>% # Keep only distinct rows - eliminate all visits in the same day with the same specialist by the same patient
    dplyr::rename(specialty = specialty_source_value) %>%  # renaming for consistency with other countries as this variable is used for the HCRU and costs too
    computeQuery()
  info(logger, "PRODUCING VISIT DATA FOR UK IS DONE")
}

## France ---------

if (country_setting == "France") {
  info(logger, "PRODUCING VISIT DATA FOR FRANCE")
  ## Join visit_occurrence and provider tables - visit_detail is empty for IQVIA
  joined_visit_provider_tables <- cdm$visit_occurrence %>% 
    dplyr::inner_join(cdm$provider) %>% 
    dplyr::compute()
  
  ## check that visit_start_date and visit_end_date are the same
  check_dates <- joined_visit_provider_tables %>%
    dplyr::filter(visit_start_date != visit_end_date) %>%
    dplyr::tally() %>% 
    dplyr::collect()
  
  ## Sub-setting the visit data
  cdm[["visit_data"]] <- joined_visit_provider_tables %>%
    dplyr::left_join(provider_cost_inputs, by = "specialty_concept_id", copy=TRUE) %>%
    dplyr::select(person_id, provider_id, specialty_concept_id, visit_start_date, visit_occurrence_id, visit_concept_id, unit_cost) %>%  
    dplyr::filter(visit_start_date >= study_start_date & # include only visits in the study period
                    visit_start_date <= study_end_date) %>%
    dplyr::rename(subject_id=person_id) %>% # just renaming for consistency with cohort data
    dplyr::distinct(visit_start_date, subject_id, specialty_concept_id, .keep_all=TRUE) %>% # Keep only distinct rows - eliminate all visits in the same day with the same specialist by the same patient
    dplyr::rename(specialty = specialty_concept_id) %>%  # renaming for consistency with other countries as this variable is used for the HCRU and costs too
    computeQuery(
      name = "visit_data", 
      temporary = FALSE, 
      schema = attr(cdm, "write_schema"), 
      overwrite = TRUE
    )
  
  # Check table
  
  visit_type_by_specialty <- cdm[["visit_data"]] %>%
    dplyr::group_by(specialty, visit_concept_id) %>%
    dplyr::summarise(visit_count = n(), .groups = 'drop') %>%
    pivot_wider(names_from = visit_concept_id, values_from = visit_count, values_fill = list(visit_count = 0)) %>% 
    dplyr::collect()
  
  
  ## Filter visit_data by concept_id
  
  # visit_data_home <- visit_data %>% filter(visit_concept_id == 581476)
  # visit_data_tele <- visit_data %>% filter(visit_concept_id == 5083)
  # visit_data_office <- visit_data %>% filter(visit_concept_id == 581477)
  info(logger, "PRODUCING VISIT DATA FOR FRANCE IS DONE")
  
}

## Germany -----------------
if (country_setting == "Germany") {
  info(logger, "PRODUCING VISIT DATA FOR GERMANY")
  ## Join visit_occurrence and provider tables - visit_detail is empty for IQVIA
  joined_visit_provider_tables <- cdm$visit_occurrence %>% 
    dplyr::inner_join(cdm$provider) %>% 
    dplyr::compute()
  
  ## check that visit_start_date and visit_end_date are the same
  check_dates <- joined_visit_provider_tables %>%
    dplyr::filter(visit_start_date != visit_end_date) %>%
    dplyr::tally() %>% 
    dplyr::collect()
  
  ## Sub-setting the visit data
  cdm[["visit_data"]] <- joined_visit_provider_tables %>%
    dplyr::left_join(provider_cost_inputs, by = "specialty_concept_id", copy=TRUE) %>%
    dplyr::select(person_id, provider_id, specialty_concept_id, visit_start_date, visit_occurrence_id, visit_concept_id, unit_cost) %>% 
    dplyr::filter(visit_start_date >= study_start_date & # include only visits in the study period
                    visit_start_date <= study_end_date) %>%
    dplyr::rename(subject_id=person_id) %>% # just renaming for consistency with cohort data
    dplyr::distinct(visit_start_date, subject_id, specialty_concept_id, .keep_all=TRUE) %>% # Keep only distinct rows - eliminate all visits in the same day with the same specialist by the same patient
    dplyr::rename(specialty = specialty_concept_id) %>%  # renaming for consistency with other countries as this variable is used for the HCRU and costs too
    computeQuery(
      name = "visit_data", 
      temporary = FALSE, 
      schema = attr(cdm, "write_schema"), 
      overwrite = TRUE
    )
  
  visit_type_by_specialty <- cdm[["visit_data"]] %>%
    dplyr::group_by(specialty, visit_concept_id) %>%
    dplyr::summarise(visit_count = n(), .groups = 'drop') %>%
    pivot_wider(names_from = visit_concept_id, values_from = visit_count, values_fill = list(visit_count = 0)) %>% 
    dplyr::collect()
  
  # In Germany we don't have the detail for concept_id, all visits are "office".
  info(logger, "PRODUCING VISIT DATA FOR GERMANY IS DONE")
}

## Italy -------
if (country_setting == "Italy") {
  info(logger, "PRODUCING VISIT DATA FOR ITALY")
  ## Join visit_occurrence and provider tables - visit_detail is empty for IQVIA
  joined_visit_provider_tables <- cdm$visit_occurrence %>% 
    dplyr::inner_join(cdm$provider, by = "provider_id")
  
  ## check that visit_start_date and visit_end_date are the same
  check_dates <- joined_visit_provider_tables %>%
    dplyr::filter(visit_start_date != visit_end_date) %>%
    dplyr::tally() %>% 
    dplyr::collect()
  
  ## Sub-setting the visit data
  cdm[["visit_data"]] <- joined_visit_provider_tables %>%
    dplyr::left_join(provider_cost_inputs, by = "specialty_source_value", copy=TRUE) %>% # here even if we use visit_occurrence we use specialty_source_value
    dplyr::select(person_id, provider_id, specialty_source_value, visit_start_date, visit_occurrence_id, visit_concept_id, unit_cost) %>%  
    dplyr::filter(visit_start_date >= study_start_date & # include only visits in the study period
                    visit_start_date <= study_end_date) %>%
    dplyr::rename(subject_id=person_id) %>% # just renaming for consistency with cohort data
    dplyr::distinct(visit_start_date, subject_id, specialty_source_value, .keep_all=TRUE) %>% # Keep only distinct rows - eliminate all visits in the same day with the same specialist by the same patient
    dplyr::rename(specialty = specialty_source_value) %>%  # renaming for consistency with other countries as this variable is used for the HCRU and costs too
    computeQuery(
      name = "visit_data", 
      temporary = FALSE, 
      schema = attr(cdm, "write_schema"), 
      overwrite = TRUE
    )  
  
  # In Italy we don't have the detail for concept_id, all visits are "office".
  info(logger, "PRODUCING VISIT DATA FOR ITALY IS DONE")
}

## Spain ----

if (country_setting == "Spain") {
  info(logger, "PRODUCING VISIT DATA FOR SPAIN")
  ## Join visit_occurrence and provider tables - visit_detail is not empty but we cannot interpret source values 
  joined_visit_provider_tables <- cdm$visit_occurrence %>% 
    dplyr::select(-"care_site_id") %>% 
    dplyr::inner_join(cdm$provider, by = "provider_id") %>% 
    dplyr::compute()
  
  ## check that visit_start_date and visit_end_date are the same
  check_dates <- joined_visit_provider_tables %>%
    dplyr::filter(visit_start_date != visit_end_date) %>%
    dplyr::tally() %>% 
    dplyr::collect()
  
  ## Sub-setting the visit data
  cdm[["visit_data"]] <- joined_visit_provider_tables %>%
    dplyr::left_join(provider_cost_inputs, by = "specialty_concept_id", copy=TRUE) %>%
    dplyr::select(person_id, provider_id, specialty_concept_id, visit_start_date, visit_occurrence_id, visit_concept_id, unit_cost) %>%  
    dplyr::filter(visit_start_date >= study_start_date & # include only visits in the study period
                    visit_start_date <= study_end_date) %>%
    dplyr::filter(visit_concept_id != 9201 & visit_concept_id != 32037) %>% # including only primary care
    dplyr::rename(subject_id=person_id) %>% # just renaming for consistency with cohort data
    dplyr::distinct(visit_start_date, subject_id, specialty_concept_id, .keep_all=TRUE) %>% # Keep only distinct rows - eliminate all visits in the same day with the same specialist by the same patient
    dplyr::rename(specialty = specialty_concept_id) %>%  # renaming for consistency with other countries as this variable is used for the HCRU and costs too
    computeQuery(
      name = "visit_data", 
      temporary = FALSE, 
      schema = attr(cdm, "write_schema"), 
      overwrite = TRUE
    )    
  
  # Check table
  
  visit_type_by_specialty <- cdm[["visit_data"]] %>%
    dplyr::group_by(specialty, visit_concept_id) %>%
    dplyr::summarise(visit_count = n(), .groups = 'drop') %>%
    pivot_wider(names_from = visit_concept_id, values_from = visit_count, values_fill = list(visit_count = 0)) %>% 
    dplyr::collect()
  
  ## Filter visit_data by concept_id
  
  # visit_data_home <- visit_data %>% filter(visit_concept_id == 581476)
  # visit_data_tele <- visit_data %>% filter(visit_concept_id == 5083)
  # visit_data_outp <- visit_data %>% filter(visit_concept_id == 9202) ## is this primary care?
  # visit_data_inp  <- visit_data %>% filter(visit_concept_id == 9201) ## this is not primary care
  # visit_data_intcare <- visit_data %>% filter(visit_concept_id == 32037) ## this is not primary care
  info(logger, "PRODUCING VISIT DATA FOR SPAIN IS DONE")
}

## Netherlands -----
if (country_setting == "Netherlands") {
  info(logger, "PRODUCING VISIT DATA FOR NETHERLANDS")
  joined_visit_provider_tables <- cdm$visit_occurrence # we cannot link visits with provider in the Netherlands, visit_detail is also empty
  
  ### I keep the name "joined" even if it is not
  #joined_visit_provider_tables <- cdm$visit_occurrence %>% 
  #  dplyr::inner_join(cdm$provider)
  
  ## check that visit_start_date and visit_end_date are the same
  check_dates <- joined_visit_provider_tables %>%
    dplyr::filter(visit_start_date != visit_end_date) %>%
    dplyr::tally() %>% 
    dplyr::collect()
  
  
  ## Sub-setting the visit data
  cdm[["visit_data"]] <- joined_visit_provider_tables %>%
    dplyr::inner_join(provider_cost_inputs %>% dplyr::filter(Include == "1"), by = "visit_concept_id", copy=TRUE) %>%
    dplyr::select(person_id, visit_start_date, visit_occurrence_id, visit_concept_id, visit_source_value) %>%  
    dplyr::filter(visit_start_date >= study_start_date & # include only visits in the study period
                    visit_start_date <= study_end_date) %>%
    dplyr::filter(visit_source_value != "Communication") %>% # here I remove communications
    dplyr::rename(subject_id=person_id) %>% # just renaming for consistency with cohort data
    dplyr::rename(type = visit_concept_id) %>%  # here we will report by type not specialty
    #dplyr::distinct(visit_start_date, subject_id, .keep_all=TRUE) %>% # Keep only distinct rows - eliminate all visits in the same day by the same patient
    computeQuery(
      name = "visit_data", 
      temporary = FALSE, 
      schema = attr(cdm, "write_schema"), 
      overwrite = TRUE
    )  
  # visits_check<-list()
  # target_matched_visit_check <- 
  #   target_matched %>% 
  #   dplyr::inner_join(cdm[["visit_occurrence"]], by = c("subject_id" = "person_id"), copy = T) %>% 
  #   dplyr::filter(visit_start_date<=follow_up_end & visit_start_date >=index_date) %>% 
  #   dplyr::left_join(cdm[["provider"]], by = c("provider_id"), copy = T) %>% 
  #   dplyr::select(subject_id, index_date, follow_up_end, cohort, provider_id, specialty_concept_id, specialty_source_value) %>% 
  #   dplyr::group_by(specialty_concept_id, specialty_source_value) %>% 
  #   dplyr::tally()
  # 
  # c1_matched_to_visit_check <- 
  #   cohort1_matched_to %>% 
  #   dplyr::inner_join(cdm[["visit_occurrence"]], by = c("subject_id" = "person_id"), copy = T) %>% 
  #   dplyr::filter(visit_start_date<=follow_up_end & visit_start_date >=index_date) %>% 
  #   dplyr::left_join(cdm[["provider"]], by = c("provider_id"), copy = T) %>% 
  #   dplyr::select(subject_id, index_date, follow_up_end, cohort, provider_id, specialty_concept_id, specialty_source_value) %>% 
  #   dplyr::group_by(specialty_concept_id, specialty_source_value) %>% 
  #   dplyr::tally()
  # 
  # c1_matched_from_visit_check <- 
  #   cohort1_matched_from %>% 
  #   dplyr::inner_join(cdm[["visit_occurrence"]], by = c("subject_id" = "person_id"), copy = T) %>% 
  #   dplyr::filter(visit_start_date<=follow_up_end & visit_start_date >=index_date) %>% 
  #   dplyr::left_join(cdm[["provider"]], by = c("provider_id"), copy = T) %>% 
  #   dplyr::select(subject_id, index_date, follow_up_end, cohort, provider_id, specialty_concept_id, specialty_source_value) %>% 
  #   dplyr::group_by(specialty_concept_id, specialty_source_value) %>% 
  #   dplyr::tally()
  # 
  # c2_matched_visit_check <- 
  #   cohort2_matched %>% 
  #   dplyr::inner_join(cdm[["visit_occurrence"]], by = c("subject_id" = "person_id"), copy = T) %>% 
  #   dplyr::filter(visit_start_date<=follow_up_end & visit_start_date >=index_date) %>% 
  #   dplyr::left_join(cdm[["provider"]], by = c("provider_id"), copy = T) %>% 
  #   dplyr::select(subject_id, index_date, follow_up_end, cohort, provider_id, specialty_concept_id, specialty_source_value) %>% 
  #   dplyr::group_by(specialty_concept_id, specialty_source_value) %>% 
  #   dplyr::tally()
  # 
  # visits_check[["target_matched_visit_check"]] <- target_matched_visit_check
  # visits_check[["c1_matched_to_visit_check"]] <- c1_matched_to_visit_check
  # visits_check[["c1_matched_from_visit_check"]] <- c1_matched_from_visit_check
  # visits_check[["c2_matched_visit_check"]] <- c2_matched_visit_check
  # 
  # write.xlsx(visits_check, file = here(sub_output_folder, "visits_check.xlsx"))
  
  info(logger, "PRODUCING VISIT DATA FOR NETHERLANDS IS DONE")
}
