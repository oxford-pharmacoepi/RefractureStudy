#v1
cdm[["condition_occurrence_aurum"]] <- cdm[["condition_occurrence"]] %>%
  dplyr::filter(condition_type_concept_id == 32827) %>%
  CDMConnector::computeQuery()

cdm[["condition_occurrence_hes"]] <- cdm[["condition_occurrence"]] %>%
  dplyr::filter(condition_type_concept_id == 32829) %>%
  CDMConnector::computeQuery()

### cohort with all records of fracture 
info(logger, "COLLECTING ALL RECORDS OF FRACTURES FROM DENOMINATORS")
cdm[["fracture"]] <- cdm[["denominator"]] %>% 
  left_join(cdm[["condition_occurrence_aurum"]], by = c("subject_id" = "person_id")) %>%
  filter(condition_concept_id %in% any_fracture_id) %>%
  select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date) %>%
  mutate(cohort_start_date = as.Date(as.character(cohort_start_date))) %>%
  mutate(cohort_end_date = as.Date(as.character(cohort_end_date))) %>%
  mutate(fracture_site = if (condition_concept_id %in% hip_fracture_id) {
    "Hip"
  }
  else if (condition_concept_id %in% femur_fracture_id) {
    "Femur"
  }
  else if (condition_concept_id %in% foot_fracture_id) {
    "Foot"
  }
  else if (condition_concept_id %in% tib_fracture_id) {
    "Tibia and Fibula"
  }
  else if (condition_concept_id %in% rib_fracture_id) {
    "Rib"
  }
  else if (condition_concept_id %in% forearm_fracture_id) {
    "Forearm"
  }
  else if (condition_concept_id %in% vert_fracture_id) {
    "Vertebra"
  }
  else if (condition_concept_id %in% pelvic_fracture_id) {
    "Pelvic"
  }  
  else if (condition_concept_id %in% humerus_fracture_id) {
    "Humerus"
  } 
  else if (condition_concept_id %in% nonspecific_fracture_id) {
    "Nonspecific"
  }
  )
fracture_table_aurum <- cdm[["fracture"]] %>% collect()

AttritionReportFrac<-tibble(
  cohort_definition_id = as.integer(1),
  number_records = fracture_table_aurum %>% tally() %>% pull(),
  number_subjects = fracture_table_aurum %>% distinct(subject_id) %>% tally() %>% pull(),
  reason = "Starting Population - Anyone With Fracture(s)"
)

### Loading exclusion criteria tables
info(logger, "LOADING EXCLUSION CRITERIA TABLES")
cdm[["cancer_aurum"]] <- cdm[["denominator"]] %>% 
  left_join(cdm[["condition_occurrence_aurum"]], by = c("subject_id" = "person_id")) %>%
  filter(condition_concept_id %in% cancer_codes) %>%
  select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date) %>%
  mutate(cohort_start_date = as.Date(as.character(cohort_start_date))) %>%
  mutate(cohort_end_date = as.Date(as.character(cohort_end_date))) %>%
  rename(cancer_date =condition_start_date) %>%
  compute()

cdm[["mbd_aurum"]] <- cdm[["denominator"]] %>% 
  left_join(cdm[["condition_occurrence_aurum"]], by = c("subject_id" = "person_id")) %>%
  filter(condition_concept_id %in% mbd_codes) %>%
  select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date) %>%
  mutate(cohort_start_date = as.Date(as.character(cohort_start_date))) %>%
  mutate(cohort_end_date = as.Date(as.character(cohort_end_date))) %>%
  rename(mbd_date =condition_start_date) %>%
  compute()

### Removing cancer records before the birth year
cdm[["cancer_aurum"]] <- cdm[["cancer_aurum"]] %>%
  left_join(cdm[["person"]], by = c("subject_id" = "person_id"), copy = T) %>%
  mutate(cancer_year = lubridate::year(cancer_date)) %>%
  filter(cancer_year > year_of_birth) %>%
  select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, cancer_date) %>%
  compute()

### Removing bone disease records before the birth year
cdm[["mbd_aurum"]] <- cdm[["mbd_aurum"]] %>%
  left_join(cdm[["person"]], by = c("subject_id" = "person_id"), copy = T) %>%
  mutate(mbd_year = lubridate::year(mbd_date)) %>%
  filter(mbd_year > year_of_birth) %>%
  select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, mbd_date) %>%
  compute()

### Removing fractures before the birth year
info(logger, "REMOVING FRACTURES BEFORE THE BIRTH YEAR")
fracture_table_aurum <- fracture_table_aurum %>%
  left_join(cdm[["person"]], by = c("subject_id" = "person_id"), copy = T) %>%
  mutate(fracture_year = as.numeric(format(condition_start_date, "%Y"))) %>%
  filter(fracture_year > year_of_birth) %>%
  select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date, fracture_site)

AttritionReportFrac<- AttritionReportFrac %>% 
  union_all(  
    tibble(
      cohort_definition_id = as.integer(1),
      number_records = fracture_table_aurum %>% tally() %>% pull(),
      number_subjects = fracture_table_aurum %>% distinct(subject_id) %>% tally() %>% pull(),
      reason = "Excluding fracture happening before the birth year"
    )
  ) 

### Removing the fractures that happen on the same day as a trauma 
info(logger, "REMOVING FRACTURES THAT HAPPEN ON THE SAME DAY AS A TRAUMA")
fracture_table_aurum <- fracture_table_aurum %>%
  anti_join(cdm[["condition_occurrence_aurum"]] %>% filter(condition_concept_id %in% trauma_condition), by = c("subject_id" = "person_id", "condition_start_date"), copy = T)

fracture_table_aurum <- fracture_table_aurum %>% 
  anti_join(cdm[["observation_aurum"]] %>% filter(observation_concept_id %in% trauma_observation), by = c("subject_id" = "person_id", "condition_start_date" = "observation_date"), copy = T) 

fracture_table_aurum<-fracture_table_aurum %>%
  anti_join(
    fracture_table_aurum %>% 
      filter(!fracture_site=="Nonspecific") %>% 
      group_by(subject_id, condition_start_date) %>% 
      summarise(number_site = n_distinct(fracture_site), .groups = "drop") %>% 
      filter(number_site>=3),
    by = c("subject_id", "condition_start_date")
  )

AttritionReportFrac<- AttritionReportFrac %>% 
  union_all(  
    tibble(
      cohort_definition_id = as.integer(1),
      number_records = fracture_table_aurum %>% tally() %>% pull(),
      number_subjects = fracture_table_aurum %>% distinct(subject_id) %>% tally() %>% pull(),
      reason = "fracture happening on the same day as a trauma code"
    )
  ) 

### Washout 
info(logger, "APPLYING WASHOUT PERIOD")
sites <- c("Hip", "Femur", "Pelvic", "Vertebra", "Humerus", "Forearm", "Tibia and Fibula", "Rib", "Foot", "Nonspecific")

fracture_table_aurum_back_up <- fracture_table_aurum
index_fractures <- tibble()

k<-2
while(nrow(fracture_table_aurum_back_up)>0){
  fracture_correction <- list()
  for (i in (1:10)){
    fracture_correction[[sites[i]]] <- fracture_table_aurum_back_up %>% 
      group_by(subject_id) %>% 
      filter(fracture_site == sites[[i]] | fracture_site == sites[[10]]) %>% 
      summarise(min_date = min(condition_start_date, na.rm =T)) %>% 
      mutate(site = sites[[i]])
  }
  
  fracture_correction_nonspecific <- list()
  
  for (i in (1:10)){
    fracture_correction_nonspecific <- rbind(fracture_correction_nonspecific, fracture_correction[[i]])
  }
  
  fracture_table_aurum_back_up <- fracture_table_aurum_back_up %>% 
    left_join(fracture_correction_nonspecific, by = c("subject_id", "fracture_site" = "site")) %>% # compute min date
    mutate(gap_to_min_date = condition_start_date - min_date) %>%
    filter(gap_to_min_date == 0 | gap_to_min_date > washout_period[[k]]) %>%
    group_by(subject_id, condition_start_date, fracture_site) %>%
    arrange(condition_concept_id, .by_group = TRUE) %>% 
    filter(row_number()==1) %>%
    ungroup()
  
  fracture_table_aurum_back_up <- fracture_table_aurum_back_up %>%
    left_join(fracture_table_aurum_back_up %>% group_by(subject_id) %>% filter (gap_to_min_date == 0) %>% count(), by = "subject_id") %>%
    filter (!((n>1) & (gap_to_min_date==0) & (fracture_site=="Nonspecific")))
  
  index_fractures <- rbind(index_fractures, fracture_table_aurum_back_up %>% filter(gap_to_min_date==0) %>% select(-min_date, -gap_to_min_date, -n))
  
  fracture_table_aurum_back_up <- fracture_table_aurum_back_up %>% 
    filter(!gap_to_min_date==0) %>%
    select(-min_date, -gap_to_min_date, -n)
}

fracture_table_aurum <- index_fractures

AttritionReportFrac<- AttritionReportFrac %>% 
  union_all(  
    tibble(
      cohort_definition_id = as.integer(1),
      number_records = fracture_table_aurum %>% tally() %>% pull(),
      number_subjects = fracture_table_aurum %>% distinct(subject_id) %>% tally() %>% pull(),
      reason = "Clean out using washout period"
    )
  ) 
rm(index_fractures)

# Applying Hierarchy to multiple records on the same day
info(logger, "APPLYING HIERARCHY TO PREVENT MORE THAN ONE RECORD ON THE SAME DAY FOR THE SAME PERSON")

fracture_table_aurum$fracture_site<-factor(fracture_table_aurum$fracture_site, levels = sites)

fracture_table_aurum <- fracture_table_aurum %>%
  group_by(subject_id, condition_start_date) %>%
  arrange(fracture_site, .by_group = TRUE) %>% 
  filter(row_number()==1) %>%
  ungroup()

AttritionReportFrac<- AttritionReportFrac %>% 
  union_all(  
    tibble(
      cohort_definition_id = as.integer(1),
      number_records = fracture_table_aurum %>% tally() %>% pull(),
      number_subjects = fracture_table_aurum %>% distinct(subject_id) %>% tally() %>% pull(),
      reason = "Excluding records on the same day for the same person according to hierarchy"
    )
  )

### Finalise attrition
AttritionReportFrac <- AttritionReportFrac %>% 
  dplyr::mutate(subjects_excluded = -(number_subjects-lag(number_subjects)), records_excluded = -(number_records - lag(number_records)))

AttritionReportDenom <- AttritionReportDenom %>%
  dplyr::mutate(masked_records = ifelse((excluded_records < minimum_counts & excluded_records>0), paste0("<", minimum_counts), as.integer(.data$excluded_records)),
                masked_subjects = ifelse((excluded_subjects < minimum_counts & excluded_subjects>0), paste0("<", minimum_counts), as.integer(.data$excluded_subjects))) %>%
  dplyr::select(-c("excluded_records", "excluded_subjects"))

AttritionReportFrac <- AttritionReportFrac %>%
  dplyr::mutate(masked_records = ifelse((records_excluded < minimum_counts & records_excluded>0), paste0("<", minimum_counts), as.integer(.data$records_excluded)),
                masked_subjects = ifelse((subjects_excluded < minimum_counts & subjects_excluded>0), paste0("<", minimum_counts), as.integer(.data$subjects_excluded))) %>%
  dplyr::select(-c("records_excluded", "subjects_excluded"))

fracture_table_aurum_counts <- 
  fracture_table_aurum %>%
  dplyr::mutate(fracture_site2 = case_when(fracture_site == "Vertebra" ~ "Vertebra",
                                           fracture_site == "Hip" ~ "Hip",
                                       !(fracture_site %in% c("Vertebra", "Hip")) ~ "Non-hip, non-vertebra"
  )) %>%
  dplyr::group_by(fracture_site2) %>%
  dplyr::tally()

write.xlsx(AttritionReportFrac, file = here::here(output_folder, "AttritionReport2AURUM.xlsx"))

############################################################################################
### cohort with all records of fracture 
info(logger, "COLLECTING ALL RECORDS OF FRACTURES FROM DENOMINATORS")
cdm[["fracture_hes"]] <- cdm[["denominator"]] %>% 
  left_join(cdm[["condition_occurrence_hes"]], by = c("subject_id" = "person_id")) %>%
  filter(condition_concept_id %in% any_fracture_id) %>%
  select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date) %>%
  mutate(cohort_start_date = as.Date(as.character(cohort_start_date))) %>%
  mutate(cohort_end_date = as.Date(as.character(cohort_end_date))) %>%
  mutate(fracture_site = if (condition_concept_id %in% hip_fracture_id) {
    "Hip"
  }
  else if (condition_concept_id %in% femur_fracture_id) {
    "Femur"
  }
  else if (condition_concept_id %in% foot_fracture_id) {
    "Foot"
  }
  else if (condition_concept_id %in% tib_fracture_id) {
    "Tibia and Fibula"
  }
  else if (condition_concept_id %in% rib_fracture_id) {
    "Rib"
  }
  else if (condition_concept_id %in% forearm_fracture_id) {
    "Forearm"
  }
  else if (condition_concept_id %in% vert_fracture_id) {
    "Vertebra"
  }
  else if (condition_concept_id %in% pelvic_fracture_id) {
    "Pelvic"
  }  
  else if (condition_concept_id %in% humerus_fracture_id) {
    "Humerus"
  } 
  else if (condition_concept_id %in% nonspecific_fracture_id) {
    "Nonspecific"
  }
  )
fracture_table_hes <- cdm[["fracture_hes"]] %>% collect()

AttritionReportFrac<-tibble(
  cohort_definition_id = as.integer(1),
  number_records = fracture_table_hes %>% tally() %>% pull(),
  number_subjects = fracture_table_hes %>% distinct(subject_id) %>% tally() %>% pull(),
  reason = "Starting Population - Anyone With Fracture(s)"
)

### Loading exclusion criteria tables
info(logger, "LOADING EXCLUSION CRITERIA TABLES")
cdm[["cancer_hes"]] <- cdm[["denominator"]] %>% 
  left_join(cdm[["condition_occurrence_hes"]], by = c("subject_id" = "person_id")) %>%
  filter(condition_concept_id %in% cancer_codes) %>%
  select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date) %>%
  mutate(cohort_start_date = as.Date(as.character(cohort_start_date))) %>%
  mutate(cohort_end_date = as.Date(as.character(cohort_end_date))) %>%
  rename(cancer_date =condition_start_date) %>%
  compute()

cdm[["mbd_hes"]] <- cdm[["denominator"]] %>% 
  left_join(cdm[["condition_occurrence_hes"]], by = c("subject_id" = "person_id")) %>%
  filter(condition_concept_id %in% mbd_codes) %>%
  select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date) %>%
  mutate(cohort_start_date = as.Date(as.character(cohort_start_date))) %>%
  mutate(cohort_end_date = as.Date(as.character(cohort_end_date))) %>%
  rename(mbd_date =condition_start_date) %>%
  compute()

### Removing cancer records before the birth year
cdm[["cancer_hes"]] <- cdm[["cancer_hes"]] %>%
  left_join(cdm[["person"]], by = c("subject_id" = "person_id"), copy = T) %>%
  mutate(cancer_year = lubridate::year(cancer_date)) %>%
  filter(cancer_year > year_of_birth) %>%
  select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, cancer_date) %>%
  compute()

### Removing bone disease records before the birth year
cdm[["mbd_hes"]] <- cdm[["mbd_hes"]] %>%
  left_join(cdm[["person"]], by = c("subject_id" = "person_id"), copy = T) %>%
  mutate(mbd_year = lubridate::year(mbd_date)) %>%
  filter(mbd_year > year_of_birth) %>%
  select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, mbd_date) %>%
  compute()

### Removing fractures before the birth year
info(logger, "REMOVING FRACTURES BEFORE THE BIRTH YEAR")
fracture_table_hes <- fracture_table_hes %>%
  left_join(cdm[["person"]], by = c("subject_id" = "person_id"), copy = T) %>%
  mutate(fracture_year = as.numeric(format(condition_start_date, "%Y"))) %>%
  filter(fracture_year > year_of_birth) %>%
  select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date, fracture_site)

AttritionReportFrac<- AttritionReportFrac %>% 
  union_all(  
    tibble(
      cohort_definition_id = as.integer(1),
      number_records = fracture_table_hes %>% tally() %>% pull(),
      number_subjects = fracture_table_hes %>% distinct(subject_id) %>% tally() %>% pull(),
      reason = "Excluding fracture happening before the birth year"
    )
  ) 

### Removing the fractures that happen on the same day as a trauma 
info(logger, "REMOVING FRACTURES THAT HAPPEN ON THE SAME DAY AS A TRAUMA")
fracture_table_hes <- fracture_table_hes %>%
  anti_join(cdm[["condition_occurrence_hes"]] %>% filter(condition_concept_id %in% trauma_condition), by = c("subject_id" = "person_id", "condition_start_date"), copy = T)

fracture_table_hes <- fracture_table_hes %>% 
  anti_join(cdm[["observation_hes"]] %>% filter(observation_concept_id %in% trauma_observation), by = c("subject_id" = "person_id", "condition_start_date" = "observation_date"), copy = T) 

fracture_table_hes<-fracture_table_hes %>%
  anti_join(
    fracture_table_hes %>% 
      filter(!fracture_site=="Nonspecific") %>% 
      group_by(subject_id, condition_start_date) %>% 
      summarise(number_site = n_distinct(fracture_site), .groups = "drop") %>% 
      filter(number_site>=3),
    by = c("subject_id", "condition_start_date")
  )

AttritionReportFrac<- AttritionReportFrac %>% 
  union_all(  
    tibble(
      cohort_definition_id = as.integer(1),
      number_records = fracture_table_hes %>% tally() %>% pull(),
      number_subjects = fracture_table_hes %>% distinct(subject_id) %>% tally() %>% pull(),
      reason = "fracture happening on the same day as a trauma code"
    )
  ) 

### Washout 
info(logger, "APPLYING WASHOUT PERIOD")
sites <- c("Hip", "Femur", "Pelvic", "Vertebra", "Humerus", "Forearm", "Tibia and Fibula", "Rib", "Foot", "Nonspecific")

fracture_table_hes_back_up <- fracture_table_hes
index_fractures <- tibble()

k<-2
while(nrow(fracture_table_hes_back_up)>0){
  fracture_correction <- list()
  for (i in (1:10)){
    fracture_correction[[sites[i]]] <- fracture_table_hes_back_up %>% 
      group_by(subject_id) %>% 
      filter(fracture_site == sites[[i]] | fracture_site == sites[[10]]) %>% 
      summarise(min_date = min(condition_start_date, na.rm =T)) %>% 
      mutate(site = sites[[i]])
  }
  
  fracture_correction_nonspecific <- list()
  
  for (i in (1:10)){
    fracture_correction_nonspecific <- rbind(fracture_correction_nonspecific, fracture_correction[[i]])
  }
  
  fracture_table_hes_back_up <- fracture_table_hes_back_up %>% 
    left_join(fracture_correction_nonspecific, by = c("subject_id", "fracture_site" = "site")) %>% # compute min date
    mutate(gap_to_min_date = condition_start_date - min_date) %>%
    filter(gap_to_min_date == 0 | gap_to_min_date > washout_period[[k]]) %>%
    group_by(subject_id, condition_start_date, fracture_site) %>%
    arrange(condition_concept_id, .by_group = TRUE) %>% 
    filter(row_number()==1) %>%
    ungroup()
  
  fracture_table_hes_back_up <- fracture_table_hes_back_up %>%
    left_join(fracture_table_hes_back_up %>% group_by(subject_id) %>% filter (gap_to_min_date == 0) %>% count(), by = "subject_id") %>%
    filter (!((n>1) & (gap_to_min_date==0) & (fracture_site=="Nonspecific")))
  
  index_fractures <- rbind(index_fractures, fracture_table_hes_back_up %>% filter(gap_to_min_date==0) %>% select(-min_date, -gap_to_min_date, -n))
  
  fracture_table_hes_back_up <- fracture_table_hes_back_up %>% 
    filter(!gap_to_min_date==0) %>%
    select(-min_date, -gap_to_min_date, -n)
}

fracture_table_hes <- index_fractures

AttritionReportFrac<- AttritionReportFrac %>% 
  union_all(  
    tibble(
      cohort_definition_id = as.integer(1),
      number_records = fracture_table_hes %>% tally() %>% pull(),
      number_subjects = fracture_table_hes %>% distinct(subject_id) %>% tally() %>% pull(),
      reason = "Clean out using washout period"
    )
  ) 
rm(index_fractures)

# Applying Hierarchy to multiple records on the same day
info(logger, "APPLYING HIERARCHY TO PREVENT MORE THAN ONE RECORD ON THE SAME DAY FOR THE SAME PERSON")

fracture_table_hes$fracture_site<-factor(fracture_table_hes$fracture_site, levels = sites)

fracture_table_hes <- fracture_table_hes %>%
  group_by(subject_id, condition_start_date) %>%
  arrange(fracture_site, .by_group = TRUE) %>% 
  filter(row_number()==1) %>%
  ungroup()

AttritionReportFrac<- AttritionReportFrac %>% 
  union_all(  
    tibble(
      cohort_definition_id = as.integer(1),
      number_records = fracture_table_hes %>% tally() %>% pull(),
      number_subjects = fracture_table_hes %>% distinct(subject_id) %>% tally() %>% pull(),
      reason = "Excluding records on the same day for the same person according to hierarchy"
    )
  )

### Finalise attrition
AttritionReportFrac <- AttritionReportFrac %>% 
  dplyr::mutate(subjects_excluded = -(number_subjects-lag(number_subjects)), records_excluded = -(number_records - lag(number_records)))

AttritionReportDenom <- AttritionReportDenom %>%
  dplyr::mutate(masked_records = ifelse((excluded_records < minimum_counts & excluded_records>0), paste0("<", minimum_counts), as.integer(.data$excluded_records)),
                masked_subjects = ifelse((excluded_subjects < minimum_counts & excluded_subjects>0), paste0("<", minimum_counts), as.integer(.data$excluded_subjects))) %>%
  dplyr::select(-c("excluded_records", "excluded_subjects"))

AttritionReportFrac <- AttritionReportFrac %>%
  dplyr::mutate(masked_records = ifelse((records_excluded < minimum_counts & records_excluded>0), paste0("<", minimum_counts), as.integer(.data$records_excluded)),
                masked_subjects = ifelse((subjects_excluded < minimum_counts & subjects_excluded>0), paste0("<", minimum_counts), as.integer(.data$subjects_excluded))) %>%
  dplyr::select(-c("records_excluded", "subjects_excluded"))

fracture_table_hes_counts <- 
  fracture_table_hes %>%
  dplyr::mutate(fracture_site2 = case_when(fracture_site == "Vertebra" ~ "Vertebra",
                                           fracture_site == "Hip" ~ "Hip",
                                           !(fracture_site %in% c("Vertebra", "Hip")) ~ "Non-hip, non-vertebra"
  )) %>%
  dplyr::group_by(fracture_site2) %>%
  dplyr::tally()

write.xlsx(AttritionReportFrac, file = here::here(output_folder, "AttritionReport2hes.xlsx"))
fracture_table_counts_hes <- 
  fracture_table_hes %>%
  dplyr::mutate(fracture_site2 = case_when(fracture_site == "Vertebra" ~ "Vertebra",
                                           fracture_site == "Hip" ~ "Hip",
                                           !(fracture_site %in% c("Vertebra", "Hip")) ~ "Non-hip, non-vertebra"
  )) %>%
  dplyr::group_by(fracture_site2) %>%
  dplyr::tally() 

#################################################################################
fracture_table_aurum <- fracture_table_aurum %>% 
  dplyr::mutate(fracture_site2 = case_when(fracture_site == "Vertebra" ~ "Vertebra",
                                           fracture_site == "Hip" ~ "Hip",
                                           !(fracture_site %in% c("Vertebra", "Hip")) ~ "Non-hip, non-vertebra"
  ))

fracture_table_hes <- fracture_table_hes %>% 
  dplyr::mutate(fracture_site2 = case_when(fracture_site == "Vertebra" ~ "Vertebra",
                                           fracture_site == "Hip" ~ "Hip",
                                           !(fracture_site %in% c("Vertebra", "Hip")) ~ "Non-hip, non-vertebra"
  ))

vert_fracture_id_aurum <- fracture_table_aurum %>% 
  dplyr::filter(fracture_site2 == "Vertebra") %>% 
  dplyr::select(subject_id) %>% 
  dplyr::distinct()

vert_fracture_id_hes <- fracture_table_hes %>% 
  dplyr::filter(fracture_site2 == "Vertebra") %>% 
  dplyr::select(subject_id) %>% 
  dplyr::distinct()

setdiff(vert_fracture_id_aurum, vert_fracture_id_hes)
setdiff(vert_fracture_id_hes, vert_fracture_id_aurum)
common <- intersect(vert_fracture_id_hes, vert_fracture_id_aurum)

fracture_table_hes %>% 
  dplyr::filter(fracture_site2 == "Vertebra") %>% 
  dplyr::group_by(subject_id) %>% 
  dplyr::count() %>% 
  dplyr::arrange(desc(n))

########## raw
fracture_table_aurum_raw <- cdm[["fracture"]] %>% collect()
fracture_table_hes_raw <- cdm[["fracture_hes"]] %>% collect()
