print(paste0("Creating condition occurrence hospital only at ", Sys.time()))
cdm[["condition_occurrence_hospital"]] <- 
  cdm[["condition_occurrence"]] %>% 
  dplyr::filter(condition_type_concept_id == 32829) %>% 
  dplyr::compute()

info(logger, "CREATING DENOMINATOR - WOMEN WHO ARE ABOVE 50 WITHIN THE STUDY PERIOD")
print(paste0("Creating denominators at ", Sys.time()))
cdm <- generateDenominatorCohortSet(
  cdm,
  name = "denominator",
  cohortDateRange = c(study_start_date, study_end_date),
  sex = "Female",
  ageGroup = list(c(50, 150)))

denom_count <-cdm[["denominator"]] %>% dplyr::tally() %>% dplyr::pull()

AttritionReportDenom<-cohortAttrition(cdm$denominator)

### Loading fracture codes
info(logger, "LOADING FRACTURE CODES")
print(paste0("Loading fracture codes at ", Sys.time()))
conditions_sheet1 <- read_excel(paste0(here(), "/1_InstantiateCohorts/fracture_sites_conditions_codes.xlsx"), sheet = 1) 
trauma <- read_excel(paste0(here(), "/1_InstantiateCohorts/trauma_codes.xlsx")) 
trauma_condition <- trauma %>% dplyr::filter(Domain == "Condition") %>% dplyr::select(Id) %>% dplyr::pull()
trauma_observation <- trauma %>% dplyr::filter(Domain == "Observation") %>% dplyr::select(Id) %>% dplyr::pull()

hip_fracture_id <- conditions_sheet1 %>% dplyr::filter(Site == "Hip") %>% dplyr::select(Id) %>% dplyr::pull()
femur_fracture_id <- conditions_sheet1 %>% dplyr::filter(Site == "Femur") %>% dplyr::select(Id) %>% dplyr::pull()
pelvic_fracture_id <- conditions_sheet1 %>% dplyr::filter(Site == "Pelvic") %>% dplyr::select(Id) %>% dplyr::pull()
vert_fracture_id <- conditions_sheet1 %>% dplyr::filter(Site == "Vertebra") %>% dplyr::select(Id) %>% dplyr::pull()
humerus_fracture_id <- conditions_sheet1 %>% dplyr::filter(Site == "Humerus") %>% dplyr::select(Id) %>% dplyr::pull()
forearm_fracture_id <- conditions_sheet1 %>% dplyr::filter(Site == "Forearm") %>% dplyr::select(Id) %>% dplyr::pull()
tib_fracture_id <- conditions_sheet1 %>% dplyr::filter(Site == "Tibia and Fibula") %>% dplyr::select(Id) %>% dplyr::pull()
rib_fracture_id <- conditions_sheet1 %>% dplyr::filter(Site == "Rib") %>% dplyr::select(Id) %>% dplyr::pull()
foot_fracture_id <- conditions_sheet1 %>% dplyr::filter(Site == "Foot") %>% dplyr::select(Id) %>% dplyr::pull()
nonspecific_fracture_id <- conditions_sheet1 %>% dplyr::filter(Site == "Nonspecific") %>% dplyr::select(Id) %>% dplyr::pull()

any_fracture_id <- conditions_sheet1 %>% dplyr::filter(!Site == "Exclude") %>% dplyr::select(Id) %>% dplyr::pull()


### cohort with all records of fracture 
info(logger, "COLLECTING ALL RECORDS OF FRACTURES FROM DENOMINATORS")
print(paste0("Collecting all records of fractures from the denominators at ", Sys.time()))
cdm[["fracture"]] <- cdm[["denominator"]] %>% 
  dplyr::left_join(cdm[["condition_occurrence_hospital"]], by = c("subject_id" = "person_id")) %>%
  dplyr::filter(condition_concept_id %in% any_fracture_id) %>%
  dplyr::select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date) %>%
  dplyr::mutate(cohort_start_date = as.Date(as.character(cohort_start_date))) %>%
  dplyr::mutate(cohort_end_date = as.Date(as.character(cohort_end_date))) %>%
  dplyr::mutate(fracture_site = if (condition_concept_id %in% hip_fracture_id) {
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
rm(hip_fracture_id, femur_fracture_id, foot_fracture_id, tib_fracture_id, rib_fracture_id, forearm_fracture_id, vert_fracture_id, pelvic_fracture_id, humerus_fracture_id, nonspecific_fracture_id)
fracture_table <- cdm[["fracture"]] %>% dplyr::collect()

AttritionReportFrac<-tibble(
  cohort_definition_id = as.integer(1),
  number_records = fracture_table %>% dplyr::tally() %>% dplyr::pull(),
  number_subjects = fracture_table %>% distinct(subject_id) %>% dplyr::tally() %>% dplyr::pull(),
  reason = "Starting Population - Anyone With Fracture(s)"
)

### Loading exclusion criteria tables
info(logger, "LOADING EXCLUSION CRITERIA TABLES")
print(paste0("Creating cancer and mbd patients at ", Sys.time()))
cancer <- read_excel(paste0(here(), "/1_InstantiateCohorts/cancer_codes.xlsx"))
mbd <- read_excel(paste0(here(), "/1_InstantiateCohorts/mbd_codes.xlsx"))
cancer_codes <- cancer %>% dplyr::select(Id) %>% dplyr::pull()
mbd_codes <- mbd %>% dplyr::select(Id) %>% dplyr::pull()

cdm[["cancer"]] <- cdm[["denominator"]] %>% 
  dplyr::left_join(cdm[["condition_occurrence"]], by = c("subject_id" = "person_id")) %>%
  dplyr::filter(condition_concept_id %in% cancer_codes) %>%
  dplyr::select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date) %>%
  dplyr::mutate(cohort_start_date = as.Date(as.character(cohort_start_date))) %>%
  dplyr::mutate(cohort_end_date = as.Date(as.character(cohort_end_date))) %>%
  dplyr::rename(cancer_date =condition_start_date) %>%
  CDMConnector::computeQuery()

cdm[["mbd"]] <- cdm[["denominator"]] %>% 
  dplyr::left_join(cdm[["condition_occurrence"]], by = c("subject_id" = "person_id")) %>%
  dplyr::filter(condition_concept_id %in% mbd_codes) %>%
  dplyr::select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date) %>%
  dplyr::mutate(cohort_start_date = as.Date(as.character(cohort_start_date))) %>%
  dplyr::mutate(cohort_end_date = as.Date(as.character(cohort_end_date))) %>%
  dplyr::rename(mbd_date =condition_start_date) %>%
  CDMConnector::computeQuery()

### Removing cancer records before the birth year
cdm[["cancer"]] <- cdm[["cancer"]] %>%
  dplyr::left_join(cdm[["person"]], by = c("subject_id" = "person_id"), copy = T) %>%
  dplyr::mutate(cancer_year = lubridate::year(cancer_date)) %>%
  dplyr::filter(cancer_year > year_of_birth) %>%
  dplyr::select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, cancer_date) %>%
  CDMConnector::computeQuery()

### Removing bone disease records before the birth year
cdm[["mbd"]] <- cdm[["mbd"]] %>%
  dplyr::left_join(cdm[["person"]], by = c("subject_id" = "person_id"), copy = T) %>%
  dplyr::mutate(mbd_year = lubridate::year(mbd_date)) %>%
  dplyr::filter(mbd_year > year_of_birth) %>%
  dplyr::select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, mbd_date) %>%
  CDMConnector::computeQuery()

### Removing fractures before the birth year
info(logger, "REMOVING FRACTURES BEFORE THE BIRTH YEAR")
print(paste0("Removing fractures before the birth year at ", Sys.time()))
fracture_table <- fracture_table %>%
  dplyr::left_join(cdm[["person"]], by = c("subject_id" = "person_id"), copy = T) %>%
  dplyr::mutate(fracture_year = as.numeric(format(condition_start_date, "%Y"))) %>%
  dplyr::filter(fracture_year > year_of_birth) %>%
  dplyr::select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date, fracture_site)

AttritionReportFrac<- AttritionReportFrac %>% 
  union_all(  
    tibble(
      cohort_definition_id = as.integer(1),
      number_records = fracture_table %>% dplyr::tally() %>% dplyr::pull(),
      number_subjects = fracture_table %>% distinct(subject_id) %>% dplyr::tally() %>% dplyr::pull(),
      reason = "Excluding fracture happening before the birth year"
    )
  ) 

### Removing the fractures that happen on the same day as a trauma 
info(logger, "REMOVING FRACTURES THAT HAPPEN ON THE SAME DAY AS A TRAUMA")
print(paste0("Removing fractures based on trauma codes at ", Sys.time()))
fracture_table <- fracture_table %>%
  anti_join(cdm[["condition_occurrence"]] %>% dplyr::filter(condition_concept_id %in% trauma_condition), by = c("subject_id" = "person_id", "condition_start_date"), copy = T)

fracture_table <- fracture_table %>% 
  anti_join(cdm[["observation"]] %>% dplyr::filter(observation_concept_id %in% trauma_observation), by = c("subject_id" = "person_id", "condition_start_date" = "observation_date"), copy = T) 

fracture_table<-fracture_table %>%
  anti_join(
    fracture_table %>% 
      dplyr::filter(!fracture_site=="Nonspecific") %>% 
      dplyr::group_by(subject_id, condition_start_date) %>% 
      dplyr::summarise(number_site = n_distinct(fracture_site), .groups = "drop") %>% 
      dplyr::filter(number_site>=3),
    by = c("subject_id", "condition_start_date")
  )

AttritionReportFrac<- AttritionReportFrac %>% 
  union_all(  
    tibble(
      cohort_definition_id = as.integer(1),
      number_records = fracture_table %>% dplyr::tally() %>% dplyr::pull(),
      number_subjects = fracture_table %>% distinct(subject_id) %>% dplyr::tally() %>% dplyr::pull(),
      reason = "fracture happening on the same day as a trauma code"
    )
  ) 

### Washout 
info(logger, "APPLYING WASHOUT PERIOD")
print(paste0("Applying a washout period of ", washout_period[[k]], " at ", Sys.time()))
sites <- c("Hip", "Femur", "Pelvic", "Vertebra", "Humerus", "Forearm", "Tibia and Fibula", "Rib", "Foot", "Nonspecific")

fracture_table_back_up <- fracture_table
index_fractures <- tibble()

suppressWarnings(
  while(nrow(fracture_table_back_up)>0){
    fracture_correction <- list()
    for (i in (1:10)){
      fracture_correction[[sites[i]]] <- fracture_table_back_up %>% 
        dplyr::group_by(subject_id) %>% 
        dplyr::filter(fracture_site == sites[[i]] | fracture_site == sites[[10]]) %>% 
        dplyr::summarise(min_date = min(condition_start_date, na.rm =T)) %>% 
        dplyr::mutate(site = sites[[i]])
    }
    
    fracture_correction_nonspecific <- list()
    
    for (i in (1:10)){
      fracture_correction_nonspecific <- rbind(fracture_correction_nonspecific, fracture_correction[[i]])
    }
    
    fracture_table_back_up <- fracture_table_back_up %>% 
      dplyr::left_join(fracture_correction_nonspecific, by = c("subject_id", "fracture_site" = "site")) %>% # compute min date
      dplyr::mutate(gap_to_min_date = condition_start_date - min_date) %>%
      dplyr::filter(gap_to_min_date == 0 | gap_to_min_date > washout_period[[k]]) %>%
      dplyr::group_by(subject_id, condition_start_date, fracture_site) %>%
      dplyr::arrange(condition_concept_id, .by_group = TRUE) %>% 
      dplyr::filter(row_number()==1) %>%
      dplyr::ungroup()
    
    fracture_table_back_up <- fracture_table_back_up %>%
      dplyr::left_join(fracture_table_back_up %>% dplyr::group_by(subject_id) %>% dplyr::filter (gap_to_min_date == 0) %>% dplyr::count(), by = "subject_id") %>%
      dplyr::filter (!((n>1) & (gap_to_min_date==0) & (fracture_site=="Nonspecific")))
    
    index_fractures <- rbind(index_fractures, fracture_table_back_up %>% dplyr::filter(gap_to_min_date==0) %>% dplyr::select(-min_date, -gap_to_min_date, -n))
    
    fracture_table_back_up <- fracture_table_back_up %>% 
      dplyr::filter(!gap_to_min_date==0) %>%
      dplyr::select(-min_date, -gap_to_min_date, -n)
  }
)

fracture_table <- index_fractures

AttritionReportFrac<- AttritionReportFrac %>% 
  union_all(  
    tibble(
      cohort_definition_id = as.integer(1),
      number_records = fracture_table %>% dplyr::tally() %>% dplyr::pull(),
      number_subjects = fracture_table %>% distinct(subject_id) %>% dplyr::tally() %>% dplyr::pull(),
      reason = "Clean out using washout period"
    )
  ) 
rm(index_fractures)

# Applying Hierarchy to multiple records on the same day
info(logger, "APPLYING HIERARCHY TO PREVENT MORE THAN ONE RECORD ON THE SAME DAY FOR THE SAME PERSON")
print(paste0("Applying hierarchy at ", Sys.time()))
fracture_table$fracture_site<-factor(fracture_table$fracture_site, levels = sites)

fracture_table <- fracture_table %>%
  dplyr::group_by(subject_id, condition_start_date) %>%
  dplyr::arrange(fracture_site, .by_group = TRUE) %>% 
  dplyr::filter(row_number()==1) %>%
  dplyr::ungroup()

AttritionReportFrac<- AttritionReportFrac %>% 
  union_all(  
    tibble(
      cohort_definition_id = as.integer(1),
      number_records = fracture_table %>% dplyr::tally() %>% dplyr::pull(),
      number_subjects = fracture_table %>% distinct(subject_id) %>% dplyr::tally() %>% dplyr::pull(),
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

# write.xlsx(AttritionReportDenom, file = here::here(sub_output_folder, "AttritionReport1.xlsx"))
# write.xlsx(AttritionReportFrac, file = here::here(sub_output_folder, "AttritionReport2.xlsx"))

rm(cancer, mbd, trauma, conditions_sheet1)

sub_output_folder <- here(output_folder, washout_period[[k]], "secondary_rq2")
if (!dir.exists(sub_output_folder)) {
  dir.create(sub_output_folder)
}

info(logger, "GENERATING COHORTS FOR RESEARCH QUESTION 1 AND 2")
print(paste0("Generating cohorts for RQ1+2 at ", Sys.time()))
source(here("2_CohortCreation", "CohortRQ1&2.R"))
info(logger, "GENERATING COHORTS FOR RESEARCH QUESTION 1 AND 2 IS DONE")
print(paste0("Generating cohorts for RQ1+2 is done at ", Sys.time()))
# 
# carrying out analyses for Research Question 1
info(logger, "CARRYING OUT ANALYSES FOR RESEARCH QUESTION 1")
source(here("3_Analysis", "RQ1Characterisation.R"))
info(logger, "CARRYING OUT ANALYSES FOR RESEARCH QUESTION 1 IS DONE")

# carrying out analyses for Research Question 2
info(logger, "CARRYING OUT ANALYSES FOR RESEARCH QUESTION 2")
source(here("3_Analysis", "RQ2IncidenceCalculation.R"))
info(logger, "CARRYING OUT ANALYSES FOR RESEARCH QUESTION 2 IS DONE")