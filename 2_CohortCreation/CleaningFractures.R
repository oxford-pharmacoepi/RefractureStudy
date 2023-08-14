### Create a cohort of women only, age above 50 between 2010/04/01 and 2018/03/31
info(logger, "CREATING DENOMINATOR - WOMEN WHO ARE ABOVE 50 WITHIN THE STUDY PERIOD")
cdm <- generateDenominatorCohortSet(
  cdm,
  name = "denominator",
  cohortDateRange = c(study_start_date, study_end_date),
  sex = "Female",
  ageGroup = list(c(50, 150)))

denom_count <-cdm[["denominator"]] %>% tally() %>% pull()

AttritionReportDenom<-cohortAttrition(cdm$denominator)

### Loading fracture codes
info(logger, "LOADING FRACTURE CODES")
conditions_sheet1 <- read_excel(paste0(here(), "/1_InstantiateCohorts/fracture_sites_conditions_codes.xlsx"), sheet = 1) 
trauma <- read_excel(paste0(here(), "/1_InstantiateCohorts/trauma_codes.xlsx")) 
trauma_condition <- trauma %>% filter(Domain == "Condition") %>% select(Id) %>% pull()
trauma_observation <- trauma %>% filter(Domain == "Observation") %>% select(Id) %>% pull()

hip_fracture_id <- conditions_sheet1 %>% filter(Site == "Hip") %>% select(Id) %>% pull()
femur_fracture_id <- conditions_sheet1 %>% filter(Site == "Femur") %>% select(Id) %>% pull()
pelvic_fracture_id <- conditions_sheet1 %>% filter(Site == "Pelvic") %>% select(Id) %>% pull()
vert_fracture_id <- conditions_sheet1 %>% filter(Site == "Vertebra") %>% select(Id) %>% pull()
humerus_fracture_id <- conditions_sheet1 %>% filter(Site == "Humerus") %>% select(Id) %>% pull()
forearm_fracture_id <- conditions_sheet1 %>% filter(Site == "Forearm") %>% select(Id) %>% pull()
tib_fracture_id <- conditions_sheet1 %>% filter(Site == "Tibia and Fibula") %>% select(Id) %>% pull()
rib_fracture_id <- conditions_sheet1 %>% filter(Site == "Rib") %>% select(Id) %>% pull()
foot_fracture_id <- conditions_sheet1 %>% filter(Site == "Foot") %>% select(Id) %>% pull()
nonspecific_fracture_id <- conditions_sheet1 %>% filter(Site == "Nonspecific") %>% select(Id) %>% pull()

any_fracture_id <- conditions_sheet1 %>% filter(!Site == "Exclude") %>% select(Id) %>% pull()

### cohort with all records of fracture 
info(logger, "COLLECTING ALL RECORDS OF FRACTURES FROM DENOMINATORS")
cdm[["fracture"]] <- cdm[["denominator"]] %>% 
  left_join(cdm[["condition_occurrence"]], by = c("subject_id" = "person_id")) %>%
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

fracture_table <- cdm[["fracture"]] %>% collect()

AttritionReportFrac<-tibble(
  cohort_definition_id = as.integer(1),
  number_records = fracture_table %>% tally() %>% pull(),
  number_subjects = fracture_table %>% distinct(subject_id) %>% tally() %>% pull(),
  reason = "Starting Population"
)

### Loading exclusion criteria tables
info(logger, "LOADING EXCLUSION CRITERIA TABLES")
cancer <- read_excel(paste0(here(), "/1_InstantiateCohorts/cancer_codes.xlsx"))
mbd <- read_excel(paste0(here(), "/1_InstantiateCohorts/mbd_codes.xlsx"))
cancer_codes <- cancer %>% select(Id) %>% pull()
mbd_codes <- mbd %>% select(Id) %>% pull()

cdm[["cancer"]] <- cdm[["denominator"]] %>% 
  left_join(cdm[["condition_occurrence"]], by = c("subject_id" = "person_id")) %>%
  filter(condition_concept_id %in% cancer_codes) %>%
  select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date) %>%
  mutate(cohort_start_date = as.Date(as.character(cohort_start_date))) %>%
  mutate(cohort_end_date = as.Date(as.character(cohort_end_date))) %>%
  rename(cancer_date =condition_start_date) %>%
  compute()

cdm[["mbd"]] <- cdm[["denominator"]] %>% 
  left_join(cdm[["condition_occurrence"]], by = c("subject_id" = "person_id")) %>%
  filter(condition_concept_id %in% mbd_codes) %>%
  select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date) %>%
  mutate(cohort_start_date = as.Date(as.character(cohort_start_date))) %>%
  mutate(cohort_end_date = as.Date(as.character(cohort_end_date))) %>%
  rename(mbd_date =condition_start_date) %>%
  compute()

### Removing cancer records before the birth year
cdm[["cancer"]] <- cdm[["cancer"]] %>%
  left_join(cdm[["person"]], by = c("subject_id" = "person_id"), copy = T) %>%
  mutate(cancer_year = lubridate::year(cancer_date)) %>%
  filter(cancer_year >= year_of_birth) %>%
  select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, cancer_date) %>%
  compute()

### Removing bone disease records before the birth year
cdm[["mbd"]] <- cdm[["mbd"]] %>%
  left_join(cdm[["person"]], by = c("subject_id" = "person_id"), copy = T) %>%
  mutate(mbd_year = lubridate::year(mbd_date)) %>%
  filter(mbd_year >= year_of_birth) %>%
  select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, mbd_date) %>%
  compute()

### Removing fractures before the birth year
info(logger, "REMOVING FRACTURES BEFORE THE BIRTH YEAR")
fracture_table <- fracture_table %>%
  left_join(cdm[["person"]], by = c("subject_id" = "person_id"), copy = T) %>%
  mutate(fracture_year = as.numeric(format(condition_start_date, "%Y"))) %>%
  filter(fracture_year >= year_of_birth) %>%
  select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date, fracture_site)

AttritionReportFrac<- AttritionReportFrac %>% 
  union_all(  
    tibble(
      cohort_definition_id = as.integer(1),
      number_records = fracture_table %>% tally() %>% pull(),
      number_subjects = fracture_table %>% distinct(subject_id) %>% tally() %>% pull(),
      reason = "Excluding fracture happening before the birth year"
    )
  ) 

### Removing the fractures that happen on the same day as a trauma 
info(logger, "REMOVING FRACTURES THAT HAPPEN ON THE SAME DAY AS A TRAUMA")
fracture_table <- fracture_table %>%
  anti_join(cdm[["condition_occurrence"]] %>% filter(condition_concept_id %in% trauma_condition), by = c("subject_id" = "person_id", "condition_start_date"), copy = T)

fracture_table <- fracture_table %>% 
  anti_join(cdm[["observation"]] %>% filter(observation_concept_id %in% trauma_observation), by = c("subject_id" = "person_id", "condition_start_date" = "observation_date"), copy = T) 

fracture_table<-fracture_table %>%
  anti_join(
    fracture_table %>% filter(!fracture_site=="Nonspecific") %>% group_by(subject_id, condition_start_date) %>% summarise(number_site = n_distinct(fracture_site)) %>% filter(number_site>=3),
    by = c("subject_id", "condition_start_date")
  )

AttritionReportFrac<- AttritionReportFrac %>% 
  union_all(  
    tibble(
      cohort_definition_id = as.integer(1),
      number_records = fracture_table %>% tally() %>% pull(),
      number_subjects = fracture_table %>% distinct(subject_id) %>% tally() %>% pull(),
      reason = "fracture happening on the same day as a trauma code"
    )
  ) 

### Washout 
info(logger, "APPLYING WASHOUT PERIOD")
sites <- c("Hip", "Femur", "Pelvic", "Vertebra", "Humerus", "Forearm", "Tibia and Fibula", "Rib", "Foot", "Nonspecific")

fracture_table_back_up <- fracture_table
index_fractures <- tibble()

while(nrow(fracture_table_back_up)>0){
  fracture_correction <- list()
  for (i in (1:10)){
    fracture_correction[[sites[i]]] <- fracture_table_back_up %>% 
      group_by(subject_id) %>% 
      filter(fracture_site == sites[[i]] | fracture_site == sites[[10]]) %>% 
      summarise(min_date = min(condition_start_date, na.rm =T)) %>% 
      mutate(site = sites[[i]])
  }
  
  fracture_correction_nonspecific <- list()
  
  for (i in (1:10)){
    fracture_correction_nonspecific <- rbind(fracture_correction_nonspecific, fracture_correction[[i]])
  }
  
  fracture_table_back_up <- fracture_table_back_up %>% 
    left_join(fracture_correction_nonspecific, by = c("subject_id", "fracture_site" = "site")) %>% # compute min date
    mutate(gap_to_min_date = condition_start_date - min_date) %>%
    filter(gap_to_min_date == 0 | gap_to_min_date > washout_period) %>%
    group_by(subject_id, condition_start_date, fracture_site) %>%
    arrange(condition_concept_id, .by_group = TRUE) %>% 
    filter(row_number()==1) %>%
    ungroup()
  
  fracture_table_back_up <- fracture_table_back_up %>%
    left_join(fracture_table_back_up %>% group_by(subject_id) %>% filter (gap_to_min_date == 0) %>% count(), by = "subject_id") %>%
    filter (!((n>1) & (gap_to_min_date==0) & (fracture_site=="Nonspecific")))
  
  index_fractures <- rbind(index_fractures, fracture_table_back_up %>% filter(gap_to_min_date==0) %>% select(-min_date, -gap_to_min_date, -n))
  
  fracture_table_back_up <- fracture_table_back_up %>% 
    filter(!gap_to_min_date==0) %>%
    select(-min_date, -gap_to_min_date, -n)
}

fracture_table <- index_fractures

AttritionReportFrac<- AttritionReportFrac %>% 
  union_all(  
    tibble(
      cohort_definition_id = as.integer(1),
      number_records = fracture_table %>% tally() %>% pull(),
      number_subjects = fracture_table %>% distinct(subject_id) %>% tally() %>% pull(),
      reason = "Clean out using washout period"
    )
  ) 

# Applying Hierarchy to multiple records on the same day
info(logger, "APPLYING HIERARCHY TO PREVENT MORE THAN ONE RECORD ON THE SAME DAY FOR THE SAME PERSON")

fracture_table$fracture_site<-factor(fracture_table$fracture_site, levels = sites)

fracture_table <- fracture_table %>%
  group_by(subject_id, condition_start_date) %>%
  arrange(fracture_site, .by_group = TRUE) %>% 
  filter(row_number()==1) %>%
  ungroup()

AttritionReportFrac<- AttritionReportFrac %>% 
  union_all(  
    tibble(
      cohort_definition_id = as.integer(1),
      number_records = fracture_table %>% tally() %>% pull(),
      number_subjects = fracture_table %>% distinct(subject_id) %>% tally() %>% pull(),
      reason = "Excluding records on the same day for the same person according to hierarchy"
    )
  )

### Finalise attrition
AttritionReportFrac <- AttritionReportFrac %>% 
  mutate(subjects_excluded = -(number_subjects-lag(number_subjects)), records_excluded = -(number_records - lag(number_records)))

write.xlsx(AttritionReportDenom, file = here::here(output_folder, "AttritionReport1.xlsx"))
write.xlsx(AttritionReportFrac, file = here::here(output_folder, "AttritionReport2.xlsx"))
