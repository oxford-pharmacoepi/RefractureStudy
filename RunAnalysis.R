# create logger
log_file <- paste0(output_folder, "/log.txt")
logger <- create.logger()
logfile(logger) <- log_file
level(logger) <- "INFO"

# generate table names
info(logger, "GENERATE TABLE NAMES")
exclusionCohortTableName <- paste0(stem_table, "_study_cohorts")

# instantiate necessary cohorts
info(logger, "INSTANTIATE COHORTS")
exclusionCohortSet <- readCohortSet(
  path = here("1_InstantiateCohorts", "Cohorts")
)

cdm <- generateCohortSet(
  cdm = cdm,
  cohortSet = exclusionCohortSet,
  name = exclusionCohortTableName,
  overwrite = TRUE
)

# # get denominator population
# cdm[["denominator"]] <- generateDenominatorCohortSet(
#   cdm,
#   startDate = as.Date("2010-04-01"), 
#   endDate = as.Date("2018-03-31"),
#   ageGroup = list(c(50, 150)),
#   sex = "Female", 
#   daysPriorHistory = 730
# )
# 
# attritionDenominatorCohort <- attrition(cdm$denominator)
# 
# # identify individuals with a previous cancer
# cancerId <- exclusionCohortSet %>%
#   filter(cohort_name == "Malignant neoplastic disease excluding non-melanoma skin cancer") %>%
#   pull("cohort_definition_id")
# individualsCancerBefore <- cdm[["denominator"]] %>%
#   inner_join(
#     cdm[[exclusionCohortTableName]] %>%
#       filter(cohort_definition_id == cancerId) %>%
#       select("subject_id", "cancer_date" = "cohort_start_date"),
#     by = "subject_id"
#   ) %>%
#   filter(cancer_date < cohort_start_date) %>%
#   compute()
# 
# # exclude individuals with previous cancer
# cdm[["denominator1"]] <- cdm[["denominator"]] %>%
#   anti_join(individualsCancerBefore, by = "subject_id") %>%
#   compute()
# 
# # now you can add a new line to the attritionDenominatorCohort
# attritionDenominatorCohort <- attritionDenominatorCohort %>%
#   union_all(
#     tibble(
#       current_n = cdm$denominator1 %>% tally() %>% pull(),
#       reason = "Previous history of Malignant neoplastic disease excluding non-melanoma skin cancer",
#       cohort_definition_id = as.integer(1),
#       step = "Exclusion later"
#     ) %>%
#       mutate(excluded = attritionDenominatorCohort$current_n[9] - .data$current_n)
#   )
# 
# # identify individuals with a previous cancer
# BoneDiseaseId <- exclusionCohortSet %>%
#   filter(cohort_name == "Metabolic bone diseases") %>%
#   pull("cohort_definition_id")
# individualsBoneDiseaseBefore <- cdm[["denominator1"]] %>%
#   inner_join(
#     cdm[[exclusionCohortTableName]] %>%
#       filter(cohort_definition_id == BoneDiseaseId) %>%
#       select("subject_id", "bone_disease_date" = "cohort_start_date"),
#     by = "subject_id"
#   ) %>%
#   filter(bone_disease_date < cohort_start_date) %>%
#   compute()
# 
# # exclude individuals with previous cancer
# cdm[["denominator2"]] <- cdm[["denominator1"]] %>%
#   anti_join(individualsBoneDiseaseBefore, by = "subject_id") %>%
#   compute()
# 
# # now you can add a new line to the attritionDenominatorCohort
# attritionDenominatorCohort <- attritionDenominatorCohort %>%
#   union_all(
#     tibble(
#       current_n = cdm$denominator2 %>% tally() %>% pull(),
#       reason = "Metabolic bone diseases",
#       cohort_definition_id = as.integer(1),
#       step = "Exclusion later"
#     ) %>%
#       mutate(excluded = attritionDenominatorCohort$current_n[10] - .data$current_n)
#   ) 

### Create a cohort of women only, age above 50 between 2010/04/01 and 2018/03/31
cdm <- generateDenominatorCohortSet(
  cdm,
  name = "denominator",
  cohortDateRange = as.Date(c("2010-04-01", "2018-03-31")),
  ageGroup = list(c(50, 150)),
  sex = "Female")

### Loading fracture codes
conditions_sheet1 <- read_excel("~/R/RefractureStudy/FracturesCandidateCodes/fracture_sites_conditions.xlsx", sheet = 1)
trauma <- read_excel("~/R/RefractureStudy/FracturesCandidateCodes/Trauma Codes.xlsx")
trauma_condition <- trauma %>% filter(Domain == "Condition") %>% select(Id) %>% pull()
trauma_observation <- trauma %>% filter(Domain == "Observation") %>% select(Id) %>% pull()

sites <- c("Hip", "Femur", "Pelvic", "Vertebra", "Humerus", "Forearm", "Tibia and Fibula", "Rib", "Foot", "Nonspecific")

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

### cohort with all records of fracture (fracture must lie between 2008 April 1st and 2020 March 31st)
cdm[["fracture"]] <- cdm[["denominator"]] %>% #temp fix
  left_join(cdm[["condition_occurrence"]], by = c("subject_id" = "person_id")) %>%
  filter(condition_concept_id %in% any_fracture_id) %>%
  select(subject_id, condition_concept_id, condition_start_date) %>%
  filter(condition_start_date >= as.Date("2008-04-01")) %>%
  filter(condition_start_date <= as.Date("2020-03-31")) %>%
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

### Removing the fractures that happen on the same day as a trauma
cdm[["fracture"]] <- cdm[["fracture"]] %>%
  anti_join(cdm[["condition_occurrence"]] %>% filter(condition_concept_id %in% trauma_condition), by = c("subject_id" = "person_id", "condition_start_date"))

cdm[["fracture"]] <- cdm[["fracture"]] %>% 
  anti_join(cdm[["observation"]] %>% filter(observation_concept_id %in% trauma_observation), by = c("subject_id" = "person_id", "condition_start_date" = "observation_date")) 

### Washout and computing index date

fracture_table <- cdm[["fracture"]] %>% collect()
fracture_table_back_up <- fracture_table
  
#Round 1: Removing re-recordings
fracture_table <- fracture_table %>% 
  right_join(fracture_table %>% group_by(subject_id, fracture_site) %>% summarise(index_date = min(condition_start_date, na.rm =T)), by = c("subject_id", "fracture_site")) %>%
  mutate(gap_to_index = condition_start_date - index_date) %>% 
  filter(gap_to_index == 0 | gap_to_index > washout_period) %>%
  group_by(subject_id, condition_start_date, fracture_site, gap_to_index) %>% 
  arrange(condition_concept_id, .by_group = TRUE) %>% 
  filter(row_number()==1) 

fracture_index_1 <- fracture_table %>% filter(gap_to_index == 0) %>% select(-index_date) %>% ungroup() %>% select(-gap_to_index)

#Round 2: Removing re-recordings  
fracture_table <- fracture_table %>% filter(!gap_to_index == 0) %>% select(-index_date) %>% ungroup() %>% select(-gap_to_index)

fracture_table <- fracture_table %>% 
  right_join(fracture_table %>% group_by(subject_id, fracture_site) %>% summarise(index_date = min(condition_start_date, na.rm =T)), by = c("subject_id", "fracture_site")) %>%
  mutate(gap_to_index = condition_start_date - index_date) %>% 
  filter(gap_to_index == 0 | gap_to_index > washout_period) %>%
  group_by(subject_id, condition_start_date, fracture_site, gap_to_index) %>% 
  arrange(condition_concept_id, .by_group = TRUE) %>% 
  filter(row_number()==1) 

fracture_index_2 <- fracture_table %>% filter(gap_to_index == 0) %>% select(-index_date) %>% ungroup() %>% select(-gap_to_index)

#After removing based on sites
fracture_table <- rbind(fracture_index_1, fracture_index_2)

# Extra care for nonspecific codes
