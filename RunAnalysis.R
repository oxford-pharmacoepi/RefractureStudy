# create logger
log_file <- paste0(output_folder, "/log.txt")
logger <- create.logger()
logfile(logger) <- log_file
level(logger) <- "INFO"

# generate table names
info(logger, "GENERATE TABLE NAMES FOR EXCLUSION CRITERIA")
exclusionCohortTableName <- paste0(stem_table, "_exclusion_cohorts")

# instantiate necessary cohorts
info(logger, "INSTANTIATE COHORTS - EXCLUSION CRITERIA")
exclusionCohortSet <- readCohortSet(
  path = here("1_InstantiateCohorts", "Cohorts")
)

cdm <- generateCohortSet(
  cdm = cdm,
  cohortSet = exclusionCohortSet,
  name = exclusionCohortTableName,
  overwrite = TRUE
)

### Create a cohort of women only, age above 50 between 2010/04/01 and 2018/03/31
info(logger, "CREATING DENOMINATOR - WOMEN WHO ARE ABOVE 50 WITHIN THE STUDY PERIOD")
cdm <- generateDenominatorCohortSet(
  cdm,
  name = "denominator",
  cohortDateRange = as.Date(c("2010-04-01", "2018-03-31")),
  ageGroup = list(c(50, 150)))

AttritionReportDenom<-cohortAttrition(cdm$denominator)

### Loading fracture codes
conditions_sheet1 <- read_excel("~/R/RefractureStudy/FracturesCandidateCodes/fracture_sites_conditions.xlsx", sheet = 1)
trauma <- read_excel("~/R/RefractureStudy/FracturesCandidateCodes/Trauma Codes.xlsx")
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

### cohort with all records of fracture (fracture must lie between 2008 April 1st and 2020 March 31st)
info(logger, "COLLECTING ALL RECORDS OF FRACTURES FROM DENOMINATORS")
cdm[["fracture"]] <- cdm[["denominator"]] %>% 
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

fracture_table <- cdm[["fracture"]] %>% collect()

AttritionReportFrac<-tibble(
  cohort_definition_id = as.integer(1),
  number_records = fracture_table %>% tally() %>% pull(),
  number_subjects = fracture_table %>% distinct(subject_id) %>% tally() %>% pull(),
  reason = "Starting Population"
)

### Removing the fractures that happen on the same day as a trauma
info(logger, "REMOVING FRACTURES THAT HAPPEN ON THE SAME DAY AS A TRAUMA")
fracture_table <- fracture_table %>%
  anti_join(cdm[["condition_occurrence"]] %>% filter(condition_concept_id %in% trauma_condition), by = c("subject_id" = "person_id", "condition_start_date"), copy = T)

fracture_table <- fracture_table %>% 
  anti_join(cdm[["observation"]] %>% filter(observation_concept_id %in% trauma_observation), by = c("subject_id" = "person_id", "condition_start_date" = "observation_date"), copy = T) 

AttritionReportFrac<- AttritionReportFrac %>% 
  union_all(  
    tibble(
    cohort_definition_id = as.integer(1),
    number_records = fracture_table %>% tally() %>% pull(),
    number_subjects = fracture_table %>% distinct(subject_id) %>% tally() %>% pull(),
    reason = "fracture happening on the same day as a trauma code"
  )
  ) 


### Washout and computing index date
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
fracture_table <- fracture_table %>% 
  group_by(subject_id) %>% arrange(condition_start_date, .by_group =T) %>%
  mutate(gap_to_prior_fracture = condition_start_date - lag(condition_start_date), lag_site = lag(fracture_site))

fracture_table <- rbind(fracture_table[is.na(fracture_table$gap_to_prior_fracture),],
                        fracture_table %>%
                          filter (!(fracture_site == "Nonspecific" & gap_to_prior_fracture < washout_period)) %>%
                          filter (!(lag_site == "Nonspecific" & gap_to_prior_fracture < washout_period)))
fracture_table <- fracture_table %>% ungroup() %>% select(-gap_to_prior_fracture, -lag_site)

# Adding index date
fracture_table <- fracture_table %>%
  right_join(fracture_table %>% 
               filter (condition_start_date >= study_start_date) %>%
               filter (condition_start_date <= study_end_date) %>% 
               group_by (subject_id) %>% 
               summarise (index_date = min(condition_start_date, na.rm = T)), by = "subject_id")
  

### Exclusion criteria
# At least 730 days prior obs
fracture_table <-fracture_table %>% 
  left_join(cdm[["observation_period"]], by = c("subject_id" = "person_id"), copy = T) %>% 
  select(subject_id:index_date, observation_period_start_date, observation_period_end_date) %>%
  mutate(days_prior_obs = index_date - observation_period_start_date) %>%
  filter(days_prior_obs >= prior_observation) %>%
  group_by(subject_id, condition_concept_id, condition_start_date, fracture_site, index_date) %>%
  arrange(observation_period_start_date) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  select(subject_id, condition_concept_id, condition_start_date, fracture_site, index_date)

# No records of death on the index date

# No records of cancer before the index date

# No records of metabolic bone disease