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

# get denominator population
cdm[["denominator"]] <- generateDenominatorCohortSet(
  cdm,
  startDate = as.Date("2010-04-01"),
  endDate = as.Date("2018-03-31"),
  ageGroup = list(c(50, 150)),
  sex = "Female",
  daysPriorHistory = 730
)

attritionDenominatorCohort <- attrition(cdm$denominator)

# identify individuals with a previous cancer
cancerId <- exclusionCohortSet %>%
  filter(cohort_name == "Malignant neoplastic disease excluding non-melanoma skin cancer") %>%
  pull("cohort_definition_id")
individualsCancerBefore <- cdm[["denominator"]] %>%
  inner_join(
    cdm[[exclusionCohortTableName]] %>%
      filter(cohort_definition_id == cancerId) %>%
      select("subject_id", "cancer_date" = "cohort_start_date"),
    by = "subject_id"
  ) %>%
  filter(cancer_date < cohort_start_date) %>%
  compute()

# exclude individuals with previous cancer
cdm[["denominator1"]] <- cdm[["denominator"]] %>%
  anti_join(individualsCancerBefore, by = "subject_id") %>%
  compute()

# now you can add a new line to the attritionDenominatorCohort
attritionDenominatorCohort <- attritionDenominatorCohort %>%
  union_all(
    tibble(
      current_n = cdm$denominator1 %>% tally() %>% pull(),
      reason = "Previous history of Malignant neoplastic disease excluding non-melanoma skin cancer",
      cohort_definition_id = as.integer(1),
      step = "Exclusion later"
    ) %>%
      mutate(excluded = attritionDenominatorCohort$current_n[9] - .data$current_n)
  )

# identify individuals with a previous cancer
BoneDiseaseId <- exclusionCohortSet %>%
  filter(cohort_name == "Metabolic bone diseases") %>%
  pull("cohort_definition_id")
individualsBoneDiseaseBefore <- cdm[["denominator1"]] %>%
  inner_join(
    cdm[[exclusionCohortTableName]] %>%
      filter(cohort_definition_id == BoneDiseaseId) %>%
      select("subject_id", "bone_disease_date" = "cohort_start_date"),
    by = "subject_id"
  ) %>%
  filter(bone_disease_date < cohort_start_date) %>%
  compute()

# exclude individuals with previous cancer
cdm[["denominator2"]] <- cdm[["denominator1"]] %>%
  anti_join(individualsBoneDiseaseBefore, by = "subject_id") %>%
  compute()

# now you can add a new line to the attritionDenominatorCohort
attritionDenominatorCohort <- attritionDenominatorCohort %>%
  union_all(
    tibble(
      current_n = cdm$denominator2 %>% tally() %>% pull(),
      reason = "Metabolic bone diseases",
      cohort_definition_id = as.integer(1),
      step = "Exclusion later"
    ) %>%
      mutate(excluded = attritionDenominatorCohort$current_n[10] - .data$current_n)
  )

### Up to this step, it gives a complete count of everyone who is eligible (after exclusion criterion) and the relevant attrition table.

