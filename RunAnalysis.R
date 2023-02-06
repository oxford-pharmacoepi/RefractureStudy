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
  cohortTableName = exclusionCohortTableName,
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
  filter(cohortName == "Malignant neoplastic disease excluding non-melanoma skin cancer") %>%
  pull("cohortId")
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
cdm[["denominator"]] <- cdm[["denominator"]] %>%
  anti_join(individualsCancerBefore, by = "subject_id") %>%
  compute()

# now you can add a new line to the attritionDenominatorCohort
# ...

