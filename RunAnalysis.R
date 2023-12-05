# cdm[["condition_occurrence_primary_care"]] <- cdm[["condition_occurrence"]] %>% 
#   dplyr::filter(condition_type_concept_id == 32827) %>% 
#   CDMConnector::computeQuery()

for (k in (1:length(washout_period))){
  sub_output_folder <- here(output_folder, washout_period[[k]])
  if (!dir.exists(sub_output_folder)) {
    dir.create(sub_output_folder)
  }
  
  # create logger
  log_file <- paste0(sub_output_folder, "/log.txt")
  logger <- create.logger()
  logfile(logger) <- log_file
  level(logger) <- "INFO"
  
  # running necessary functions
  info(logger, "RUNNING NECESSARY FUNCTIONS")
  source(here("Functions", "functions.R"))
  info(logger, "RUNNING NECESSARY FUNCTIONS IS DONE")
  
  # cleaning fractures
  info(logger, "CLEANING FRACTURES")
  source(here("2_CohortCreation", "CleaningFractures.R"))
  info(logger, "CLEANING FRACTURES IS DONE")
  
  # generating cohorts for Research Question 1 and 2
  info(logger, "GENERATING COHORTS FOR RESEARCH QUESTION 1 AND 2")
  source(here("2_CohortCreation", "CohortRQ1&2.R"))
  info(logger, "GENERATING COHORTS FOR RESEARCH QUESTION 1 AND 2 IS DONE")
  # 
  # # carrying out analyses for Research Question 1
  # info(logger, "CARRYING OUT ANALYSES FOR RESEARCH QUESTION 1")
  # source(here("3_Analysis", "RQ1Characterisation.R"))
  # info(logger, "CARRYING OUT ANALYSES FOR RESEARCH QUESTION 1 IS DONE")
  # 
  # # carrying out analyses for Research Question 2
  # info(logger, "CARRYING OUT ANALYSES FOR RESEARCH QUESTION 2")
  # source(here("3_Analysis", "RQ2IncidenceCalculation.R"))
  # info(logger, "CARRYING OUT ANALYSES FOR RESEARCH QUESTION 2 IS DONE")
  
  # generating cohorts for Research Question 3
  info(logger, "GENERATING COHORTS FOR RESEARCH QUESTION 3")
  source(here("2_CohortCreation", "CohortRQ3.R"))
  info(logger, "GENERATING COHORTS FOR RESEARCH QUESTION 3 IS DONE")

  # carrying out analyses for Research Question 3
  info(logger, "CARRYING OUT ANALYSES FOR RESEARCH QUESTION 3")
  source(here("3_Analysis", "RQ3PSMatching.R"))
  info(logger, "CARRYING OUT ANALYSES FOR RESEARCH QUESTION 3 IS DONE")
}

# delete temp files - be careful
for (k in (1:length(washout_period))){
  sub_output_folder <- here(output_folder, washout_period[[k]])
  file.remove(here(sub_output_folder, "tempData"))
}

# create zip file
info(logger, "ZIPPING RESULTS")
output_folder <- basename(output_folder)
zip(
  zipfile = file.path(paste0(output_folder, "/Results_", db_name, ".zip")),
  files = list.files(output_folder, full.names = TRUE)
)
