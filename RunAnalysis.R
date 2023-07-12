# create logger
log_file <- paste0(output_folder, "/log.txt")
logger <- create.logger()
logfile(logger) <- log_file
level(logger) <- "INFO"

# running necessary functions
info(logger, "RUNNING NECESSARY FUNCTIONS")
source(here("Functions", "functions.R"))
info(logger, "RUNNING NECESSARY FUNCTIONS IS DONE")

# generating cohorts for Research Question 1 and 2
info(logger, "GENERATING COHORTS FOR RESEARCH QUESTION 1 AND 2")
source(here("2_CohortCreation", "CohortRQ1&2.R"))
info(logger, "GENERATING COHORTS FOR RESEARCH QUESTION 1 AND 2 IS DONE")

# carrying out analyses for Research Question 1 
info(logger, "CARRYING OUT ANALYSES FOR RESEARCH QUESTION 1")
source(here("3_Analysis", "RQ1Characterisation.R"))
info(logger, "CARRYING OUT ANALYSES FOR RESEARCH QUESTION 1 IS DONE")

# carrying out analyses for Research Question 2
info(logger, "CARRYING OUT ANALYSES FOR RESEARCH QUESTION 2")
source(here("3_Analysis", "RQ2IncidenceCalculation.R"))
info(logger, "CARRYING OUT ANALYSES FOR RESEARCH QUESTION 2 IS DONE")

# generating cohorts for Research Question 3
info(logger, "GENERATING COHORTS FOR RESEARCH QUESTION 3")
source(here("2_CohortCreation", "CohortRQ3.R"))
info(logger, "GENERATING COHORTS FOR RESEARCH QUESTION 3 IS DONE")

# carrying out analyses for Research Question 3
info(logger, "CARRYING OUT ANALYSES FOR RESEARCH QUESTION 3")
source(here("3_Analysis", "RQ3PropensityScores.R"))
info(logger, "CARRYING OUT ANALYSES FOR RESEARCH QUESTION 3 IS DONE")