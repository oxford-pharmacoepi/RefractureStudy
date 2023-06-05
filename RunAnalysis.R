# create logger
log_file <- paste0(output_folder, "/log.txt")
logger <- create.logger()
logfile(logger) <- log_file
level(logger) <- "INFO"

# generating cohorts for Research Question 1 and 2
info(logger, "GENERATING COHORTS FOR RESEARCH QUESTION 1 AND 2")
source(here("2_CohortCreation", "CohortRQ1&2.R"))
info(logger, "GENERATING COHORTS FOR RESEARCH QUESTION 1 AND 2 IS DONE")