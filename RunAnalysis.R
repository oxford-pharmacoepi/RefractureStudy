print(paste0("Starting the analysis at ", Sys.time()))

if (!(country_setting %in% c("UK", "Germany", "France", "Italy", "Spain", "Netherlands"))) {
  stop('country_setting should be one of the following: "UK", "Germany", "France", "Italy", "Spain", "Netherlands"')
}

for (k in (1:length(washout_period))){
  print(paste0("Starting the analysis for washout of ", washout_period[[k]], " at ", Sys.time()))
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
  print(paste0("Running necessary functions at ", Sys.time()))
  source(here("Functions", "functions.R"))
  info(logger, "RUNNING NECESSARY FUNCTIONS IS DONE")
  print(paste0("Running necessary functions is done at ", Sys.time()))
  
  # cleaning fractures
  info(logger, "CLEANING FRACTURES")
  print(paste0("Cleaning fractures at ", Sys.time()))
  source(here("2_CohortCreation", "CleaningFractures.R"))
  info(logger, "CLEANING FRACTURES IS DONE")
  print(paste0("Cleaning fractures is done at ", Sys.time()))
  
  # generating cohorts for Research Question 1 and 2
  info(logger, "GENERATING COHORTS FOR RESEARCH QUESTION 1 AND 2")
  print(paste0("Generating cohorts for RQ1+2 at ", Sys.time()))
  source(here("2_CohortCreation", "CohortRQ1&2.R"))
  info(logger, "GENERATING COHORTS FOR RESEARCH QUESTION 1 AND 2 IS DONE")
  print(paste0("Generating cohorts for RQ1+2 is done at ", Sys.time()))
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
  print(paste0("Generating cohorts for RQ3 at ", Sys.time()))
  source(here("2_CohortCreation", "CohortRQ3.R"))
  info(logger, "GENERATING COHORTS FOR RESEARCH QUESTION 3 IS DONE")
  print(paste0("Generating cohorts for RQ3 is done at ", Sys.time()))

  # carrying out analyses for Research Question 3 - Matching
  info(logger, "CARRYING OUT ANALYSES FOR RESEARCH QUESTION 3 - MATCHING")
  print(paste0("Start matching at ", Sys.time()))
  source(here("3_Analysis", "RQ3PSMatching.R"))
  info(logger, "CARRYING OUT ANALYSES FOR RESEARCH QUESTION 3 - MATCHING IS DONE")
  print(paste0("Finish matching at ", Sys.time()))
  
  #carrying out analyses for Research Question 3
  info(logger, "CARRYING OUT ANALYSES FOR RESEARCH QUESTION 3 - HEALTH ECON")
  print(paste0("Starting RQ3 Health Econ at ", Sys.time()))
  suppressWarnings(  
    source(here("3_Analysis", "RQ3HealthEconomics.R"))
    )
  print(paste0("Finishing RQ3 Health Econ at ", Sys.time()))
  info(logger, "CARRYING OUT ANALYSES FOR RESEARCH QUESTION 3 - HEALTH ECON IS DONE")
  
  # carrying out analyses for Research Question 3 - Plotting
  info(logger, "CARRYING OUT ANALYSES FOR RESEARCH QUESTION 3 - PLOTTING")
  print(paste0("Starting RQ3 Health Econ plots at ", Sys.time()))
  source(here("4_HealthEconomics", "Graphs_HE.R"))
  info(logger, "CARRYING OUT ANALYSES FOR RESEARCH QUESTION 3 - MATCHING IS DONE")
  print(paste0("Finishing RQ3 Health Econ plots at ", Sys.time()))
}

	if (country_setting %in% c("Spain", "UK")){
	  for (k in (1:length(washout_period))){
	    print(paste0("Starting the analysis for washout of ", washout_period[[k]], " at ", Sys.time()))
	    washout_folder <- here(output_folder, washout_period[[k]])
	    if (!dir.exists(washout_folder)) {
	      dir.create(washout_folder)
	    }
	    # create logger
	    log_file <- paste0(sub_output_folder, "/log.txt")
	    logger <- create.logger()
	    logfile(logger) <- log_file
	    level(logger) <- "INFO"
    
     # running necessary functions
 	    info(logger, "RUNNING NECESSARY FUNCTIONS")
      print(paste0("Running necessary functions at ", Sys.time()))
  	  source(here("Functions", "functions.R"))
      info(logger, "RUNNING NECESSARY FUNCTIONS IS DONE")
      print(paste0("Running necessary functions is done at ", Sys.time()))
    	 
      info(logger, "Starting incidence primary vs hospital")
      source(here("Miscellaneous", "rq2_additional_analysis_secondary.R"))
      source(here("Miscellaneous", "rq2_additional_analysis_primary.R"))
      info(logger, "Finishing incidence primary vs hospital")
	  }
	}

# delete temp files - be careful
for (k in (1:length(washout_period))){
  sub_output_folder <- here(output_folder, washout_period[[k]])
  file.remove(here(sub_output_folder, "tempData"))
}

# create zip file
info(logger, "ZIPPING RESULTS")
print(paste0("Outputing zips at ", Sys.time()))
output_folder <- basename(output_folder)
zip(
  zipfile = file.path(paste0(output_folder, "/Results_", db_name, ".zip")),
  files = list.files(output_folder, full.names = TRUE)
)
