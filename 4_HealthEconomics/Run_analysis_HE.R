# Libraries ----
# install.packages("renv") # if not already installed, install renv from CRAN
# renv::activate() 
# renv::restore() # this should prompt you to install the various packages required for the study

##Libraries Gianluca
library(CDMConnector)
library(DBI)
library(log4r)
library(dplyr)
library(dbplyr)
library(purrr)
library(here)
library(RPostgres)
library(readxl)
library(tidyr)
library(matrixStats)
library(readxl)
## libraries Xihang
library(CDMConnector)
library(DBI)
library(log4r)
library(dplyr)
library(dbplyr)
library(here)
library(IncidencePrevalence)
library(readxl)
library(DescTools)
library(openxlsx)
library(CirceR) #remotes::install_github("ohdsi/CirceR")
library(testthat)
library(SqlRender)
library(lubridate)
library(PatientProfiles)
library(DrugUtilisation)
library(tidyr)
library(glmnet)
library(readr)
library(CodelistGenerator)
library(purrr)
library(MatchIt)
library(RPostgres)
library(matrixStats)

# Set up server ----

# database metadata and connection details 
# The name/ acronym for the database
db_name <- "HEALTH_ECON_AURUM"


# Set output folder location 
# the path to a folder where the results from this analysis will be saved
output_folder <- here(paste0("Results_", db_name))
if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}

# use your own .Renviron "keys" to select the database that you want to access
server_dbi <- Sys.getenv("DB_SERVER_DBI_name_database")
user <- Sys.getenv("DB_USER")
password <- Sys.getenv("DB_PASSWORD")
port <- Sys.getenv("DB_PORT") 
host <- Sys.getenv("DB_HOST")

db <- dbConnect(RPostgres::Postgres(),
                dbname = server_dbi,
                port = port,
                host = host,
                user = user,
                password = password)
tbl(db, sql("SELECT * FROM public.person limit 1"))

# The name of the schema that contains the OMOP CDM with patient-level data
cdm_database_schema <- "public"

# The name of the schema where results tables will be created 
results_database_schema <- "results"

# Name of stem outcome table in the result schema where the outcome cohorts will
# be stored. 
# Notes: 
# - if there is an existing table in your results schema with the same names it
#   will be overwritten
# - more than one cohort will be created
# - name must be lower case
stem_table <- "gianluca_2"

# minimum counts that can be displayed according to data governance
minimum_counts <- 5

# create cdm reference 
cdm <- CDMConnector::cdm_from_con(
  con = db,
  cdm_schema = cdm_database_schema,
  write_schema = results_database_schema
)
# check database connection
# running the next line should give you a count of your person table
cdm$person %>% 
  tally()

# Import cohort from RQ1 and RQ2 Xihang----

minimum_counts <- 5

# washout period, fractures of the same site within this window is considered as a re-recording
washout_period <- c(30,90) 

# study start date
study_start_date <- as.Date("2010-04-01")

# study end date
study_end_date <- as.Date("2018-03-31")

# prior obs
prior_observation <- 730

# create logger
log_file <- paste0(output_folder, "/log.txt")
logger <- create.logger()
logfile(logger) <- log_file
level(logger) <- "INFO"

k<-2

sub_output_folder <- here(output_folder, washout_period[[k]])
if (!dir.exists(sub_output_folder)) {
  dir.create(sub_output_folder)
}

source(here("Functions", "functions.R")) # line 13 - runanalysis.r
source(here("2_CohortCreation", "CleaningFractures.R")) # line 18 - runanalysis.r
source(here("2_CohortCreation", "CohortRQ1&2.R")) # line 23 - runanalysis.r
source(here("2_CohortCreation", "CohortRQ3.R")) # line 38 - runanalysis.r
#source(here("3_Analysis", "RQ3PSMatching.R")) # for the matching

# Data preparation RQ3 -----
# Ensure the readxl package is installed and loaded

# Country setting
country_setting <- "UK"

#Choose from
##UK
##Germany
##France
##Italy
##Spain
##Netherlands

# Import inputs for providers and costs

## Define the path to the Excel file
file_path <- here("4_Healtheconomics", "Inputs", "provider_cost_inputs.xlsx")

# Select the sheet name based on the country setting
sheet_name <- switch(country_setting,
                     "UK" = "UK", # Replace 'UK_Sheet_Name' with the actual sheet name
                     "Germany" = "Germany",
                     "France" = "France",
                     "Italy" = "Italy",
                     "Spain" = "Spain",
                     "Netherlands" = "Netherlands")

# Import the data from the selected sheet
provider_cost_inputs <- read_excel(file_path, sheet = sheet_name)

# VISIT DATA ----

source(here("4_Healtheconomics", "Visit_data_HE.R")) 

# COHORT DATA ----

source(here("4_Healtheconomics", "CohortRQ3_HE.R")) # might be better to combine with Xihang code

# ANALYSIS -----

source(here("4_Healtheconomics", "Function_HE.R")) 

## 1. Visits - HCRU ----

### Apply the (1) function - Analyse primary care visits

# Comparison 1: Target versus Matched cohort 1 
target_results <- analyse_visits(target_matched, visit_data) 
cohort1_comp1_results <- analyse_visits(cohort1_matched_to, visit_data)
# Comparison 2: cohort 1 versus matched cohort 2
cohort1_comp2_results <- analyse_visits(cohort1_matched_from, visit_data) 
cohort2_results <- analyse_visits(cohort2_matched, visit_data) 

### Generate results
## Comparison 1: Target versus Matched cohort 1 
# Target
target_results_user <-target_results$user_only_summary
target_results_all  <-target_results$all_summary
target_non_service_users <-target_results$non_service_users

# Matched Cohort1
cohort1_comp1_results_user <-cohort1_comp1_results$user_only_summary
cohort1_comp1_results_all  <-cohort1_comp1_results$all_summary
cohort1_comp1_non_service_users <-cohort1_comp1_results$non_service_users

## Comparison 2: cohort 1 versus matched cohort 2
# Cohort 1
cohort1_comp2_results_user <-cohort1_comp2_results$user_only_summary
cohort1_comp2_results_all  <-cohort1_comp2_results$all_summary
cohort1_comp2_non_service_users <-cohort1_comp2_results$non_service_users

# Matched - cohort 2
cohort2_results_user <-cohort2_results$user_only_summary
cohort2_results_all  <-cohort2_results$all_summary
cohort2_non_service_users <-cohort2_results$non_service_users


## 2. Visits - Cost ----

### Apply the (2) function - Estimate costs primary care visits 

# Comparison 1: Target versus Matched cohort 1 
target_results_cost <- analyse_visits_cost(target_matched, visit_data) 
cohort1_comp1_results_cost <- analyse_visits_cost(cohort1_matched_to, visit_data)
# Comparison 2: cohort 1 versus matched cohort 2
cohort1_comp2_results_cost <- analyse_visits_cost(cohort1_matched_from, visit_data) 
cohort2_results_cost <- analyse_visits_cost(cohort2_matched, visit_data) 

### Generate results
## Comparison 1: Target versus Matched cohort 1 
# Target
target_results_user_cost <-target_results_cost$user_only_summary
target_results_all_cost <-target_results_cost$all_summary

# Matched Cohort1
cohort1_comp1_results_user_cost <-cohort1_comp1_results_cost$user_only_summary
cohort1_comp1_results_all_cost  <-cohort1_comp1_results_cost$all_summary

## Comparison 2: cohort 1 versus matched cohort 2
# Cohort 1
cohort1_comp2_results_user_cost <-cohort1_comp2_results_cost$user_only_summary
cohort1_comp2_results_all_cost  <-cohort1_comp2_results_cost$all_summary

# Matched - cohort 2
cohort2_results_user_cost <-cohort2_results_cost$user_only_summary
cohort2_results_all_cost  <-cohort2_results_cost$all_summary

## 3. Cohort - Summary ------

### Apply the (3) function - Cohort summary

# Comparison 1: Target versus Matched cohort 1 
target_summary <- cohort_summary(target_matched, "Target", target_non_service_users)
cohort1_comp1_summary <- cohort_summary(cohort1_matched_to, "Matched cohort1", cohort1_comp1_non_service_users)
# Comparison 2: cohort 1 versus matched cohort 2
cohort1_comp2_summary <- cohort_summary(cohort1_matched_from, "Cohort1", cohort1_comp2_non_service_users)
cohort2_summary <- cohort_summary(cohort2_matched, "Matched cohort2", cohort2_non_service_users)

### Combine the summaries for the two comparisons
summary_cohort_comp1 <- bind_rows(target_summary, cohort1_comp1_summary)
summary_cohort_comp2 <- bind_rows(cohort1_comp2_summary, cohort2_summary)


## 4. Save to csv

# Write the list of data frames to an Excel file, each list should appear on a separate sheet

#HCRU
write_xlsx(target_results, "target_results.xlsx")
write_xlsx(cohort1_comp1_results, "cohort1_comp1_results.xlsx")
write_xlsx(cohort1_comp2_results, "cohort1_comp2_results.xlsx")
write_xlsx(cohort2_results, "cohort2_results.xlsx")
#Cost
write_xlsx(target_results_cost, "target_results_cost.xlsx")
write_xlsx(cohort1_comp1_results_cost, "cohort1_comp1_results_cost.xlsx")
write_xlsx(cohort1_comp2_results_cost, "cohort1_comp2_results_cost.xlsx")
write_xlsx(cohort2_results_cost, "cohort2_results_cost.xlsx")
#summary
write_xlsx(summary_cohort_comp1, "summary_cohort_comp1.xlsx")
write_xlsx(summary_cohort_comp2, "summary_cohort_comp2.xlsx")
#Other
if (country_setting != "UK") {
  write_xlsx(check_dates, "check_dates.xlsx")
  write_xlsx(visit_type_by_specialty, "visit_type_by_specialty.xlsx")
}

if (country_setting == "Netherlands") {
  write_xlsx(combined_frequency_table, "combined_frequency_table.xlsx")

}

}

