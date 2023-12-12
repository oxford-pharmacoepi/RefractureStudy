info(logger, "READING IN PROVIDER COST SPREADSHEET (HEALTH ECONOMICS)")
## Define the path to the Excel file
file_path <- here("4_HealthEconomics", "Inputs", "provider_cost_inputs.xlsx")

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

specialty_names <- provider_cost_inputs %>% dplyr::filter(Include == 1) %>% dplyr::pull(specialty_source_value)

# VISIT DATA ----
info(logger, "START COMPUTING VISIT DATA")
source(here("4_HealthEconomics", "Visit_data_HE.R")) 
info(logger, "COMPUTING VISIT DATA IS DONE")
# COHORT DATA ----
info(logger, "START COHORT CONSTRUCTION FOR HEALTH ECONOMICS")
source(here("2_CohortCreation", "CohortRQ3_HE.R")) 
info(logger, "COHORT CONSTRUCTION FOR HEALTH ECONOMICS IS DONE")

info(logger, "START ANALYSES FOR HEALTH ECONOMICS")
# ANALYSIS -----
## 1. Visits - HCRU ----

### Apply the (1) function - Analyse primary care visits
info(logger, "START ANALYSE_VISITS FOR HEALTH ECONOMICS")
# Comparison 1: Target versus Matched cohort 1 
target_results <- analyse_visits(target_matched, cdm[["visit_data"]]) 
cohort1_comp1_results <- analyse_visits(cohort1_matched_to, cdm[["visit_data"]])
# Comparison 2: cohort 1 versus matched cohort 2
cohort1_comp2_results <- analyse_visits(cohort1_matched_from, cdm[["visit_data"]]) 
cohort2_results <- analyse_visits(cohort2_matched, cdm[["visit_data"]]) 

### Generate results
## Comparison 1: Target versus Matched cohort 1 
# Target
target_results_user <-target_results$user_only_summary %>% dplyr::collect()
target_results_all  <-target_results$all_summary %>% dplyr::collect()
target_non_service_users <-target_results$non_service_users %>% dplyr::collect()

# Matched Cohort1
cohort1_comp1_results_user <-cohort1_comp1_results$user_only_summary %>% dplyr::collect()
cohort1_comp1_results_all  <-cohort1_comp1_results$all_summary %>% dplyr::collect()
cohort1_comp1_non_service_users <-cohort1_comp1_results$non_service_users %>% dplyr::collect()

## Comparison 2: cohort 1 versus matched cohort 2
# Cohort 1
cohort1_comp2_results_user <-cohort1_comp2_results$user_only_summary %>% dplyr::collect()
cohort1_comp2_results_all  <-cohort1_comp2_results$all_summary %>% dplyr::collect()
cohort1_comp2_non_service_users <-cohort1_comp2_results$non_service_users %>% dplyr::collect()

# Matched - cohort 2
cohort2_results_user <-cohort2_results$user_only_summary %>% dplyr::collect()
cohort2_results_all  <-cohort2_results$all_summary %>% dplyr::collect()
cohort2_non_service_users <-cohort2_results$non_service_users %>% dplyr::collect()
info(logger, "ANALYSES_VISITS FOR HEALTH ECONOMICS IS DONE")

## 2. Visits - Cost ----
info(logger, "START ANALYSE_VISITS_COSTS FOR HEALTH ECONOMICS")
if (country_setting != "Netherlands") {
  
### Apply the (2) function - Estimate costs primary care visits 

# Comparison 1: Target versus Matched cohort 1 
target_results_cost <- analyse_visits_cost(target_matched, cdm[["visit_data"]]) 
cohort1_comp1_results_cost <- analyse_visits_cost(cohort1_matched_to, cdm[["visit_data"]])
# Comparison 2: cohort 1 versus matched cohort 2
cohort1_comp2_results_cost <- analyse_visits_cost(cohort1_matched_from, cdm[["visit_data"]]) 
cohort2_results_cost <- analyse_visits_cost(cohort2_matched, cdm[["visit_data"]]) 

### Generate results
## Comparison 1: Target versus Matched cohort 1 
# Target
target_results_user_cost <-target_results_cost$user_only_cost_summary %>% dplyr::collect()
target_results_all_cost <-target_results_cost$all_cost_summary %>% dplyr::collect()

# Matched Cohort1
cohort1_comp1_results_user_cost <-cohort1_comp1_results_cost$user_only_cost_summary %>% dplyr::collect()
cohort1_comp1_results_all_cost  <-cohort1_comp1_results_cost$all_cost_summary %>% dplyr::collect()

## Comparison 2: cohort 1 versus matched cohort 2
# Cohort 1
cohort1_comp2_results_user_cost <-cohort1_comp2_results_cost$user_only_cost_summary %>% dplyr::collect()
cohort1_comp2_results_all_cost  <-cohort1_comp2_results_cost$all_cost_summary %>% dplyr::collect()

# Matched - cohort 2
cohort2_results_user_cost <-cohort2_results_cost$user_only_cost_summary %>% dplyr::collect()
cohort2_results_all_cost  <-cohort2_results_cost$all_cost_summary %>% dplyr::collect()
}
info(logger, "ANALYSES_VISITS_COST FOR HEALTH ECONOMICS IS DONE")

## 3. Cohort - Summary ------

### Apply the (3) function - Cohort summary
info(logger, "STARTING COHORT SUMMARY FOR HEALTH ECONOMICS")
# Comparison 1: Target versus Matched cohort 1 
target_summary <- cohort_summary(target_matched, "Target", target_non_service_users)
cohort1_comp1_summary <- cohort_summary(cohort1_matched_to, "Matched cohort1", cohort1_comp1_non_service_users)
# Comparison 2: cohort 1 versus matched cohort 2
cohort1_comp2_summary <- cohort_summary(cohort1_matched_from, "Cohort1", cohort1_comp2_non_service_users)
cohort2_summary <- cohort_summary(cohort2_matched, "Matched cohort2", cohort2_non_service_users)

### Combine the summaries for the two comparisons
summary_cohort_comp1 <- bind_rows(target_summary, cohort1_comp1_summary)
summary_cohort_comp2 <- bind_rows(cohort1_comp2_summary, cohort2_summary)
info(logger, "COHORT SUMMARY FOR HEALTH ECONOMICS IS DONE")
## 4. Save to csv

# Write the list of data frames to an Excel file, each list should appear on a separate sheet
info(logger, "STARTING HEALTH ECONOMICS OUTPUT")
#HCRU
write.xlsx(target_results, file = here(sub_output_folder, "target_results.xlsx"))
write.xlsx(cohort1_comp1_results, file = here(sub_output_folder, "cohort1_comp1_results.xlsx"))
write.xlsx(cohort1_comp2_results, file = here(sub_output_folder, "cohort1_comp2_results.xlsx"))
write.xlsx(cohort2_results, file = here(sub_output_folder, "cohort2_results.xlsx"))

if (country_setting != "Netherlands") {
  
#Cost
write.xlsx(target_results_cost, file = here(sub_output_folder, "target_results_cost.xlsx"))
write.xlsx(cohort1_comp1_results_cost, file = here(sub_output_folder, "cohort1_comp1_results_cost.xlsx"))
write.xlsx(cohort1_comp2_results_cost, file = here(sub_output_folder, "cohort1_comp2_results_cost.xlsx"))
write.xlsx(cohort2_results_cost, file = here(sub_output_folder, "cohort2_results_cost.xlsx"))

}

#summary
write.xlsx(summary_cohort_comp1, file = here(sub_output_folder, "summary_cohort_comp1.xlsx"))
write.xlsx(summary_cohort_comp2, file = here(sub_output_folder, "summary_cohort_comp2.xlsx"))

#Other
if (country_setting != "UK") {
  write.xlsx(check_dates, file = here(sub_output_folder, "check_dates.xlsx"))
  write.xlsx(visit_type_by_specialty, file = here(sub_output_folder, "visit_type_by_specialty.xlsx"))
}
info(logger, "OUTPUT FOR HEALTH ECONOMICS IS DONE")
# suppressWarnings(
#   rm(all_summary,
#      cohort_combined,
#      cohort1_comp1_non_service_users,
#      cohort1_comp1_results_all,
#      cohort1_comp1_results_all_cost,
#      cohort1_comp1_results,
#      cohort1_comp2_results_all,
#      cohort1_comp2_non_service_users,
#      cohort1_comp1_summary,
#      cohort1_comp1_results_user,
#      cohort1_comp1_results_user_cost,
#      cohort1_matched_from,
#      cohort1_matched_to,
#      cohort1_comp1_results_cost,
#      cohort1_comp2_results, 
#      cohort1_comp2_results_cost,
#      cohort1_comp2_results_all_cost,
#      cohort1_comp2_results_user,
#      cohort1_comp2_results_user_cost,
#      cohort1_comp2_summary,
#      cohort2_matched,
#      cohort2_non_service_users,
#      cohort2_results_all,
#      cohort2_results_all_cost,
#      cohort2_results_user,
#      cohort2_results_user_cost,
#      cohort2_summary,
#      cohort2_results,
#      cohort2_results_cost,
#      non_service_users,
#      summary_cohort_comp1,
#      summary_cohort_comp2,
#      target_non_service_users,
#      target_matched,
#      target_results_all,
#      target_results_all_cost,
#      target_results_user,
#      target_results_user_cost,
#      target_summary,
#      target_results,
#      target_results_cost,
#      targetCohort,
#      user_only_summary
#      )
# )
