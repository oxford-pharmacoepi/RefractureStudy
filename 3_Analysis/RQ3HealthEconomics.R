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

primary_resources_subfolder <- here(sub_output_folder, "primary_resources")
if (!dir.exists(primary_resources_subfolder)) {
  dir.create(primary_resources_subfolder)
}

primary_costs_subfolder <- here(sub_output_folder, "primary_costs")
if (!dir.exists(primary_costs_subfolder)) {
  dir.create(primary_costs_subfolder)
}

if(country_setting %in% c("UK", "Italy")){
  specialty_names <- provider_cost_inputs %>% dplyr::filter(Include == 1) %>% dplyr::pull(specialty_source_value)
} else if(country_setting %in% c("Spain", "Germany", "France")){
  specialty_names <- provider_cost_inputs %>% dplyr::filter(Include == 1) %>% dplyr::pull(description_athena)
} else {
  specialty_names <- provider_cost_inputs %>% dplyr::filter(Include == 1) %>% dplyr::pull(description_athena)
}

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
target_results <- analyse_visits(cohort_combined = target_matched, visit_data = cdm[["visit_data"]]) 
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
info(logger, "START MASKING THE DATA")
target_results_output <- list()
for (name in (names(target_results)[1:length(target_results)])){
  target_results_output[[name]]<-target_results[[name]]
}

if (country_setting %in% c("UK", "Italy")){
  data_all <- target_results_output[["visits_count_wide"]] %>% 
    dplyr::select(-exposed_yrs, -cohort) %>% 
    pivot_longer(cols = -c(subject_id, index_date, follow_up_end)) %>% 
    dplyr::group_by(subject_id, index_date, follow_up_end) %>% 
    dplyr::summarise(tot_visits = sum(value, na.rm = T)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(cohort = "target",
                  tot_exposed_yrs = as.numeric((follow_up_end - index_date)/365.25),
                  tot_visits_per_yr = tot_visits/tot_exposed_yrs)
} else {
data_all <- target_results_output[["visits_count_wide"]] %>% 
  dplyr::select(-exposed_yrs, -cohort) %>% 
  pivot_longer(cols = -c(subject_id, index_date, follow_up_end, unit_cost, Include)) %>% 
  dplyr::group_by(subject_id, index_date, follow_up_end) %>% 
  dplyr::summarise(tot_visits = sum(value, na.rm = T)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(cohort = "target",
                tot_exposed_yrs = as.numeric((follow_up_end - index_date)/365.25),
                tot_visits_per_yr = tot_visits/tot_exposed_yrs)
}

data_user <- data_all %>% 
  dplyr::filter(tot_visits > 0)

target_results_output[["user_only_summary_statistics"]] <- tibble(mean_visit_per_year=(sum(data_user$tot_visits)/sum(data_user$tot_exposed_yrs)),
                                                                  min_visit_per_year = min(data_user$tot_visits_per_yr),
                                                                  max_visit_per_year = max(data_user$tot_visits_per_yr),
                                                                  sd_visit_per_year = signif(sd(data_user$tot_visits_per_yr), 4),
                                                                  median_visit_per_year = signif(quantile(data_user$tot_visits_per_yr, probs = (.5)), 4),
                                                                  lower_q_visits_per_year = signif(quantile(data_user$tot_visits_per_yr, probs = (.25)), 4),
                                                                  upper_q_visits_per_year = signif(quantile(data_user$tot_visits_per_yr, probs = (.75)), 4))

target_results_output[["all_summary_statistics"]] <- tibble(mean_visit_per_year=(sum(data_all$tot_visits)/sum(data_all$tot_exposed_yrs)),
                                                            min_visit_per_year = min(data_all$tot_visits_per_yr),
                                                            max_visit_per_year = max(data_all$tot_visits_per_yr),
                                                            sd_visit_per_year = signif(sd(data_all$tot_visits_per_yr),4),
                                                            median_visit_per_year = signif(quantile(data_all$tot_visits_per_yr, probs = (.5)), 4),
                                                            lower_q_visits_per_year = signif(quantile(data_all$tot_visits_per_yr, probs = (.25)), 4), 
                                                            upper_q_visits_per_year = signif(quantile(data_all$tot_visits_per_yr, probs = (.75)), 4))

target_results_output[[names(target_results)[1]]] <-target_results_output[[names(target_results)[1]]] %>% 
  dplyr::mutate(tot_visits = ifelse((num_subjects_visited<minimum_counts & num_subjects_visited>0), paste0("<", minimum_counts), as.numeric(.data$tot_visits)),
                mean_visits_per_year = ifelse((num_subjects_visited<minimum_counts & num_subjects_visited>0), paste0("<", minimum_counts), as.numeric(.data$mean_visits_per_year)),
                sd_visits_per_year = ifelse((num_subjects_visited<minimum_counts & num_subjects_visited>0), paste0("<", minimum_counts), as.numeric(.data$sd_visits_per_year)),
                min_visits_per_year  = ifelse((num_subjects_visited<minimum_counts & num_subjects_visited>0), paste0("<", minimum_counts), as.numeric(.data$min_visits_per_year )),
                max_visits_per_year = ifelse((num_subjects_visited<minimum_counts & num_subjects_visited>0), paste0("<", minimum_counts), as.numeric(.data$max_visits_per_year)),
                num_subjects_visited = ifelse((num_subjects_visited<minimum_counts & num_subjects_visited>0), paste0("<", minimum_counts), as.numeric(.data$num_subjects_visited)))

target_results_output[[names(target_results)[2]]] <-target_results_output[[names(target_results)[2]]] %>% 
  dplyr::mutate(mean_visits_per_year = ifelse((tot_visits<minimum_counts & tot_visits>0), paste0("<", minimum_counts), as.numeric(.data$mean_visits_per_year)),
                sd_visits_per_year = ifelse((tot_visits<minimum_counts & tot_visits>0), paste0("<", minimum_counts), as.numeric(.data$sd_visits_per_year)),
                min_visits_per_year  = ifelse((tot_visits<minimum_counts & tot_visits>0), paste0("<", minimum_counts), as.numeric(.data$min_visits_per_year )),
                max_visits_per_year = ifelse((tot_visits<minimum_counts & tot_visits>0), paste0("<", minimum_counts), as.numeric(.data$max_visits_per_year)),
                num_subjects_visited = ifelse((tot_visits<minimum_counts & tot_visits>0), paste0("<", minimum_counts), as.numeric(.data$num_subjects_visited)),
                tot_visits = ifelse((tot_visits<minimum_counts & tot_visits>0), paste0("<", minimum_counts), as.numeric(.data$tot_visits)))

if(target_results_output[[names(target_results)[3]]]$nonservice<5 & target_results_output[[names(target_results)[3]]]$nonservice>0){
  target_results_output[[names(target_results)[3]]]$nonservice <- paste0("<", minimum_counts)
}

#####
cohort1_comp1_results_output <- list()
for (name in (names(cohort1_comp1_results)[1:length(cohort1_comp1_results)])){
  cohort1_comp1_results_output[[name]]<-cohort1_comp1_results[[name]]
}

if (country_setting %in% c("UK", "Italy")){
  data_all <- cohort1_comp1_results_output[["visits_count_wide"]] %>% 
  dplyr::select(-exposed_yrs, -cohort) %>% 
  pivot_longer(cols = -c(subject_id, index_date, follow_up_end)) %>% 
  dplyr::group_by(subject_id, index_date, follow_up_end) %>% 
  dplyr::summarise(tot_visits = sum(value, na.rm = T)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(cohort = "cohort1_comp1",
                tot_exposed_yrs = as.numeric((follow_up_end - index_date)/365.25),
                tot_visits_per_yr = tot_visits/tot_exposed_yrs)
} else {
  data_all <- cohort1_comp1_results_output[["visits_count_wide"]] %>% 
    dplyr::select(-exposed_yrs, -cohort) %>% 
    pivot_longer(cols = -c(subject_id, index_date, follow_up_end, unit_cost, Include)) %>% 
    dplyr::group_by(subject_id, index_date, follow_up_end) %>% 
    dplyr::summarise(tot_visits = sum(value, na.rm = T)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(cohort = "cohort1_comp1",
                  tot_exposed_yrs = as.numeric((follow_up_end - index_date)/365.25),
                  tot_visits_per_yr = tot_visits/tot_exposed_yrs)
  }

data_user <- data_all %>% 
  dplyr::filter(tot_visits > 0)

cohort1_comp1_results_output[["user_only_summary_statistics"]] <- tibble(mean_visit_per_year=(sum(data_user$tot_visits)/sum(data_user$tot_exposed_yrs)),
                                                                         min_visit_per_year = min(data_user$tot_visits_per_yr),
                                                                         max_visit_per_year = max(data_user$tot_visits_per_yr),
                                                                         sd_visit_per_year = signif(sd(data_user$tot_visits_per_yr), 4),
                                                                         median_visit_per_year = signif(quantile(data_user$tot_visits_per_yr, probs = (.5)), 4),
                                                                         lower_q_visits_per_year = signif(quantile(data_user$tot_visits_per_yr, probs = (.25)), 4),
                                                                         upper_q_visits_per_year = signif(quantile(data_user$tot_visits_per_yr, probs = (.75)), 4))

cohort1_comp1_results_output[["all_summary_statistics"]] <- tibble(mean_visit_per_year=(sum(data_all$tot_visits)/sum(data_all$tot_exposed_yrs)),
                                                                   min_visit_per_year = min(data_all$tot_visits_per_yr),
                                                                   max_visit_per_year = max(data_all$tot_visits_per_yr),
                                                                   sd_visit_per_year = signif(sd(data_all$tot_visits_per_yr),4),
                                                                   median_visit_per_year = signif(quantile(data_all$tot_visits_per_yr, probs = (.5)), 4),
                                                                   lower_q_visits_per_year = signif(quantile(data_all$tot_visits_per_yr, probs = (.25)), 4), 
                                                                   upper_q_visits_per_year = signif(quantile(data_all$tot_visits_per_yr, probs = (.75)), 4))

cohort1_comp1_results_output[[names(cohort1_comp1_results)[1]]] <-cohort1_comp1_results_output[[names(cohort1_comp1_results)[1]]] %>% 
  dplyr::mutate(tot_visits = ifelse((num_subjects_visited<minimum_counts & num_subjects_visited>0), paste0("<", minimum_counts), as.numeric(.data$tot_visits)),
                mean_visits_per_year = ifelse((num_subjects_visited<minimum_counts & num_subjects_visited>0), paste0("<", minimum_counts), as.numeric(.data$mean_visits_per_year)),
                sd_visits_per_year = ifelse((num_subjects_visited<minimum_counts & num_subjects_visited>0), paste0("<", minimum_counts), as.numeric(.data$sd_visits_per_year)),
                min_visits_per_year  = ifelse((num_subjects_visited<minimum_counts & num_subjects_visited>0), paste0("<", minimum_counts), as.numeric(.data$min_visits_per_year )),
                max_visits_per_year = ifelse((num_subjects_visited<minimum_counts & num_subjects_visited>0), paste0("<", minimum_counts), as.numeric(.data$max_visits_per_year)),
                num_subjects_visited = ifelse((num_subjects_visited<minimum_counts & num_subjects_visited>0), paste0("<", minimum_counts), as.numeric(.data$num_subjects_visited)))

cohort1_comp1_results_output[[names(cohort1_comp1_results)[2]]] <-cohort1_comp1_results_output[[names(cohort1_comp1_results)[2]]] %>% 
  dplyr::mutate(mean_visits_per_year = ifelse((tot_visits<minimum_counts & tot_visits>0), paste0("<", minimum_counts), as.numeric(.data$mean_visits_per_year)),
                sd_visits_per_year = ifelse((tot_visits<minimum_counts & tot_visits>0), paste0("<", minimum_counts), as.numeric(.data$sd_visits_per_year)),
                min_visits_per_year  = ifelse((tot_visits<minimum_counts & tot_visits>0), paste0("<", minimum_counts), as.numeric(.data$min_visits_per_year )),
                max_visits_per_year = ifelse((tot_visits<minimum_counts & tot_visits>0), paste0("<", minimum_counts), as.numeric(.data$max_visits_per_year)),
                num_subjects_visited = ifelse((tot_visits<minimum_counts & tot_visits>0), paste0("<", minimum_counts), as.numeric(.data$num_subjects_visited)),
                tot_visits = ifelse((tot_visits<minimum_counts & tot_visits>0), paste0("<", minimum_counts), as.numeric(.data$tot_visits)))

if(cohort1_comp1_results_output[[names(cohort1_comp1_results)[3]]]$nonservice<5 & cohort1_comp1_results_output[[names(cohort1_comp1_results)[3]]]$nonservice>0){
  cohort1_comp1_results_output[[names(cohort1_comp1_results)[3]]]$nonservice <- paste0("<", minimum_counts)
}

#####
cohort1_comp2_results_output <- list()
for (name in (names(cohort1_comp2_results)[1:length(cohort1_comp2_results)])){
  cohort1_comp2_results_output[[name]]<-cohort1_comp2_results[[name]]
}

if (country_setting %in% c("UK", "Italy")){
  data_all <- cohort1_comp2_results_output[["visits_count_wide"]] %>% 
    dplyr::select(-exposed_yrs, -cohort) %>% 
    pivot_longer(cols = -c(subject_id, index_date, follow_up_end)) %>% 
    dplyr::group_by(subject_id, index_date, follow_up_end) %>% 
    dplyr::summarise(tot_visits = sum(value, na.rm = T)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(cohort = "cohort1_comp1",
                  tot_exposed_yrs = as.numeric((follow_up_end - index_date)/365.25),
                  tot_visits_per_yr = tot_visits/tot_exposed_yrs)
} else {
  data_all <- cohort1_comp2_results_output[["visits_count_wide"]] %>% 
    dplyr::select(-exposed_yrs, -cohort) %>% 
    pivot_longer(cols = -c(subject_id, index_date, follow_up_end, unit_cost, Include)) %>% 
    dplyr::group_by(subject_id, index_date, follow_up_end) %>% 
    dplyr::summarise(tot_visits = sum(value, na.rm = T)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(cohort = "cohort1_comp1",
                  tot_exposed_yrs = as.numeric((follow_up_end - index_date)/365.25),
                  tot_visits_per_yr = tot_visits/tot_exposed_yrs)
}

data_user <- data_all %>% 
  dplyr::filter(tot_visits > 0)

cohort1_comp2_results_output[["user_only_summary_statistics"]] <- tibble(mean_visit_per_year=(sum(data_user$tot_visits)/sum(data_user$tot_exposed_yrs)),
                                                                         min_visit_per_year = min(data_user$tot_visits_per_yr),
                                                                         max_visit_per_year = max(data_user$tot_visits_per_yr),
                                                                         sd_visit_per_year = signif(sd(data_user$tot_visits_per_yr), 4),
                                                                         median_visit_per_year = signif(quantile(data_user$tot_visits_per_yr, probs = (.5)), 4),
                                                                         lower_q_visits_per_year = signif(quantile(data_user$tot_visits_per_yr, probs = (.25)), 4),
                                                                         upper_q_visits_per_year = signif(quantile(data_user$tot_visits_per_yr, probs = (.75)), 4))

cohort1_comp2_results_output[["all_summary_statistics"]] <- tibble(mean_visit_per_year=(sum(data_all$tot_visits)/sum(data_all$tot_exposed_yrs)),
                                                                   min_visit_per_year = min(data_all$tot_visits_per_yr),
                                                                   max_visit_per_year = max(data_all$tot_visits_per_yr),
                                                                   sd_visit_per_year = signif(sd(data_all$tot_visits_per_yr),4),
                                                                   median_visit_per_year = signif(quantile(data_all$tot_visits_per_yr, probs = (.5)), 4),
                                                                   lower_q_visits_per_year = signif(quantile(data_all$tot_visits_per_yr, probs = (.25)), 4), 
                                                                   upper_q_visits_per_year = signif(quantile(data_all$tot_visits_per_yr, probs = (.75)), 4))

cohort1_comp2_results_output[[names(cohort1_comp2_results)[1]]] <-cohort1_comp2_results_output[[names(cohort1_comp2_results)[1]]] %>% 
  dplyr::mutate(tot_visits = ifelse((num_subjects_visited<minimum_counts & num_subjects_visited>0), paste0("<", minimum_counts), as.numeric(.data$tot_visits)),
                mean_visits_per_year = ifelse((num_subjects_visited<minimum_counts & num_subjects_visited>0), paste0("<", minimum_counts), as.numeric(.data$mean_visits_per_year)),
                sd_visits_per_year = ifelse((num_subjects_visited<minimum_counts & num_subjects_visited>0), paste0("<", minimum_counts), as.numeric(.data$sd_visits_per_year)),
                min_visits_per_year  = ifelse((num_subjects_visited<minimum_counts & num_subjects_visited>0), paste0("<", minimum_counts), as.numeric(.data$min_visits_per_year )),
                max_visits_per_year = ifelse((num_subjects_visited<minimum_counts & num_subjects_visited>0), paste0("<", minimum_counts), as.numeric(.data$max_visits_per_year)),
                num_subjects_visited = ifelse((num_subjects_visited<minimum_counts & num_subjects_visited>0), paste0("<", minimum_counts), as.numeric(.data$num_subjects_visited)))

cohort1_comp2_results_output[[names(cohort1_comp2_results)[2]]] <-cohort1_comp2_results_output[[names(cohort1_comp2_results)[2]]] %>% 
  dplyr::mutate(mean_visits_per_year = ifelse((tot_visits<minimum_counts & tot_visits>0), paste0("<", minimum_counts), as.numeric(.data$mean_visits_per_year)),
                sd_visits_per_year = ifelse((tot_visits<minimum_counts & tot_visits>0), paste0("<", minimum_counts), as.numeric(.data$sd_visits_per_year)),
                min_visits_per_year  = ifelse((tot_visits<minimum_counts & tot_visits>0), paste0("<", minimum_counts), as.numeric(.data$min_visits_per_year )),
                max_visits_per_year = ifelse((tot_visits<minimum_counts & tot_visits>0), paste0("<", minimum_counts), as.numeric(.data$max_visits_per_year)),
                num_subjects_visited = ifelse((tot_visits<minimum_counts & tot_visits>0), paste0("<", minimum_counts), as.numeric(.data$num_subjects_visited)),
                tot_visits = ifelse((tot_visits<minimum_counts & tot_visits>0), paste0("<", minimum_counts), as.numeric(.data$tot_visits)))

if(cohort1_comp2_results_output[[names(cohort1_comp2_results)[3]]]$nonservice<5 & cohort1_comp2_results_output[[names(cohort1_comp2_results)[3]]]$nonservice>0){
  cohort1_comp2_results_output[[names(cohort1_comp2_results)[3]]]$nonservice <- paste0("<", minimum_counts)
}

####
cohort2_results_output <- list()
for (name in (names(cohort2_results)[1:length(cohort2_results)])){
  cohort2_results_output[[name]]<-cohort2_results[[name]]
}

if (country_setting %in% c("UK", "Italy")){
  data_all <- cohort2_results_output[["visits_count_wide"]] %>% 
    dplyr::select(-exposed_yrs, -cohort) %>% 
    pivot_longer(cols = -c(subject_id, index_date, follow_up_end)) %>% 
    dplyr::group_by(subject_id, index_date, follow_up_end) %>% 
    dplyr::summarise(tot_visits = sum(value, na.rm = T)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(cohort = "cohort2",
                  tot_exposed_yrs = as.numeric((follow_up_end - index_date)/365.25),
                  tot_visits_per_yr = tot_visits/tot_exposed_yrs)
} else {
  data_all <- cohort2_results_output[["visits_count_wide"]] %>% 
    dplyr::select(-exposed_yrs, -cohort) %>% 
    pivot_longer(cols = -c(subject_id, index_date, follow_up_end, unit_cost, Include)) %>% 
    dplyr::group_by(subject_id, index_date, follow_up_end) %>% 
    dplyr::summarise(tot_visits = sum(value, na.rm = T)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(cohort = "cohort2",
                  tot_exposed_yrs = as.numeric((follow_up_end - index_date)/365.25),
                  tot_visits_per_yr = tot_visits/tot_exposed_yrs)
  
}

data_user <- data_all %>% 
  dplyr::filter(tot_visits > 0)

cohort2_results_output[["user_only_summary_statistics"]] <- tibble(mean_visit_per_year=(sum(data_user$tot_visits)/sum(data_user$tot_exposed_yrs)),
                                                                   min_visit_per_year = min(data_user$tot_visits_per_yr),
                                                                   max_visit_per_year = max(data_user$tot_visits_per_yr),
                                                                   sd_visit_per_year = signif(sd(data_user$tot_visits_per_yr), 4),
                                                                   median_visit_per_year = signif(quantile(data_user$tot_visits_per_yr, probs = (.5)), 4),
                                                                   lower_q_visits_per_year = signif(quantile(data_user$tot_visits_per_yr, probs = (.25)), 4),
                                                                   upper_q_visits_per_year = signif(quantile(data_user$tot_visits_per_yr, probs = (.75)), 4))

cohort2_results_output[["all_summary_statistics"]] <- tibble(mean_visit_per_year=(sum(data_all$tot_visits)/sum(data_all$tot_exposed_yrs)),
                                                             min_visit_per_year = min(data_all$tot_visits_per_yr),
                                                             max_visit_per_year = max(data_all$tot_visits_per_yr),
                                                             sd_visit_per_year = signif(sd(data_all$tot_visits_per_yr),4),
                                                             median_visit_per_year = signif(quantile(data_all$tot_visits_per_yr, probs = (.5)), 4),
                                                             lower_q_visits_per_year = signif(quantile(data_all$tot_visits_per_yr, probs = (.25)), 4), 
                                                             upper_q_visits_per_year = signif(quantile(data_all$tot_visits_per_yr, probs = (.75)), 4))

cohort2_results_output[[names(cohort2_results)[1]]] <-cohort2_results_output[[names(cohort2_results)[1]]] %>% 
  dplyr::mutate(tot_visits = ifelse((num_subjects_visited<minimum_counts & num_subjects_visited>0), paste0("<", minimum_counts), as.numeric(.data$tot_visits)),
                mean_visits_per_year = ifelse((num_subjects_visited<minimum_counts & num_subjects_visited>0), paste0("<", minimum_counts), as.numeric(.data$mean_visits_per_year)),
                sd_visits_per_year = ifelse((num_subjects_visited<minimum_counts & num_subjects_visited>0), paste0("<", minimum_counts), as.numeric(.data$sd_visits_per_year)),
                min_visits_per_year  = ifelse((num_subjects_visited<minimum_counts & num_subjects_visited>0), paste0("<", minimum_counts), as.numeric(.data$min_visits_per_year )),
                max_visits_per_year = ifelse((num_subjects_visited<minimum_counts & num_subjects_visited>0), paste0("<", minimum_counts), as.numeric(.data$max_visits_per_year)),
                num_subjects_visited = ifelse((num_subjects_visited<minimum_counts & num_subjects_visited>0), paste0("<", minimum_counts), as.numeric(.data$num_subjects_visited)))

cohort2_results_output[[names(cohort2_results)[2]]] <-cohort2_results_output[[names(cohort2_results)[2]]] %>% 
  dplyr::mutate(mean_visits_per_year = ifelse((tot_visits<minimum_counts & tot_visits>0), paste0("<", minimum_counts), as.numeric(.data$mean_visits_per_year)),
                sd_visits_per_year = ifelse((tot_visits<minimum_counts & tot_visits>0), paste0("<", minimum_counts), as.numeric(.data$sd_visits_per_year)),
                min_visits_per_year  = ifelse((tot_visits<minimum_counts & tot_visits>0), paste0("<", minimum_counts), as.numeric(.data$min_visits_per_year )),
                max_visits_per_year = ifelse((tot_visits<minimum_counts & tot_visits>0), paste0("<", minimum_counts), as.numeric(.data$max_visits_per_year)),
                num_subjects_visited = ifelse((tot_visits<minimum_counts & tot_visits>0), paste0("<", minimum_counts), as.numeric(.data$num_subjects_visited)),
                tot_visits = ifelse((tot_visits<minimum_counts & tot_visits>0), paste0("<", minimum_counts), as.numeric(.data$tot_visits)))

if(cohort2_results_output[[names(cohort2_results)[3]]]$nonservice<5 & cohort2_results_output[[names(cohort2_results)[3]]]$nonservice>0){
  cohort2_results_output[[names(cohort2_results)[3]]]$nonservice <- paste0("<", minimum_counts)
}

#HCRU
write.xlsx(target_results_output[-4], file = here(primary_resources_subfolder, "target_results.xlsx"))
write.xlsx(cohort1_comp1_results_output[-4], file = here(primary_resources_subfolder, "cohort1_comp1_results.xlsx"))
write.xlsx(cohort1_comp2_results_output[-4], file = here(primary_resources_subfolder, "cohort1_comp2_results.xlsx"))
write.xlsx(cohort2_results_output[-4], file = here(primary_resources_subfolder, "cohort2_results.xlsx"))

if (country_setting != "Netherlands") {
  
#Cost
  target_results_cost_output <- list()
  for (name in (names(target_results_cost)[1:length(target_results_cost)])){
    target_results_cost_output[[name]]<-target_results_cost[[name]]
  }
  
  data_all <- target_results_cost_output[["visits_cost_wide"]] %>% 
    dplyr::select(-exposed_yrs, -cohort) %>% 
    pivot_longer(cols = -c(subject_id, index_date, follow_up_end)) %>% 
    dplyr::group_by(subject_id, index_date, follow_up_end) %>% 
    dplyr::summarise(tot_visits = sum(value, na.rm = T)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(cohort = "target",
                  tot_exposed_yrs = as.numeric((follow_up_end - index_date)/365.25),
                  tot_visits_per_yr = tot_visits/tot_exposed_yrs)
  
  data_user <- data_all %>% 
    dplyr::filter(tot_visits > 0)
  
  target_results_cost_output[["user_only_summary_statistics"]] <- tibble(mean_cost_visit_per_year=(sum(data_user$tot_visits)/sum(data_user$tot_exposed_yrs)),
                                                                    min_cost_visit_per_year = min(data_user$tot_visits_per_yr),
                                                                    max_cost_visit_per_year = max(data_user$tot_visits_per_yr),
                                                                    sd_cost_visit_per_year = signif(sd(data_user$tot_visits_per_yr), 4),
                                                                    median_cost_visit_per_year = signif(quantile(data_user$tot_visits_per_yr, probs = (.5)), 4),
                                                                    lower_q_cost_visits_per_year = signif(quantile(data_user$tot_visits_per_yr, probs = (.25)), 4),
                                                                    upper_q_cost_visits_per_year = signif(quantile(data_user$tot_visits_per_yr, probs = (.75)), 4))
  
  target_results_cost_output[["all_summary_statistics"]] <- tibble(mean_cost_visit_per_year=(sum(data_all$tot_visits)/sum(data_all$tot_exposed_yrs)),
                                                              min_visit_cost_per_year = min(data_all$tot_visits_per_yr),
                                                              max_visit_cost_per_year = max(data_all$tot_visits_per_yr),
                                                              sd_visit_cost_per_year = signif(sd(data_all$tot_visits_per_yr),4),
                                                              median_cost_visit_per_year = signif(quantile(data_all$tot_visits_per_yr, probs = (.5)), 4),
                                                              lower_q_cost_visits_per_year = signif(quantile(data_all$tot_visits_per_yr, probs = (.25)), 4), 
                                                              upper_q_cost_visits_per_year = signif(quantile(data_all$tot_visits_per_yr, probs = (.75)), 4))
  
  target_results_cost_output[[names(target_results_cost)[1]]] <-target_results_cost_output[[names(target_results_cost)[1]]] %>% 
    dplyr::mutate(tot_visits_costs = ifelse((num_subjects_visited<minimum_counts & num_subjects_visited>0), paste0("<", minimum_counts), round(as.numeric(.data$tot_visits_costs),1)),
                  mean_cost_visits_per_year = ifelse((num_subjects_visited<minimum_counts & num_subjects_visited>0), paste0("<", minimum_counts), round(as.numeric(.data$mean_cost_visits_per_year),1)),
                  sd_cost_visits_per_year = ifelse((num_subjects_visited<minimum_counts & num_subjects_visited>0), paste0("<", minimum_counts), round(as.numeric(.data$sd_cost_visits_per_year),1)),
                  min_cost_visits_per_year  = ifelse((num_subjects_visited<minimum_counts & num_subjects_visited>0), paste0("<", minimum_counts), round(as.numeric(.data$min_cost_visits_per_year),1)),
                  max_cost_visits_per_year = ifelse((num_subjects_visited<minimum_counts & num_subjects_visited>0), paste0("<", minimum_counts), round(as.numeric(.data$max_cost_visits_per_year), 1)), 
                  num_subjects_visited = ifelse((num_subjects_visited<minimum_counts & num_subjects_visited>0), paste0("<", minimum_counts), round(as.numeric(.data$num_subjects_visited),1)))
  
  target_results_cost_output[[names(target_results_cost)[2]]] <-target_results_cost_output[[names(target_results_cost)[2]]] %>% 
    dplyr::mutate(tot_visits_costs = ifelse((num_subjects_visited<minimum_counts & num_subjects_visited > 0), paste0("<", minimum_counts), round(as.numeric(.data$tot_visits_costs),1)),
                  mean_cost_visits_per_year = ifelse((num_subjects_visited < minimum_counts & num_subjects_visited > 0), paste0("<", minimum_counts), as.numeric(.data$mean_cost_visits_per_year)),
                  sd_cost_visits_per_year = ifelse((num_subjects_visited < minimum_counts & num_subjects_visited > 0), paste0("<", minimum_counts), as.numeric(.data$sd_cost_visits_per_year)),
                  min_cost_visits_per_year  = ifelse((num_subjects_visited < minimum_counts & num_subjects_visited > 0), paste0("<", minimum_counts), as.numeric(.data$min_cost_visits_per_year )),
                  max_cost_visits_per_year = ifelse((num_subjects_visited < minimum_counts & num_subjects_visited > 0), paste0("<", minimum_counts), as.numeric(.data$max_cost_visits_per_year)),
                  num_subjects_visited = ifelse((num_subjects_visited < minimum_counts & num_subjects_visited > 0), paste0("<", minimum_counts), as.numeric(.data$num_subjects_visited)))
  
  #####
  cohort1_comp1_results_cost_output <- list()
  for (name in (names(cohort1_comp1_results_cost)[1:length(cohort1_comp1_results_cost)])){
    cohort1_comp1_results_cost_output[[name]]<-cohort1_comp1_results_cost[[name]]
  }
  
  data_all <- cohort1_comp1_results_cost_output[["visits_cost_wide"]] %>% 
    dplyr::select(-exposed_yrs, -cohort) %>% 
    pivot_longer(cols = -c(subject_id, index_date, follow_up_end)) %>% 
    dplyr::group_by(subject_id, index_date, follow_up_end) %>% 
    dplyr::summarise(tot_visits = sum(value, na.rm = T)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(cohort = "cohort1_comp1",
                  tot_exposed_yrs = as.numeric((follow_up_end - index_date)/365.25),
                  tot_visits_per_yr = tot_visits/tot_exposed_yrs)
  
  data_user <- data_all %>% 
    dplyr::filter(tot_visits > 0)
  
  cohort1_comp1_results_cost_output[["user_only_summary_statistics"]] <- tibble(mean_cost_visit_per_year=(sum(data_user$tot_visits)/sum(data_user$tot_exposed_yrs)),
                                                                           min_cost_visit_per_year = min(data_user$tot_visits_per_yr),
                                                                           max_cost_visit_per_year = max(data_user$tot_visits_per_yr),
                                                                           sd_cost_visit_per_year = signif(sd(data_user$tot_visits_per_yr), 4),
                                                                           median_cost_visit_per_year = signif(quantile(data_user$tot_visits_per_yr, probs = (.5)), 4),
                                                                           lower_q_cost_visits_per_year = signif(quantile(data_user$tot_visits_per_yr, probs = (.25)), 4),
                                                                           upper_q_cost_visits_per_year = signif(quantile(data_user$tot_visits_per_yr, probs = (.75)), 4))
  
  cohort1_comp1_results_cost_output[["all_summary_statistics"]] <- tibble(mean_cost_visit_per_year=(sum(data_all$tot_visits)/sum(data_all$tot_exposed_yrs)),
                                                                     min_cost_visit_per_year = min(data_all$tot_visits_per_yr),
                                                                     max_cost_visit_per_year = max(data_all$tot_visits_per_yr),
                                                                     sd_cost_visit_per_year = signif(sd(data_all$tot_visits_per_yr),4),
                                                                     median_cost_visit_per_year = signif(quantile(data_all$tot_visits_per_yr, probs = (.5)), 4),
                                                                     lower_q_cost_visits_per_year = signif(quantile(data_all$tot_visits_per_yr, probs = (.25)), 4), 
                                                                     upper_q_cost_visits_per_year = signif(quantile(data_all$tot_visits_per_yr, probs = (.75)), 4))
  
  cohort1_comp1_results_cost_output[[names(cohort1_comp1_results_cost)[1]]] <-cohort1_comp1_results_cost_output[[names(cohort1_comp1_results_cost)[1]]] %>% 
    dplyr::mutate(tot_visits_costs = ifelse((num_subjects_visited<minimum_counts & num_subjects_visited>0), paste0("<", minimum_counts), round(as.numeric(.data$tot_visits_costs),1)),
                  mean_cost_visits_per_year = ifelse((num_subjects_visited<minimum_counts & num_subjects_visited>0), paste0("<", minimum_counts), round(as.numeric(.data$mean_cost_visits_per_year),1)),
                  sd_cost_visits_per_year = ifelse((num_subjects_visited<minimum_counts & num_subjects_visited>0), paste0("<", minimum_counts), round(as.numeric(.data$sd_cost_visits_per_year),1)),
                  min_cost_visits_per_year  = ifelse((num_subjects_visited<minimum_counts & num_subjects_visited>0), paste0("<", minimum_counts), round(as.numeric(.data$min_cost_visits_per_year),1)),
                  max_cost_visits_per_year = ifelse((num_subjects_visited<minimum_counts & num_subjects_visited>0), paste0("<", minimum_counts), round(as.numeric(.data$max_cost_visits_per_year), 1)), 
                  num_subjects_visited = ifelse((num_subjects_visited<minimum_counts & num_subjects_visited>0), paste0("<", minimum_counts), round(as.numeric(.data$num_subjects_visited),1)))
  
  cohort1_comp1_results_cost_output[[names(cohort1_comp1_results_cost)[2]]] <-cohort1_comp1_results_cost_output[[names(cohort1_comp1_results_cost)[2]]] %>% 
    dplyr::mutate(tot_visits_costs = ifelse((num_subjects_visited<minimum_counts & num_subjects_visited > 0), paste0("<", minimum_counts), round(as.numeric(.data$tot_visits_costs),1)),
                  mean_cost_visits_per_year = ifelse((num_subjects_visited < minimum_counts & num_subjects_visited > 0), paste0("<", minimum_counts), as.numeric(.data$mean_cost_visits_per_year)),
                  sd_cost_visits_per_year = ifelse((num_subjects_visited < minimum_counts & num_subjects_visited > 0), paste0("<", minimum_counts), as.numeric(.data$sd_cost_visits_per_year)),
                  min_cost_visits_per_year  = ifelse((num_subjects_visited < minimum_counts & num_subjects_visited > 0), paste0("<", minimum_counts), as.numeric(.data$min_cost_visits_per_year )),
                  max_cost_visits_per_year = ifelse((num_subjects_visited < minimum_counts & num_subjects_visited > 0), paste0("<", minimum_counts), as.numeric(.data$max_cost_visits_per_year)),
                  num_subjects_visited = ifelse((num_subjects_visited < minimum_counts & num_subjects_visited > 0), paste0("<", minimum_counts), as.numeric(.data$num_subjects_visited)))
  
  #####
  cohort1_comp2_results_cost_output <- list()
  for (name in (names(cohort1_comp2_results_cost)[1:length(cohort1_comp2_results_cost)])){
    cohort1_comp2_results_cost_output[[name]]<-cohort1_comp2_results_cost[[name]]
  }
  
  data_all <- cohort1_comp2_results_cost_output[["visits_cost_wide"]] %>% 
    dplyr::select(-exposed_yrs, -cohort) %>% 
    pivot_longer(cols = -c(subject_id, index_date, follow_up_end)) %>% 
    dplyr::group_by(subject_id, index_date, follow_up_end) %>% 
    dplyr::summarise(tot_visits = sum(value, na.rm = T)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(cohort = "cohort1_comp2",
                  tot_exposed_yrs = as.numeric((follow_up_end - index_date)/365.25),
                  tot_visits_per_yr = tot_visits/tot_exposed_yrs)
  
  data_user <- data_all %>% 
    dplyr::filter(tot_visits > 0)
  
  cohort1_comp2_results_cost_output[["user_only_summary_statistics"]] <- tibble(mean_cost_visit_per_year=(sum(data_user$tot_visits)/sum(data_user$tot_exposed_yrs)),
                                                                                min_cost_visit_per_year = min(data_user$tot_visits_per_yr),
                                                                                max_cost_visit_per_year = max(data_user$tot_visits_per_yr),
                                                                                sd_cost_visit_per_year = signif(sd(data_user$tot_visits_per_yr), 4),
                                                                                median_cost_visit_per_year = signif(quantile(data_user$tot_visits_per_yr, probs = (.5)), 4),
                                                                                lower_q_cost_visits_per_year = signif(quantile(data_user$tot_visits_per_yr, probs = (.25)), 4),
                                                                                upper_q_cost_visits_per_year = signif(quantile(data_user$tot_visits_per_yr, probs = (.75)), 4))
  
  cohort1_comp2_results_cost_output[["all_summary_statistics"]] <- tibble(mean_cost_visit_per_year=(sum(data_all$tot_visits)/sum(data_all$tot_exposed_yrs)),
                                                                          min_cost_visit_per_year = min(data_all$tot_visits_per_yr),
                                                                          max_cost_visit_per_year = max(data_all$tot_visits_per_yr),
                                                                          sd_cost_visit_per_year = signif(sd(data_all$tot_visits_per_yr),4),
                                                                          median_cost_visit_per_year = signif(quantile(data_all$tot_visits_per_yr, probs = (.5)), 4),
                                                                          lower_q_cost_visits_per_year = signif(quantile(data_all$tot_visits_per_yr, probs = (.25)), 4), 
                                                                          upper_q_cost_visits_per_year = signif(quantile(data_all$tot_visits_per_yr, probs = (.75)), 4))
  
  cohort1_comp2_results_cost_output[[names(cohort1_comp2_results_cost)[1]]] <-cohort1_comp2_results_cost_output[[names(cohort1_comp2_results_cost)[1]]] %>% 
    dplyr::mutate(tot_visits_costs = ifelse((num_subjects_visited<minimum_counts & num_subjects_visited>0), paste0("<", minimum_counts), round(as.numeric(.data$tot_visits_costs),1)),
                  mean_cost_visits_per_year = ifelse((num_subjects_visited<minimum_counts & num_subjects_visited>0), paste0("<", minimum_counts), round(as.numeric(.data$mean_cost_visits_per_year),1)),
                  sd_cost_visits_per_year = ifelse((num_subjects_visited<minimum_counts & num_subjects_visited>0), paste0("<", minimum_counts), round(as.numeric(.data$sd_cost_visits_per_year),1)),
                  min_cost_visits_per_year  = ifelse((num_subjects_visited<minimum_counts & num_subjects_visited>0), paste0("<", minimum_counts), round(as.numeric(.data$min_cost_visits_per_year),1)),
                  max_cost_visits_per_year = ifelse((num_subjects_visited<minimum_counts & num_subjects_visited>0), paste0("<", minimum_counts), round(as.numeric(.data$max_cost_visits_per_year), 1)), 
                  num_subjects_visited = ifelse((num_subjects_visited<minimum_counts & num_subjects_visited>0), paste0("<", minimum_counts), round(as.numeric(.data$num_subjects_visited),1)))
  
  cohort1_comp2_results_cost_output[[names(cohort1_comp2_results_cost)[2]]] <-cohort1_comp2_results_cost_output[[names(cohort1_comp2_results_cost)[2]]] %>% 
    dplyr::mutate(tot_visits_costs = ifelse((num_subjects_visited<minimum_counts & num_subjects_visited > 0), paste0("<", minimum_counts), round(as.numeric(.data$tot_visits_costs),1)),
                  mean_cost_visits_per_year = ifelse((num_subjects_visited < minimum_counts & num_subjects_visited > 0), paste0("<", minimum_counts), as.numeric(.data$mean_cost_visits_per_year)),
                  sd_cost_visits_per_year = ifelse((num_subjects_visited < minimum_counts & num_subjects_visited > 0), paste0("<", minimum_counts), as.numeric(.data$sd_cost_visits_per_year)),
                  min_cost_visits_per_year  = ifelse((num_subjects_visited < minimum_counts & num_subjects_visited > 0), paste0("<", minimum_counts), as.numeric(.data$min_cost_visits_per_year )),
                  max_cost_visits_per_year = ifelse((num_subjects_visited < minimum_counts & num_subjects_visited > 0), paste0("<", minimum_counts), as.numeric(.data$max_cost_visits_per_year)),
                  num_subjects_visited = ifelse((num_subjects_visited < minimum_counts & num_subjects_visited > 0), paste0("<", minimum_counts), as.numeric(.data$num_subjects_visited)))
  
  ####
  cohort2_results_cost_output <- list()
  for (name in (names(cohort2_results_cost)[1:length(cohort2_results_cost)])){
    cohort2_results_cost_output[[name]]<-cohort2_results_cost[[name]]
  }
  
  data_all <- cohort2_results_cost_output[["visits_cost_wide"]] %>% 
    dplyr::select(-exposed_yrs, -cohort) %>% 
    pivot_longer(cols = -c(subject_id, index_date, follow_up_end)) %>% 
    dplyr::group_by(subject_id, index_date, follow_up_end) %>% 
    dplyr::summarise(tot_visits = sum(value, na.rm = T)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(cohort = "cohort2",
                  tot_exposed_yrs = as.numeric((follow_up_end - index_date)/365.25),
                  tot_visits_per_yr = tot_visits/tot_exposed_yrs)
  
  data_user <- data_all %>% 
    dplyr::filter(tot_visits > 0)
  
  cohort2_results_cost_output[["user_only_summary_statistics"]] <- tibble(mean_cost_visit_per_year=(sum(data_user$tot_visits)/sum(data_user$tot_exposed_yrs)),
                                                                     min_cost_visit_per_year = min(data_user$tot_visits_per_yr),
                                                                     max_cost_visit_per_year = max(data_user$tot_visits_per_yr),
                                                                     sd_cost_visit_per_year = signif(sd(data_user$tot_visits_per_yr), 4),
                                                                     median_cost_visit_per_year = signif(quantile(data_user$tot_visits_per_yr, probs = (.5)), 4),
                                                                     lower_q_cost_visits_per_year = signif(quantile(data_user$tot_visits_per_yr, probs = (.25)), 4),
                                                                     upper_q_cost_visits_per_year = signif(quantile(data_user$tot_visits_per_yr, probs = (.75)), 4))
  
  cohort2_results_cost_output[["all_summary_statistics"]] <- tibble(mean_cost_visit_per_year=(sum(data_all$tot_visits)/sum(data_all$tot_exposed_yrs)),
                                                               min_cost_visit_per_year = min(data_all$tot_visits_per_yr),
                                                               max_cost_visit_per_year = max(data_all$tot_visits_per_yr),
                                                               sd_cost_visit_per_year = signif(sd(data_all$tot_visits_per_yr),4),
                                                               median_cost_visit_per_year = signif(quantile(data_all$tot_visits_per_yr, probs = (.5)), 4),
                                                               lower_q_cost_visits_per_year = signif(quantile(data_all$tot_visits_per_yr, probs = (.25)), 4), 
                                                               upper_q_cost_visits_per_year = signif(quantile(data_all$tot_visits_per_yr, probs = (.75)), 4))
  
  cohort2_results_cost_output[[names(cohort2_results_cost)[1]]] <-cohort2_results_cost_output[[names(cohort2_results_cost)[1]]] %>% 
    dplyr::mutate(tot_visits_costs = ifelse((num_subjects_visited<minimum_counts & num_subjects_visited>0), paste0("<", minimum_counts), round(as.numeric(.data$tot_visits_costs),1)),
                  mean_cost_visits_per_year = ifelse((num_subjects_visited<minimum_counts & num_subjects_visited>0), paste0("<", minimum_counts), round(as.numeric(.data$mean_cost_visits_per_year),1)),
                  sd_cost_visits_per_year = ifelse((num_subjects_visited<minimum_counts & num_subjects_visited>0), paste0("<", minimum_counts), round(as.numeric(.data$sd_cost_visits_per_year),1)),
                  min_cost_visits_per_year  = ifelse((num_subjects_visited<minimum_counts & num_subjects_visited>0), paste0("<", minimum_counts), round(as.numeric(.data$min_cost_visits_per_year),1)),
                  max_cost_visits_per_year = ifelse((num_subjects_visited<minimum_counts & num_subjects_visited>0), paste0("<", minimum_counts), round(as.numeric(.data$max_cost_visits_per_year), 1)), 
                  num_subjects_visited = ifelse((num_subjects_visited<minimum_counts & num_subjects_visited>0), paste0("<", minimum_counts), round(as.numeric(.data$num_subjects_visited),1)))
  
  cohort2_results_cost_output[[names(cohort2_results_cost)[2]]] <-cohort2_results_cost_output[[names(cohort2_results_cost)[2]]] %>% 
    dplyr::mutate(tot_visits_costs = ifelse((num_subjects_visited<minimum_counts & num_subjects_visited > 0), paste0("<", minimum_counts), round(as.numeric(.data$tot_visits_costs),1)),
                  mean_cost_visits_per_year = ifelse((num_subjects_visited < minimum_counts & num_subjects_visited > 0), paste0("<", minimum_counts), as.numeric(.data$mean_cost_visits_per_year)),
                  sd_cost_visits_per_year = ifelse((num_subjects_visited < minimum_counts & num_subjects_visited > 0), paste0("<", minimum_counts), as.numeric(.data$sd_cost_visits_per_year)),
                  min_cost_visits_per_year  = ifelse((num_subjects_visited < minimum_counts & num_subjects_visited > 0), paste0("<", minimum_counts), as.numeric(.data$min_cost_visits_per_year )),
                  max_cost_visits_per_year = ifelse((num_subjects_visited < minimum_counts & num_subjects_visited > 0), paste0("<", minimum_counts), as.numeric(.data$max_cost_visits_per_year)),
                  num_subjects_visited = ifelse((num_subjects_visited < minimum_counts & num_subjects_visited > 0), paste0("<", minimum_counts), as.numeric(.data$num_subjects_visited)))
  
  #HCRU
  write.xlsx(target_results_cost_output[-3], file = here(primary_costs_subfolder, "target_results_cost.xlsx"))
  write.xlsx(cohort1_comp1_results_cost_output[-3], file = here(primary_costs_subfolder, "cohort1_comp1_results_cost.xlsx"))
  write.xlsx(cohort1_comp2_results_cost_output[-3], file = here(primary_costs_subfolder, "cohort1_comp2_results_cost.xlsx"))
  write.xlsx(cohort2_results_cost_output[-3], file = here(primary_costs_subfolder, "cohort2_results_cost.xlsx"))
}

#summary
write.xlsx(summary_cohort_comp1, file = here(primary_resources_subfolder, "summary_cohort_comp1.xlsx"))
write.xlsx(summary_cohort_comp2, file = here(primary_resources_subfolder, "summary_cohort_comp2.xlsx"))

#Other 
if (exists("check_dates")){
  write.xlsx(check_dates, file = here(primary_resources_subfolder, "check_dates.xlsx"))
}
if (exists("visit_type_by_specialty")) {
  write.xlsx(visit_type_by_specialty, file = here(primary_resources_subfolder, "visit_type_by_specialty.xlsx"))
}

#Secondary Care
if (country_setting %in% c("Spain", "UK")){
  info(logger, "Starting secondary care")
  source(here("4_HealthEconomics", "secondary_care.R"))
  info(logger, "Finishing secondary care")
}

#secondary care and glm
if (country_setting %in% c("Spain", "UK")){
  info(logger, "Starting secondary care cost")
  source(here("4_HealthEconomics", "secondary_care_cost.R"))
  info(logger, "Finishing secondary care cost")
  
  # info(logger, "Starting general linear model")
  # source(here("4_HealthEconomics", "glm.R"))
  # info(logger, "Finishing general linear model")
}

info(logger, "OUTPUT FOR HEALTH ECONOMICS IS DONE")
