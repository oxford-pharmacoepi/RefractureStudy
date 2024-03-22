included_cols <- colnames(target_results_cost$visits_cost_wide)[(colnames(target_results_cost$visits_cost_wide)%in% specialty_names)]
target_glm_01 <- target_results_cost$visits_cost_wide %>%
  dplyr::mutate(exposed_yrs = as.numeric(follow_up_end - index_date)/ 365.25) %>% 
  pivot_longer(all_of(included_cols), names_to = "specialty", values_to = "visits_costs") %>% 
  dplyr::mutate(visits_costs_per_year = visits_costs / exposed_yrs) %>% 
  dplyr::group_by(subject_id, index_date) %>% 
  dplyr::summarise(visits_costs_per_year_per_person = sum(visits_costs_per_year), 
                   .groups = "drop") %>% 
  replace(is.na(.), 0)

included_cols <- colnames(cohort1_comp1_results_cost$visits_cost_wide)[(colnames(cohort1_comp1_results_cost$visits_cost_wide)%in% specialty_names)]
c1_comparison1_glm_01 <- cohort1_comp1_results_cost$visits_cost_wide %>%
  dplyr::mutate(exposed_yrs = as.numeric(follow_up_end - index_date)/ 365.25) %>% 
  pivot_longer(all_of(included_cols), names_to = "specialty", values_to = "visits_costs") %>% 
  dplyr::mutate(visits_costs_per_year = visits_costs / exposed_yrs) %>% 
  dplyr::group_by(subject_id, index_date) %>% 
  dplyr::summarise(visits_costs_per_year_per_person = sum(visits_costs_per_year), 
                   .groups = "drop")

glm_bind <- rbind(target_glm_01, c1_comparison1_glm_01)

cdm_char01[["after_matching_01_cohort"]] <- cdm_char01[["after_matching_01_cohort"]] %>% 
  dplyr::left_join(glm_bind, by = c("subject_id", c("cohort_start_date" = "index_date")), copy = T) %>% 
  replace(is.na(.), 0) %>% 
  dplyr::compute()

rm(glm_bind, included_cols, target_glm_01, c1_comparison1_glm_01)

########################################################################################
included_cols <- colnames(cohort1_comp2_results_cost$visits_cost_wide)[(colnames(cohort1_comp2_results_cost$visits_cost_wide)%in% specialty_names)]
c1_comparison2_glm_01 <- cohort1_comp2_results_cost$visits_cost_wide %>%
  dplyr::mutate(exposed_yrs = as.numeric(follow_up_end - index_date)/ 365.25) %>% 
  pivot_longer(all_of(included_cols), names_to = "specialty", values_to = "visits_costs") %>% 
  dplyr::mutate(visits_costs_per_year = visits_costs / exposed_yrs) %>% 
  dplyr::group_by(subject_id, index_date) %>% 
  dplyr::summarise(visits_costs_per_year_per_person = sum(visits_costs_per_year), 
                   .groups = "drop") %>% 
  replace(is.na(.), 0)

included_cols <- colnames(cohort2_results_cost$visits_cost_wide)[(colnames(cohort2_results_cost$visits_cost_wide)%in% specialty_names)]
c2_glm_01 <- cohort2_results_cost$visits_cost_wide %>%
  dplyr::mutate(exposed_yrs = as.numeric(follow_up_end - index_date)/ 365.25) %>% 
  pivot_longer(all_of(included_cols), names_to = "specialty", values_to = "visits_costs") %>% 
  dplyr::mutate(visits_costs_per_year = visits_costs / exposed_yrs) %>% 
  dplyr::group_by(subject_id, index_date) %>% 
  dplyr::summarise(visits_costs_per_year_per_person = sum(visits_costs_per_year), 
                   .groups = "drop")

glm_bind <- rbind(c1_comparison2_glm_01, c2_glm_01)

cdm_char12[["after_matching_12_cohort"]] <- cdm_char12[["after_matching_12_cohort"]] %>% 
  dplyr::left_join(glm_bind, by = c("subject_id", c("cohort_start_date" = "index_date")), copy = T) %>% 
  replace(is.na(.), 0) %>% 
  dplyr::compute()

