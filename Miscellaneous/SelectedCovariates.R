load(here(sub_output_folder, "tempData","selectedLassoFeatures01.RData"))

selectedCovs01 <- list()
for (i in (1:length(selectedLassoFeatures01))){
  selectedCovs01[[i]] <- as.data.frame(selectedLassoFeatures01[[i]]) %>%
    dplyr::mutate(period = i) %>%
    dplyr::rename(selected_covariates = 'selectedLassoFeatures01[[i]]') %>%
    dplyr::filter(!(selected_covariates %in%(c("age", "number_visits_m180_to_m1", "number_visits_m730_to_m181", "number_visits_minf_to_m731", "prior_observation"))))
}
selected01 <- Reduce(union_all, selectedCovs01)
selected01[["selected_covariates"]] <- substr(selected01$selected_covariates, 2, nchar(selected01$selected_covariates)-2)

other_covs01 <- list()
for (i in (1:length(selectedLassoFeatures01))){
  other_covs01[[i]] <- as.data.frame(selectedLassoFeatures01[[i]]) %>%
    dplyr::mutate(period = i) %>%
    dplyr::rename(selected_covariates = 'selectedLassoFeatures01[[i]]') %>%
    dplyr::filter(selected_covariates %in%(c("age", "number_visits_m180_to_m1", "number_visits_m730_to_m181", "number_visits_minf_to_m731", "prior_observation")))
}

for (i in (1:length(selectedLassoFeatures01))){
  selected01 <- rbind(selected01, other_covs01[[i]])
}

selected01 <- selected01 %>% dplyr::arrange(period)
write.xlsx(selected01, file = here::here(sub_output_folder, "01selected.xlsx"))
###
load(here(sub_output_folder, "tempData", "selectedLassoFeatures12.RData"))
selectedCovs12 <- list()

for (i in (1:length(selectedLassoFeatures12))){
  selectedCovs12[[i]] <- as.data.frame(selectedLassoFeatures12[[i]]) %>%
    dplyr::mutate(period = i) %>%
    dplyr::rename(selected_covariates = 'selectedLassoFeatures12[[i]]') %>%
    dplyr::filter(!(selected_covariates %in%(c("age", "number_visits_m180_to_m1", "number_visits_m730_to_m181", "number_visits_minf_to_m731", "prior_observation"))))
}

selected12 <- Reduce(union_all, selectedCovs12)
selected12[["selected_covariates"]] <- substr(selected12$selected_covariates, 2, nchar(selected12$selected_covariates)-2)

other_covs12 <- list()
for (i in (1:length(selectedLassoFeatures12))){
  other_covs12[[i]] <- as.data.frame(selectedLassoFeatures12[[i]]) %>%
    dplyr::mutate(period = i) %>%
    dplyr::rename(selected_covariates = 'selectedLassoFeatures12[[i]]') %>%
    dplyr::filter(selected_covariates %in%(c("age", "number_visits_m180_to_m1", "number_visits_m730_to_m181", "number_visits_minf_to_m731", "prior_observation")))
}

for (i in (1:length(selectedLassoFeatures12))){
  selected12 <- rbind(selected12, other_covs12[[i]])
}

selected12 <- selected12 %>% dplyr::arrange(period)
write.xlsx(selected12, file = here::here(sub_output_folder, "12selected.xlsx"))

###coeff
load(here(sub_output_folder, "tempData", "match_results_12.RData"))
load(here(sub_output_folder, "tempData", "match_results_01.RData"))
coefficients_ps_12 <- list()
for (i in (1:length(match_results_12))){
  coefficients_ps_12[[i]] <- match_results_12[[i]]$model$coefficients %>% 
    as.data.frame() %>% 
    mutate(period = i) %>% 
    rename(coef = '.')
  coefficients_ps_12[[i]]$cov <- row.names(coefficients_ps_12[[i]])
  coefficients_ps_12[[i]] <- coefficients_ps_12[[i]] %>% select(cov, coef, period)
}

coefficients_ps_01 <- list()
for (i in (1:length(match_results_01))){
  coefficients_ps_01[[i]] <- match_results_01[[i]]$model$coefficients %>% 
    as.data.frame() %>% 
    mutate(period = i) %>% 
    rename(coef = '.')
  coefficients_ps_01[[i]]$cov <- row.names(coefficients_ps_01[[i]])
  coefficients_ps_01[[i]] <- coefficients_ps_01[[i]] %>% select(cov, coef, period)
}

write.xlsx(coefficients_ps_01, file = here::here(sub_output_folder, "coefficients_ps_01.xlsx"))
write.xlsx(coefficients_ps_12, file = here::here(sub_output_folder, "coefficients_ps_12.xlsx"))

#################################
selected01_names <- selected01 %>% 
  dplyr::left_join(selected_covs %>% dplyr::mutate(Id = as.character(Id)), by = c("selected_covariates" = "Id"))
write.xlsx(selected01_names, file = here::here(sub_output_folder, "selected01_names.xlsx"))

selected12_names <- selected12 %>% 
  dplyr::left_join(selected_covs %>% dplyr::mutate(Id = as.character(Id)), by = c("selected_covariates" = "Id"))
write.xlsx(selected12_names, file = here::here(sub_output_folder, "selected12_names.xlsx"))