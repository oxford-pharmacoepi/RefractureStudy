load(here(psFolder,"selectedLassoFeatures01.RData"))

selectedCovs01 <- list()
for (i in (1:length(selectedLassoFeatures01))){
  selectedCovs01[[i]] <- as.data.frame(selectedLassoFeatures01[[i]]) %>%
    dplyr::mutate(period = i) %>%
    dplyr::rename(selected_covariates = 'selectedLassoFeatures01[[i]]') %>%
    dplyr::filter(!(selected_covariates %in%(c("age", "number_visits_m180_to_m1", "number_visits_m730_to_m181", "number_visits_minf_to_m731", "prior_observation"))))
}
selected01 <- Reduce(union_all, selectedCovs01)
selected01[["selected_covariates"]] <- substr(selected01$selected_covariates, 2, nchar(selected01$selected_covariates)-2)

selected01 <- selected01 %>% 
  dplyr::mutate(selected_covariates = as.integer(selected_covariates)) %>% 
  dplyr::rename("concept_id" = "selected_covariates") %>% 
  left_join(tbl(db, sql(paste0(
    "SELECT * FROM ",
    cdm_database_schema,
    ".concept"
  ))),
  copy = T) %>%
  dplyr::select(
    "concept_id", "concept_name",
    "domain_id", "vocabulary_id", "period"
  ) %>%
  dplyr::collect() %>% 
  dplyr::rename("selected_covariates" = "concept_id")

other_covs01 <- list()
for (i in (1:length(selectedLassoFeatures01))){
  other_covs01[[i]] <- as.data.frame(selectedLassoFeatures01[[i]]) %>%
    dplyr::mutate(period = i) %>%
    dplyr::rename(selected_covariates = 'selectedLassoFeatures01[[i]]') %>%
    dplyr::filter(selected_covariates %in%(c("age", "number_visits_m180_to_m1", "number_visits_m730_to_m181", "number_visits_minf_to_m731", "prior_observation"))) %>% 
    dplyr::mutate(concept_name = NA,
                  domain_id = NA, 
                  vocabulary_id = NA)
}

for (i in (1:length(selectedLassoFeatures01))){
  selected01 <- rbind(selected01, other_covs01[[i]])
}

selected01 <- selected01 %>% dplyr::arrange(period)
write.xlsx(selected01, file = here::here(sub_output_folder, "01selected.xlsx"))
###
load(here(psFolder, "selectedLassoFeatures12.RData"))
selectedCovs12 <- list()

for (i in (1:length(selectedLassoFeatures12))){
  selectedCovs12[[i]] <- as.data.frame(selectedLassoFeatures12[[i]]) %>%
    dplyr::mutate(period = i) %>%
    dplyr::rename(selected_covariates = 'selectedLassoFeatures12[[i]]') %>%
    dplyr::filter(!(selected_covariates %in%(c("age", "number_visits_m180_to_m1", "number_visits_m730_to_m181", "number_visits_minf_to_m731", "prior_observation"))))
}

selected12 <- Reduce(union_all, selectedCovs12)
selected12[["selected_covariates"]] <- substr(selected12$selected_covariates, 2, nchar(selected12$selected_covariates)-2)

selected12 <- selected12 %>% 
dplyr::mutate(selected_covariates = as.integer(selected_covariates)) %>% 
  dplyr::rename("concept_id" = "selected_covariates") %>% 
  left_join(tbl(db, sql(paste0(
    "SELECT * FROM ",
    cdm_database_schema,
    ".concept"
  ))),
  copy = T) %>%
  dplyr::select(
    "concept_id", "concept_name",
    "domain_id", "vocabulary_id", "period"
  ) %>%
  dplyr::collect() %>% 
  dplyr::rename("selected_covariates" = "concept_id")

other_covs12 <- list()
for (i in (1:length(selectedLassoFeatures12))){
  other_covs12[[i]] <- as.data.frame(selectedLassoFeatures12[[i]]) %>%
    dplyr::mutate(period = i) %>%
    dplyr::rename(selected_covariates = 'selectedLassoFeatures12[[i]]') %>%
    dplyr::filter(selected_covariates %in%(c("age", "number_visits_m180_to_m1", "number_visits_m730_to_m181", "number_visits_minf_to_m731", "prior_observation"))) %>% 
    dplyr::mutate(concept_name = NA,
                  domain_id = NA, 
                  vocabulary_id = NA)
}

for (i in (1:length(selectedLassoFeatures12))){
  selected12 <- rbind(selected12, other_covs12[[i]])
}

selected12 <- selected12 %>% dplyr::arrange(period)
write.xlsx(selected12, file = here::here(sub_output_folder, "12selected.xlsx"))

###coeff
load(here(psFolder, "match_results_12.RData"))
load(here(psFolder, "match_results_01.RData"))
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
