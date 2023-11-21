load("~/RefractureStudy/Results_AURUM/30/tempData/selectedLassoFeatures01.RData")
selected30 <- selectedLassoFeatures01
load("~/RefractureStudy/Results_AURUM/90/tempData/selectedLassoFeatures01.RData")
selected90 <- selectedLassoFeatures01

selectedCovs30 <- list()
selectedCovs90 <- list()

for (i in (1:length(selected30))){
  selectedCovs30[[i]] <- as.data.frame(selected30[[i]]) %>%
    dplyr::mutate(period = i) %>%
    dplyr::rename(selected_covariates = 'selected30[[i]]') %>%
    dplyr::filter(!(selected_covariates %in%(c("age", "number_visits_m180_to_m1", "number_visits_m730_to_m181", "number_visits_minf_to_m731"))))
}
selected30 <- Reduce(union_all, selectedCovs30)
selected30[["selected_covariates"]] <- substr(selected30$selected_covariates, 2, nchar(selected30$selected_covariates)-2)
write.xlsx(selected30, file = here::here(output_folder, "01selected30.xlsx"))

for (i in (1:length(selected90))){
  selectedCovs90[[i]] <- as.data.frame(selected90[[i]]) %>%
    dplyr::mutate(period = i) %>%
    dplyr::rename(selected_covariates = 'selected90[[i]]') %>%
    dplyr::filter(!(selected_covariates %in%(c("age", "number_visits_m180_to_m1", "number_visits_m730_to_m181", "number_visits_minf_to_m731"))))
}
selected90 <- Reduce(union_all, selectedCovs90)
selected90[["selected_covariates"]] <- substr(selected90$selected_covariates, 2, nchar(selected90$selected_covariates)-2)
write.xlsx(selected90, file = here::here(output_folder, "01selected90.xlsx"))

load("~/RefractureStudy/Results_AURUM/30/tempData/selectedLassoFeatures12.RData")
selected30 <- selectedLassoFeatures12
load("~/RefractureStudy/Results_AURUM/90/tempData/selectedLassoFeatures12.RData")
selected90 <- selectedLassoFeatures12

selectedCovs30 <- list()
selectedCovs90 <- list()

for (i in (1:length(selected30))){
  selectedCovs30[[i]] <- as.data.frame(selected30[[i]]) %>%
    dplyr::mutate(period = i) %>%
    dplyr::rename(selected_covariates = 'selected30[[i]]') %>%
    dplyr::filter(!(selected_covariates %in%(c("age", "number_visits_m180_to_m1", "number_visits_m730_to_m181", "number_visits_minf_to_m731"))))
}
selected30 <- Reduce(union_all, selectedCovs30)
selected30[["selected_covariates"]] <- substr(selected30$selected_covariates, 2, nchar(selected30$selected_covariates)-2)
write.xlsx(selected30, file = here::here(output_folder, "12selected30.xlsx"))

for (i in (1:length(selected90))){
  selectedCovs90[[i]] <- as.data.frame(selected90[[i]]) %>%
    dplyr::mutate(period = i) %>%
    dplyr::rename(selected_covariates = 'selected90[[i]]') %>%
    dplyr::filter(!(selected_covariates %in%(c("age", "number_visits_m180_to_m1", "number_visits_m730_to_m181", "number_visits_minf_to_m731"))))
}
selected90 <- Reduce(union_all, selectedCovs90)
selected90[["selected_covariates"]] <- substr(selected90$selected_covariates, 2, nchar(selected90$selected_covariates)-2)
write.xlsx(selected90, file = here::here(output_folder, "12selected90.xlsx"))
