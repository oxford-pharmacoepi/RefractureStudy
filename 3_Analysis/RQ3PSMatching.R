suppressWarnings(
  rm(stratifiedCohort,
     cdm_char_imm,
     entryTable,
     fracture_table_rq2,
     fracture_table_rq2_back_up,
     fracture_table_rq2_index,
     imminentFractureCohortTotal,
     fracture_table,
     fracture_table_rq3_imminent,
     fracture_table_rq3,
     fracture_table_rq3_index,
     fracture_table_back_up,
     fracture_correction,
     fracture_table_rq2_index_ids)
)

psFolder <- here(sub_output_folder, "tempData")
if (!dir.exists(psFolder)) {
  dir.create(psFolder)
}

### Extracting features
info(logger, "EXTRACTING FEATURES")
print(paste0("Start extracting features at ", Sys.time()))
allSubjects <- tibble()

for (i in (1:length(targetCohort))){
  targetCohort[[i]] <- targetCohort[[i]] %>% dplyr::mutate(group = "target", period = i)
  compCohort1[[i]] <- compCohort1[[i]] %>% dplyr::mutate(group = "comparator 1", period = i)
  compCohort2[[i]] <- compCohort2[[i]] %>% dplyr::mutate(group = "comparator 2", period = i)
}
save(targetCohort, file = here(psFolder, "targetCohort.RData"))
save(compCohort1, file = here(psFolder, "compCohort1.RData"))
save(compCohort2, file = here(psFolder, "compCohort2.RData"))

for (i in (1:length(targetCohort))){
  allSubjects <- rbind(allSubjects,
                       targetCohort[[i]] %>% dplyr::select(subject_id, index_date, follow_up_end, group, period) %>% dplyr::distinct(),
                       compCohort1[[i]] %>% dplyr::select(subject_id, index_date, follow_up_end, group, period) %>% dplyr::distinct(),
                       compCohort2[[i]] %>% dplyr::select(subject_id, index_date, follow_up_end, group, period) %>% dplyr::distinct())
}

# feature_remove <- read_excel(here("FracturesCandidateCodes", "feature_remove.xlsx"))
# feature_remove_condition <- feature_remove %>% dplyr::filter(domain_id == "Condition") %>% dplyr::mutate(selected_covariates = as.integer(selected_covariates))
# feature_remove_procedure <- feature_remove %>% dplyr::filter(domain_id == "Procedure") %>% dplyr::mutate(selected_covariates = as.integer(selected_covariates))
# feature_remove_measurement <- feature_remove %>% dplyr::filter(domain_id == "Measurement") %>% dplyr::mutate(selected_covariates = as.integer(selected_covariates))
# feature_remove_drug <- feature_remove %>% dplyr::filter(domain_id == "Drug") %>% dplyr::mutate(selected_covariates = as.integer(selected_covariates))

# cdm[["condition_occurrence_2"]] <- cdm[["condition_occurrence"]] %>% 
#   dplyr::filter(!(condition_concept_id %in% any_fracture_id)) %>%
#   dplyr::anti_join(feature_remove_condition, by = c("condition_concept_id" = "selected_covariates"), copy = T) %>% 
#   CDMConnector::computeQuery()
#   
# cdm[["procedure_occurrence_2"]] <- cdm[["procedure_occurrence"]] %>% 
#   dplyr::anti_join(feature_remove_procedure, by = c("procedure_concept_id" = "selected_covariates"), copy = T) %>% 
#   CDMConnector::computeQuery()
# 
# cdm[["drug_era_2"]] <- cdm[["drug_era"]] %>% 
#   dplyr::anti_join(feature_remove_drug, by = c("drug_concept_id" = "selected_covariates"), copy = T) %>% 
#   CDMConnector::computeQuery()
# 
# cdm[["measurement_2"]] <- cdm[["measurement"]] %>% 
#   dplyr::anti_join(feature_remove_measurement, by = c("measurement_concept_id" = "selected_covariates"), copy = T) %>% 
#   CDMConnector::computeQuery()

cdm[["condition_occurrence_2"]] <- cdm[["condition_occurrence"]] %>%
  dplyr::filter(!(condition_concept_id %in% any_fracture_id)) %>%
  CDMConnector::computeQuery()

cdm[["features"]] <- cdm$condition_occurrence_2 %>%
  dplyr::filter(condition_concept_id != 0) %>% 
  dplyr::inner_join(allSubjects, by = c("person_id" = "subject_id"), copy = T) %>%
  dplyr::select(
    "subject_id" = "person_id",
    "index_date",
    "follow_up_end",
    "concept_id" = "condition_concept_id", 
    "date" = "condition_start_date"
  ) %>%
  dplyr::filter(date < index_date) %>%
  dplyr::mutate(dif_time = !!datediff("index_date", "date")) %>%
  dplyr::mutate(window = as.integer(if_else(dif_time >= -180, 1, if_else(dif_time >= -730, 2, 3)))) %>% # window 1: <180, 2: 180-730, 3: >730
  dplyr::mutate(feature = paste0("f", concept_id, "_", window)) %>%
  dplyr::select("subject_id", "follow_up_end", "index_date", "feature") %>%
  dplyr::distinct() %>%
  union_all(
    cdm$drug_era %>%
      dplyr::filter(drug_concept_id != 0) %>% 
      dplyr::inner_join(allSubjects, by = c("person_id" = "subject_id"), copy = T) %>%
      dplyr::select(
        "subject_id" = "person_id", 
        "index_date",
        "follow_up_end",
        "concept_id" = "drug_concept_id", 
        "date" = "drug_era_start_date"
      ) %>%
      dplyr::mutate(date = as.Date(date)) %>%
      dplyr::filter(date < index_date) %>%
      dplyr::mutate(dif_time = !!datediff("index_date", "date")) %>%
      dplyr::filter(dif_time >= -365) %>%
      dplyr::mutate(window = as.integer(if_else(dif_time >= -180, 1, if_else(dif_time >= -730, 2, 3)))) %>%
      dplyr::mutate(feature = paste0("f", concept_id, "_", window)) %>%
      dplyr::select("subject_id", "follow_up_end", "index_date", "feature") %>%
      dplyr::distinct()
  ) %>%
  union_all(
    cdm$procedure_occurrence %>%
      dplyr::filter(procedure_concept_id != 0) %>% 
      dplyr::inner_join(allSubjects, by = c("person_id" = "subject_id"), copy = T) %>%
      dplyr::select(
        "subject_id" = "person_id", 
        "index_date",
        "follow_up_end",
        "concept_id" = "procedure_concept_id", 
        "date" = "procedure_date"
      ) %>%
      dplyr::mutate(date = as.Date(date)) %>%
      dplyr::filter(date < index_date) %>%
      dplyr::mutate(dif_time = !!datediff("index_date", "date")) %>%
      dplyr::filter(dif_time >= -365) %>%
      dplyr::mutate(window = as.integer(if_else(dif_time >= -180, 1, if_else(dif_time >= -730, 2, 3)))) %>%
      dplyr::mutate(feature = paste0("f", concept_id, "_", window)) %>%
      dplyr::select("subject_id", "follow_up_end", "index_date", "feature") %>%
      dplyr::distinct()
  ) %>%
  union_all(
    cdm$measurement %>%
      dplyr::filter(measurement_concept_id != 0) %>% 
      dplyr::inner_join(allSubjects, by = c("person_id" = "subject_id"), copy = T) %>%
      dplyr::select(
        "subject_id" = "person_id", 
        "index_date",
        "follow_up_end",
        "concept_id" = "measurement_concept_id", 
        "date" = "measurement_date"
      ) %>%
      dplyr::mutate(date = as.Date(date)) %>%
      dplyr::filter(date < index_date) %>%
      dplyr::mutate(dif_time = !!datediff("index_date", "date")) %>%
      dplyr::filter(dif_time >= -365) %>%
      dplyr::mutate(window = as.integer(if_else(dif_time >= -180, 1, if_else(dif_time >= -730, 2, 3)))) %>%
      dplyr::mutate(feature = paste0("f", concept_id, "_", window)) %>%
      dplyr::select("subject_id", "follow_up_end", "index_date", "feature") %>%
      dplyr::distinct()
  ) %>%
  dplyr::compute()
print(paste0("Finish extracting features at ", Sys.time()))

features_count <- cdm[["features"]] %>% 
  dplyr::group_by(feature) %>%
  dplyr::tally()

features_count_threshold <- features_count %>%
  dplyr::filter(n<as.integer(denom_count)/200)

print(paste0("Creating subfeatures at ", Sys.time()))
cdm[["subfeatures"]] <- cdm[["features"]] %>% 
  dplyr::anti_join(features_count_threshold, by = "feature") %>% 
  CDMConnector::computeQuery()

counts <- cdm[["subfeatures"]] %>% dplyr::select(feature) %>% dplyr::distinct(feature) %>% tally() %>% collect()
counts <- counts %>% dplyr::rename(subfeatures_count = n)
write.xlsx(counts, file = here(sub_output_folder, "counts_subfeatures.xlsx"))
rm(counts)

# save(features, file = here(psFolder, "features.RData"))
# save(subfeatures, file = here(psFolder, "subfeatures.RData"))
# rm(features)
# rm(subfeatures)
# rm(features_count, features_count_threshold)
print(paste0("Finishing subfeatures at ", Sys.time()))
info(logger, "FEATURE EXTRACTION IS DONE")

### Using Patient Profiles and pre-defined functions
info(logger, "COMPUTING OTHER DEMOGRAPHICS")
cdm[["all_subjects"]] <- cdm[["denominator"]] %>%
  dplyr::mutate(cohort_start_date = as.Date(as.character(cohort_start_date)),
         cohort_end_date = as.Date(as.character(cohort_end_date))) %>%
  dplyr::inner_join(allSubjects, by = "subject_id", copy = T) %>%
  dplyr::select(cohort_definition_id, subject_id, index_date, group, period) %>% 
  dplyr::rename(cohort_start_date = index_date) %>%
  dplyr::mutate(cohort_end_date = cohort_start_date) %>%
  CDMConnector::computeQuery()

rm(allSubjects)

print(paste0("Extracting age and prior obs at ", Sys.time()))
allSubjectsCohort <- 
  cdm[["all_subjects"]] %>% 
  addAge() %>% 
  addPriorObservation()

print(paste0("Extracting the number of visits at ", Sys.time()))
allSubjectsCohort <- allSubjectsCohort %>%
  addIntersect(
    tableName = "visit_occurrence",
    value = "count",
    window = list(c(-Inf, -731), c(-730, -181), c(-180, -1)),
    nameStyle = "number_visits_{window_name}"
  ) %>%
  dplyr::collect()

save(allSubjectsCohort, file = here(psFolder, "allSubjectsCohort.RData"))
rm(allSubjectsCohort)
info(logger, "COMPUTING OTHER DEMOGRAPHICS IS DONE")

################################################################
#lasso regression, ps and matching between target and comp cohort 1
info(logger, "START MATCHING BETWEEN TARGET AND COMPARATOR COHORT 1")
print(paste0("Starting matching C1-T at ", Sys.time()))
lasso_reg_01 <- list()
selectedLassoFeatures01 <- list()
match_results_01 <- list()
subclasses01 <- list()
summary01 <- list()
load(here(psFolder, "targetCohort.RData"))
load(here(psFolder, "compCohort1.RData"))
load(here(psFolder, "compCohort2.RData"))

for (l in (1:length(targetCohort))){
  print(paste0("Starting matching C1-T for period ", l, " at ", Sys.time()))
  set.seed(12345)
  print(paste0("Pulling the relevant subfeatures at ", Sys.time()))
  # load(here(psFolder, "subfeatures.RData"))
  subfeatures_01 <- cdm[["subfeatures"]] %>% 
    dplyr::inner_join(rbind(targetCohort[[l]] %>% dplyr::select(subject_id, index_date, group), 
                     compCohort1[[l]] %>% dplyr::select(subject_id, index_date, group)),
               by = c("subject_id", "index_date"),
               copy = T) %>% 
    dplyr::collect()
  # rm(subfeatures)
  
  subfeatures_01 <- lowerBoundLasso01(subfeatures_01, 50)
  
  features_lasso01 <- subfeatures_01 %>% 
    dplyr::distinct() %>%
    dplyr::mutate(value = 1) %>%
    pivot_wider(names_from = "feature", values_from =  "value", values_fill = 0)
  
  rm(subfeatures_01)
  load(here(psFolder, "allSubjectsCohort.RData"))
  
  features_lasso01 <- allSubjectsCohort %>% 
    dplyr::filter(period == l) %>%
    dplyr::select(-"period") %>%
    dplyr::filter(group %in% c("comparator 1", "target")) %>%
    dplyr::inner_join(features_lasso01, by = c("subject_id", "group", "cohort_start_date"="index_date"), relationship = "many-to-many")
  
  features_lasso01$prior_observation <- as.double(features_lasso01$prior_observation)
  features_lasso01 <- features_lasso01 %>% 
    dplyr::mutate(group = factor(group, c("comparator 1", "target"))) %>% 
    dplyr::rename(index_date = cohort_start_date) %>% 
    dplyr::select(-cohort_end_date, -cohort_definition_id)
  
  print(paste0("Producing lasso_reg at ", Sys.time()))
  x <- data.matrix(features_lasso01 %>% dplyr::select(-c("group", "subject_id", "index_date", "follow_up_end")))
  y <- features_lasso01$group
  y<-as.integer(y)
  lambdas <- 10^seq(2, -3, by = -.1)
  lasso_reg_01[[l]] <- cv.glmnet(x, y, lambda = lambdas, standardize = TRUE, nfolds = 5, family = "binomial", alpha = 1)
  rm(x,y)
  
  print(paste0("Producing selectedLassoFeatures at ", Sys.time()))
  coef.lasso_reg <- coef(lasso_reg_01[[l]], s = lasso_reg_01[[l]]$lambda.1se)
  selectedLassoFeatures01[[l]] <- c("age", names(coef.lasso_reg[(coef.lasso_reg[,1]!=0),1])) %>% unique()
  selectedLassoFeatures01[[l]] <- selectedLassoFeatures01[[l]][selectedLassoFeatures01[[l]] != "(Intercept)"]
  
  features_lasso01 <- features_lasso01 %>%
    dplyr::select(all_of(c("subject_id", "group", "follow_up_end", "index_date", selectedLassoFeatures01[[l]])))
  
  print(paste0("Producing matched_results at ", Sys.time()))
  match_results_01[[l]] <- matchit(group ~ . -subject_id -follow_up_end -index_date, 
                                   data=features_lasso01,
                                   antiexact = ~subject_id,
                                   method="nearest", 
                                   caliper= c(0.20, age = 5), 
                                   std.caliper = c(TRUE, FALSE),
                                   ratio=5)
  
  print(paste0("Producing subclasses at ", Sys.time()))
  subclasses01[[l]] <- match.data(match_results_01[[l]])
  print(paste0("Producing summary at ", Sys.time()))
  summary01[[l]] <- summary(match_results_01[[l]])
  rm(allSubjectsCohort, features_lasso01)
}

save(lasso_reg_01, file = here(psFolder, "lasso_reg_01.RData"))
rm(lasso_reg_01)
save(selectedLassoFeatures01, file = here(psFolder, "selectedLassoFeatures01.RData"))
rm(selectedLassoFeatures01)
save(match_results_01, file = here(psFolder, "match_results_01.RData"))
rm(match_results_01)
save(subclasses01, file = here(psFolder, "subclasses01.RData"))
rm(subclasses01)
save(summary01, file = here(psFolder, "summary01.RData"))
rm(summary01)
rm(coef.lasso_reg)
gc()
info(logger, "MATCHING BETWEEN TARGET AND COMPARATOR COHORT 1 IS DONE")

#lasso regression, ps and matching between comp cohort 1 and 2
info(logger, "START MATCHING BETWEEN COMPARATOR COHORT 1 AND COMPARATOR COHORT 2")
print(paste0("Starting matching between C2-C1 at ", Sys.time()))
lasso_reg_12 <- list()
selectedLassoFeatures12 <- list()
match_results_12 <- list()
subclasses12 <- list()
summary12 <- list()
load(here(psFolder, "targetCohort.RData"))
load(here(psFolder, "compCohort1.RData"))
load(here(psFolder, "compCohort2.RData"))

for (l in (1:length(compCohort1))){
  print(paste0("Starting matching C2-C1 for period ", l, " at ", Sys.time()))
  set.seed(12345)
  print(paste0("Pulling relevant subfeatures at ", Sys.time()))
  #load(here(psFolder, "subfeatures.RData"))
  subfeatures_12 <- cdm[["subfeatures"]] %>% 
    dplyr::inner_join(rbind(compCohort1[[l]] %>% dplyr::select(subject_id, index_date, group), 
                     compCohort2[[l]] %>% dplyr::select(subject_id, index_date, group)),
               by = c("subject_id", "index_date"),
               copy = T) %>% 
    dplyr::collect()
  # rm(subfeatures)
  
  subfeatures_12 <- lowerBoundLasso12(subfeatures_12, 50)
  
  features_lasso12 <- subfeatures_12 %>% 
    dplyr::distinct() %>%
    dplyr::mutate(value = 1) %>%
    pivot_wider(names_from = "feature", values_from =  "value", values_fill = 0)
  
  rm(subfeatures_12)
  load(here(psFolder, "allSubjectsCohort.RData"))
  
  features_lasso12 <- allSubjectsCohort %>% 
    dplyr::filter(period == l) %>%
    dplyr::select(-"period") %>%
    dplyr::filter(group %in% c("comparator 2", "comparator 1")) %>%
    dplyr::inner_join(features_lasso12, by = c("subject_id", "group", "cohort_start_date"="index_date"), relationship = "many-to-many")
  
  features_lasso12$prior_observation <- as.double(features_lasso12$prior_observation)
  features_lasso12 <- features_lasso12 %>% 
    dplyr::mutate(group = factor(group, c("comparator 2", "comparator 1"))) %>% 
    dplyr::rename(index_date = cohort_start_date) %>% 
    dplyr::select(-cohort_end_date, -cohort_definition_id)
  
  print(paste0("Producing lasso_reg at ", Sys.time()))
  x <- data.matrix(features_lasso12 %>% dplyr::select(-c("group", "subject_id", "index_date", "follow_up_end")))
  y <- features_lasso12$group
  y<-as.integer(y)
  lambdas <- 10^seq(2, -3, by = -.1)
  lasso_reg_12[[l]] <- cv.glmnet(x, y, lambda = lambdas, standardize = TRUE, nfolds = 5, family = "binomial", alpha = 1)
  rm(x,y)
  
  print(paste0("Producing selectedLassoFeatures at ", Sys.time()))
  coef.lasso_reg <- coef(lasso_reg_12[[l]], s = lasso_reg_12[[l]]$lambda.1se)
  selectedLassoFeatures12[[l]] <- c("age", names(coef.lasso_reg[(coef.lasso_reg[,1]!=0),1])) %>% unique()
  selectedLassoFeatures12[[l]] <- selectedLassoFeatures12[[l]][selectedLassoFeatures12[[l]] != "(Intercept)"]
  
  features_lasso12 <- features_lasso12 %>%
    dplyr::select(all_of(c("subject_id", "group", "index_date", "follow_up_end", selectedLassoFeatures12[[l]])))
  
  print(paste0("Producing match_results at ", Sys.time()))
  match_results_12[[l]] <- matchit(group ~ .-subject_id -index_date -follow_up_end, 
                                   data=features_lasso12,
                                   antiexact = ~subject_id,
                                   method="nearest", 
                                   caliper= c(0.20, age = 5), 
                                   std.caliper = c(TRUE, FALSE),
                                   ratio=5)
  
  print(paste0("Producing subclasses at ", Sys.time()))
  subclasses12[[l]] <- match.data(match_results_12[[l]])
  print(paste0("Producing summary at ", Sys.time()))
  summary12[[l]] <- summary(match_results_12[[l]])
  rm(allSubjectsCohort, features_lasso12)
}

save(lasso_reg_12, file = here(psFolder, "lasso_reg_12.RData"))
rm(lasso_reg_12)
save(selectedLassoFeatures12, file = here(psFolder, "selectedLassoFeatures12.RData"))
rm(selectedLassoFeatures12)
save(match_results_12, file = here(psFolder, "match_results_12.RData"))
rm(match_results_12)
save(subclasses12, file = here(psFolder, "subclasses12.RData"))
rm(subclasses12)
save(summary12, file = here(psFolder, "summary12.RData"))
rm(summary12)
rm(compCohort1, compCohort2, targetCohort, coef.lasso_reg)
gc()

####
info(logger, "EXTRACTING OUTPUTS")

print(paste0("Extracting covarites summary at ", Sys.time()))
source(here("Miscellaneous", "SelectedCovariates.R"))
print(paste0("Extracting covarites summary is done at ", Sys.time()))

print(paste0("Extracting other output summary at ", Sys.time()))
source(here("Miscellaneous", "OtherOutputRQ3_Epi.R"))
print(paste0("Extracting other output summary is done at ", Sys.time()))

print(paste0("Extracting table ones at ", Sys.time()))
source(here("Miscellaneous", "RQ3_table_one.R"))
print(paste0("Extracting table ones is done at ", Sys.time()))
info(logger, "MATCHING BETWEEN COMPARATOR COHORT 1 AND COMPARATOR COHORT 2 IS DONE")
