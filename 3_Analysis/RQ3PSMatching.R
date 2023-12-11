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
allSubjects <- tibble()

for (i in (1:length(targetCohort))){
  targetCohort[[i]] <- targetCohort[[i]] %>% dplyr::mutate(group = "target", period = i)
  compCohort1[[i]] <- compCohort1[[i]] %>% dplyr::mutate(group = "comparator 1", period = i)
  compCohort2[[i]] <- compCohort2[[i]] %>% dplyr::mutate(group = "comparator 2", period = i)
}
save(targetCohort, file = here(sub_output_folder, "tempData", "targetCohort.RData"))
save(compCohort1, file = here(sub_output_folder, "tempData", "compCohort1.RData"))
save(compCohort2, file = here(sub_output_folder, "tempData", "compCohort2.RData"))

for (i in (1:length(targetCohort))){
  allSubjects <- rbind(allSubjects,
                       targetCohort[[i]] %>% dplyr::select(subject_id, index_date, follow_up_end, group, period) %>% dplyr::distinct(),
                       compCohort1[[i]] %>% dplyr::select(subject_id, index_date, follow_up_end, group, period) %>% dplyr::distinct(),
                       compCohort2[[i]] %>% dplyr::select(subject_id, index_date, follow_up_end, group, period) %>% dplyr::distinct())
}

cdm[["condition_occurrence_2"]] <- cdm[["condition_occurrence"]] %>% 
  dplyr::filter(!(condition_concept_id %in% any_fracture_id)) %>% 
  CDMConnector::computeQuery()

features <- cdm$condition_occurrence_2 %>%
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
  dplyr::collect()

features_count <- features %>% 
  dplyr::group_by(feature) %>%
  dplyr::tally()

features_count_threshold <- features_count %>%
  dplyr::filter(n<as.integer(denom_count)/200)

subfeatures <- features %>% dplyr::anti_join(features_count_threshold, by = "feature")

# save(features, file = here(sub_output_folder, "tempData", "features.RData"))
save(subfeatures, file = here(sub_output_folder, "tempData", "subfeatures.RData"))
rm(features)
rm(subfeatures)
rm(features_count, features_count_threshold)

### Using Patient Profiles and pre-defined functions
cdm[["all_subjects"]] <- cdm[["denominator"]] %>%
  dplyr::mutate(cohort_start_date = as.Date(as.character(cohort_start_date)),
         cohort_end_date = as.Date(as.character(cohort_end_date))) %>%
  dplyr::inner_join(allSubjects, by = "subject_id", copy = T) %>%
  dplyr::select(cohort_definition_id, subject_id, index_date, group, period) %>% 
  dplyr::rename(cohort_start_date = index_date) %>%
  dplyr::mutate(cohort_end_date = cohort_start_date) %>%
  CDMConnector::computeQuery()

rm(allSubjects)

allSubjectsCohort <- 
  cdm[["all_subjects"]] %>% 
  addAge() %>% 
  addPriorObservation()

allSubjectsCohort <- allSubjectsCohort %>%
  addIntersect(
    tableName = "visit_occurrence",
    value = "count",
    window = list(c(-Inf, -731), c(-730, -181), c(-180, -1)),
    nameStyle = "number_visits_{window_name}"
  ) %>%
  dplyr::collect()

save(allSubjectsCohort, file = here(sub_output_folder, "tempData", "allSubjectsCohort.RData"))
rm(allSubjectsCohort)

################################################################
#lasso regression, ps and matching between target and comp cohort 1
lasso_reg_01 <- list()
selectedLassoFeatures01 <- list()
match_results_01 <- list()
subclasses01 <- list()
summary01 <- list()
load(here(sub_output_folder, "tempData", "targetCohort.RData"))
load(here(sub_output_folder, "tempData", "compCohort1.RData"))
load(here(sub_output_folder, "tempData", "compCohort2.RData"))

for (l in (1:length(targetCohort))){
  set.seed(12345)
  load(here(sub_output_folder, "tempData", "subfeatures.RData"))
  subfeatures_01 <- subfeatures %>% 
    dplyr::inner_join(rbind(targetCohort[[l]] %>% dplyr::select(subject_id, index_date), 
                     compCohort1[[l]] %>% dplyr::select(subject_id, index_date)),
               by = c("subject_id", "index_date"))
  rm(subfeatures)
  
  subfeatures_01 <- lowerBoundLasso01(subfeatures_01, 50)
  
  features_lasso01 <- subfeatures_01 %>% 
    dplyr::distinct() %>%
    dplyr::mutate(value = 1) %>%
    pivot_wider(names_from = "feature", values_from =  "value", values_fill = 0)
  
  rm(subfeatures_01)
  load(here(sub_output_folder, "tempData", "allSubjectsCohort.RData"))
  
  features_lasso01 <- allSubjectsCohort %>% dplyr::select(-c("cohort_definition_id", "cohort_end_date", "cohort_start_date")) %>%
    dplyr::filter(period == l) %>%
    dplyr::select(-"period") %>%
    dplyr::filter(group %in% c("comparator 1", "target")) %>%
    dplyr::inner_join(features_lasso01, by = "subject_id", relationship = "many-to-many")
  
  features_lasso01$prior_observation <- as.double(features_lasso01$prior_observation)
  features_lasso01 <- features_lasso01 %>% 
    dplyr::mutate(group = factor(group, c("comparator 1", "target")))
  
  x <- data.matrix(features_lasso01 %>% dplyr::select(-c("group", "subject_id", "index_date", "follow_up_end")))
  y <- features_lasso01$group
  y<-as.integer(y)
  lambdas <- 10^seq(2, -3, by = -.1)
  lasso_reg_01[[l]] <- cv.glmnet(x, y, lambda = lambdas, standardize = TRUE, nfolds = 5, family = "binomial", alpha = 1)
  rm(x,y)
  
  coef.lasso_reg <- coef(lasso_reg_01[[l]], s = lasso_reg_01[[l]]$lambda.1se)
  selectedLassoFeatures01[[l]] <- names(coef.lasso_reg[(coef.lasso_reg[,1]!=0),1])
  selectedLassoFeatures01[[l]] <- selectedLassoFeatures01[[l]][selectedLassoFeatures01[[l]] != "(Intercept)"]
  selectedLassoFeatures01[[l]] <- c("age", selectedLassoFeatures01[[l]]) %>% unique()
  
  features_lasso01 <- features_lasso01 %>%
    dplyr::select(all_of(c("subject_id", "group", "follow_up_end", "index_date", selectedLassoFeatures01[[l]])))
  
  match_results_01[[l]] <- matchit(group ~ . -subject_id -follow_up_end -index_date, 
                                   data=features_lasso01,
                                   antiexact = ~subject_id,
                                   method="nearest", 
                                   caliper= c(0.20, age = 5), 
                                   std.caliper = c(TRUE, FALSE),
                                   ratio=5)
  subclasses01[[l]] <- match.data(match_results_01[[l]])
  summary01[[l]] <- summary(match_results_01[[l]])
  rm(allSubjectsCohort, features_lasso01)
}

save(lasso_reg_01, file = here(sub_output_folder, "tempData", "lasso_reg_01.RData"))
save(lasso_reg_01, file = here(sub_output_folder, "lasso_reg_01.RData"))
rm(lasso_reg_01)
save(selectedLassoFeatures01, file = here(sub_output_folder, "tempData", "selectedLassoFeatures01.RData"))
save(selectedLassoFeatures01, file = here(sub_output_folder, "selectedLassoFeatures01.RData"))
rm(selectedLassoFeatures01)
save(match_results_01, file = here(sub_output_folder, "tempData", "match_results_01.RData"))
rm(match_results_01)
save(subclasses01, file = here(sub_output_folder, "tempData", "subclasses01.RData"))
rm(subclasses01)
save(summary01, file = here(sub_output_folder, "tempData", "summary01.RData"))
save(summary01, file = here(sub_output_folder, "summary01.RData"))
rm(summary01)
rm(coef.lasso_reg)
gc()

#lasso regression, ps and matching between comp cohort 1 and 2
lasso_reg_12 <- list()
selectedLassoFeatures12 <- list()
match_results_12 <- list()
subclasses12 <- list()
summary12 <- list()

for (l in (1:length(compCohort1))){
  set.seed(12345)
  load(here(sub_output_folder, "tempData", "subfeatures.RData"))
  subfeatures_12 <- subfeatures %>% 
    dplyr::inner_join(rbind(compCohort1[[l]] %>% dplyr::select(subject_id, index_date), 
                     compCohort2[[l]] %>% dplyr::select(subject_id, index_date)),
               by = c("subject_id", "index_date"))
  rm(subfeatures)
  
  subfeatures_12 <- lowerBoundLasso12(subfeatures_12, 50)
  
  features_lasso12 <- subfeatures_12 %>% 
    dplyr::distinct() %>%
    dplyr::mutate(value = 1) %>%
    pivot_wider(names_from = "feature", values_from =  "value", values_fill = 0)
  
  rm(subfeatures_12)
  load(here(sub_output_folder, "tempData", "allSubjectsCohort.RData"))
  
  features_lasso12 <- allSubjectsCohort %>% dplyr::select(-c("cohort_definition_id", "cohort_end_date", "cohort_start_date")) %>%
    dplyr::filter(period == l) %>%
    dplyr::select(-"period") %>%
    dplyr::filter(group %in% c("comparator 2", "comparator 1")) %>%
    dplyr::inner_join(features_lasso12, by = "subject_id", relationship = "many-to-many")
  
  features_lasso12$prior_observation <- as.double(features_lasso12$prior_observation)
  features_lasso12 <- features_lasso12 %>% 
    dplyr::mutate(group = factor(group, c("comparator 2", "comparator 1")))
  
  x <- data.matrix(features_lasso12 %>% dplyr::select(-c("group", "subject_id", "index_date", "follow_up_end")))
  y <- features_lasso12$group
  y<-as.integer(y)
  lambdas <- 10^seq(2, -3, by = -.1)
  lasso_reg_12[[l]] <- cv.glmnet(x, y, lambda = lambdas, standardize = TRUE, nfolds = 5, family = "binomial", alpha = 1)
  rm(x,y)
  
  coef.lasso_reg <- coef(lasso_reg_12[[l]], s = lasso_reg_12[[l]]$lambda.1se)
  selectedLassoFeatures12[[l]] <- names(coef.lasso_reg[(coef.lasso_reg[,1]!=0),1])
  selectedLassoFeatures12[[l]] <- selectedLassoFeatures12[[l]][selectedLassoFeatures12[[l]] != "(Intercept)"]
  selectedLassoFeatures12[[l]] <- c("age", selectedLassoFeatures12[[l]]) %>% unique()
  
  features_lasso12 <- features_lasso12 %>%
    dplyr::select(all_of(c("subject_id", "group", "index_date", "follow_up_end", selectedLassoFeatures12[[l]])))
  
  match_results_12[[l]] <- matchit(group ~ .-subject_id -index_date -follow_up_end, 
                                   data=features_lasso12,
                                   antiexact = ~subject_id,
                                   method="nearest", 
                                   caliper= c(0.20, age = 5), 
                                   std.caliper = c(TRUE, FALSE),
                                   ratio=5)
  subclasses12[[l]] <- match.data(match_results_12[[l]])
  summary12[[l]] <- summary(match_results_12[[l]])
  rm(allSubjectsCohort, features_lasso12)
}

save(lasso_reg_12, file = here(sub_output_folder, "tempData", "lasso_reg_12.RData"))
save(lasso_reg_12, file = here(sub_output_folder, "lasso_reg_12.RData"))
rm(lasso_reg_12)
save(selectedLassoFeatures12, file = here(sub_output_folder, "tempData", "selectedLassoFeatures12.RData"))
save(selectedLassoFeatures12, file = here(sub_output_folder, "selectedLassoFeatures12.RData"))
rm(selectedLassoFeatures12)
save(match_results_12, file = here(sub_output_folder, "tempData", "match_results_12.RData"))
rm(match_results_12)
save(subclasses12, file = here(sub_output_folder, "tempData", "subclasses12.RData"))
rm(subclasses12)
save(summary12, file = here(sub_output_folder, "tempData", "summary12.RData"))
save(summary12, file = here(sub_output_folder, "summary12.RData"))
rm(summary12)
rm(compCohort1, compCohort2, targetCohort, coef.lasso_reg)
gc()
