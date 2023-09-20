psFolder <- here(output_folder, "tempData")
if (!dir.exists(psFolder)) {
  dir.create(psFolder)
}

### Extracting features
info(logger, "EXTRACTING FEATURES")
allSubjects <- tibble()

for (i in (1:length(targetCohort))){
  targetCohort[[i]] <- targetCohort[[i]] %>% mutate(group = "target", period = i)
  compCohort1[[i]] <- compCohort1[[i]] %>% mutate(group = "comparator 1", period = i)
  compCohort2[[i]] <- compCohort2[[i]] %>% mutate(group = "comparator 2", period = i)
}

for (i in (1:length(targetCohort))){
  allSubjects <- rbind(allSubjects,
                       targetCohort[[i]] %>% select(subject_id, index_date, group, period) %>% distinct(),
                       compCohort1[[i]] %>% select(subject_id, index_date, group, period) %>% distinct(),
                       compCohort2[[i]] %>% select(subject_id, index_date, group, period) %>% distinct())
}

cdm[["condition_occurrence_2"]] <- cdm[["condition_occurrence"]] %>% 
  filter(!(condition_concept_id %in% any_fracture_id)) %>% compute()

features <- cdm$condition_occurrence_2 %>%
  inner_join(allSubjects, by = c("person_id" = "subject_id"), copy = T) %>%
  select(
    "subject_id" = "person_id",
    "index_date",
    "concept_id" = "condition_concept_id", 
    "date" = "condition_start_date"
  ) %>%
  filter(date < index_date) %>%
  mutate(dif_time = !!datediff("index_date", "date")) %>%
  mutate(window = as.integer(if_else(dif_time >= -180, 1, if_else(dif_time >= -730, 2, 3)))) %>% # window 1: <180, 2: 180-730, 3: >730
  mutate(feature = paste0("f", concept_id, "_", window)) %>%
  select("subject_id", "index_date", "feature") %>%
  distinct() %>%
  union_all(
    cdm$drug_era %>%
      inner_join(allSubjects, by = c("person_id" = "subject_id"), copy = T) %>%
      select(
        "subject_id" = "person_id", 
        "index_date",
        "concept_id" = "drug_concept_id", 
        "date" = "drug_era_start_date"
      ) %>%
      mutate(date = as.Date(date)) %>%
      filter(date < index_date) %>%
      mutate(dif_time = !!datediff("index_date", "date")) %>%
      filter(dif_time >= -365) %>%
      mutate(window = as.integer(if_else(dif_time >= -180, 1, if_else(dif_time >= -730, 2, 3)))) %>%
      mutate(feature = paste0("f", concept_id, "_", window)) %>%
      select("subject_id", "index_date", "feature") %>%
      distinct()
  ) %>%
  union_all(
    cdm$procedure_occurrence %>%
      inner_join(allSubjects, by = c("person_id" = "subject_id"), copy = T) %>%
      select(
        "subject_id" = "person_id", 
        "index_date",
        "concept_id" = "procedure_concept_id", 
        "date" = "procedure_date"
      ) %>%
      mutate(date = as.Date(date)) %>%
      filter(date < index_date) %>%
      mutate(dif_time = !!datediff("index_date", "date")) %>%
      filter(dif_time >= -365) %>%
      mutate(window = as.integer(if_else(dif_time >= -180, 1, if_else(dif_time >= -730, 2, 3)))) %>%
      mutate(feature = paste0("f", concept_id, "_", window)) %>%
      select("subject_id", "index_date", "feature") %>%
      distinct()
  ) %>%
  union_all(
    cdm$measurement %>%
      inner_join(allSubjects, by = c("person_id" = "subject_id"), copy = T) %>%
      select(
        "subject_id" = "person_id", 
        "index_date",
        "concept_id" = "measurement_concept_id", 
        "date" = "measurement_date"
      ) %>%
      mutate(date = as.Date(date)) %>%
      filter(date < index_date) %>%
      mutate(dif_time = !!datediff("index_date", "date")) %>%
      filter(dif_time >= -365) %>%
      mutate(window = as.integer(if_else(dif_time >= -180, 1, if_else(dif_time >= -730, 2, 3)))) %>%
      mutate(feature = paste0("f", concept_id, "_", window)) %>%
      select("subject_id", "index_date", "feature") %>%
      distinct()
  ) %>%
  collect()

features_count <- features %>% 
  group_by(feature) %>%
  tally()

features_count_threshold <- features_count %>%
  filter(n<as.integer(denom_count)/200)

subfeatures <- features %>% anti_join(features_count_threshold, by = "feature")

save(features, file = here(output_folder, "tempData", "features.RData"))
save(subfeatures, file = here(output_folder, "tempData", "subfeatures.RData"))
rm(features)
rm(subfeatures)
rm(features_count, features_count_threshold)

### Using Patient Profiles and pre-defined functions
cdm[["all_subjects"]] <- cdm[["denominator"]] %>%
  mutate(cohort_start_date = as.Date(as.character(cohort_start_date)),
         cohort_end_date = as.Date(as.character(cohort_end_date))) %>%
  inner_join(allSubjects, by = "subject_id", copy = T) %>%
  select(cohort_definition_id, subject_id, index_date, group, period) %>% 
  rename(cohort_start_date = index_date) %>%
  mutate(cohort_end_date = cohort_start_date) %>%
  compute()

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
  collect()

save(allSubjectsCohort, file = here(output_folder, "tempData", "allSubjectsCohort.RData"))
rm(allSubjectsCohort, denom)

################################################################
#lasso regression, ps and matching between target and comp cohort 1
lasso_reg_01 <- list()
selectedLassoFeatures01 <- list()
match_results_01 <- list()
subclasses01 <- list()
summary01 <- list()

for (i in (1:length(targetCohort))){
  set.seed(12345)
  load(here(output_folder, "tempData", "subfeatures.RData"))
  subfeatures_01 <- subfeatures %>% 
    inner_join(rbind(targetCohort[[i]] %>% select(subject_id, index_date), 
                     compCohort1[[i]] %>% select(subject_id, index_date)),
               by = c("subject_id", "index_date"))
  rm(subfeatures)
  
  subfeatures_01 <- lowerBoundLasso01(subfeatures_01, 50)
  
  features_lasso01 <- subfeatures_01 %>% 
    select(-"index_date") %>%
    distinct() %>%
    mutate(value = 1) %>%
    pivot_wider(names_from = "feature", values_from =  "value", values_fill = 0)
  
  rm(subfeatures_01)
  load(here(output_folder, "tempData", "allSubjectsCohort.RData"))
  
  features_lasso01 <- allSubjectsCohort %>% select(-c("cohort_definition_id", "cohort_end_date", "cohort_start_date")) %>%
    filter(period == i) %>%
    select(-"period") %>%
    filter(group %in% c("comparator 1", "target")) %>%
    inner_join(features_lasso01, by = "subject_id")
  
  features_lasso01$prior_observation <- as.double(features_lasso01$prior_observation)
  features_lasso01 <- features_lasso01 %>% 
    mutate(group = factor(group, c("comparator 1", "target")))
  
  x <- data.matrix(features_lasso01 %>% select(-c("group", "subject_id")))
  y <- features_lasso01$group
  y<-as.integer(y)
  lambdas <- 10^seq(2, -3, by = -.1)
  lasso_reg_01[[i]] <- cv.glmnet(x, y, lambda = lambdas, standardize = TRUE, nfolds = 5, family = "binomial", alpha = 1)
  rm(x,y)
  
  coef.lasso_reg <- coef(lasso_reg_01[[i]], s = lasso_reg_01[[i]]$lambda.1se)
  selectedLassoFeatures01[[i]] <- names(coef.lasso_reg[(coef.lasso_reg[,1]!=0),1])
  selectedLassoFeatures01[[i]] <- selectedLassoFeatures01[[i]][selectedLassoFeatures01[[i]] != "(Intercept)"]
  selectedLassoFeatures01[[i]] <- c("age", selectedLassoFeatures01[[i]]) %>% unique()
  
  features_lasso01 <- features_lasso01 %>%
    select(all_of(c("subject_id", "group", selectedLassoFeatures01[[i]])))
  
  match_results_01[[i]] <- matchit(group ~ . -subject_id, 
                                   data=features_lasso01,
                                   method="nearest", 
                                   caliper= c(0.20, age = 5), 
                                   std.caliper = c(TRUE, FALSE),
                                   ratio=5)
  subclasses01[[i]] <- match.data(match_results_01[[i]])
  summary01[[i]] <- summary(match_results_01[[i]])
}

save(lasso_reg_01, file = here(output_folder, "tempData", "lasso_reg_01.RData"))
rm(lasso_reg_01)
save(selectedLassoFeatures01, file = here(output_folder, "tempData", "selectedLassoFeatures01.RData"))
rm(selectedLassoFeatures01)
save(match_results_01, file = here(output_folder, "tempData", "match_results_01.RData"))
rm(match_results_01)
save(subclasses01, file = here(output_folder, "tempData", "subclasses01.RData"))
rm(subclasses01)
save(summary01, file = here(output_folder, "tempData", "summary01.RData"))
rm(summary01)

#lasso regression, ps and matching between comp cohort 1 and 2
lasso_reg_12 <- list()
selectedLassoFeatures12 <- list()
match_results_12 <- list()
subclasses12 <- list()
summary12 <- list()

for (i in (1:length(compCohort1))){
  set.seed(12345)
  load(here(output_folder, "tempData", "subfeatures.RData"))
  subfeatures_12 <- subfeatures %>% 
    inner_join(rbind(compCohort1[[i]] %>% select(subject_id, index_date), 
                     compCohort2[[i]] %>% select(subject_id, index_date)),
               by = c("subject_id", "index_date"))
  rm(subfeatures)
  
  subfeatures_12 <- lowerBoundLasso12(subfeatures_12, 50)
  
  features_lasso12 <- subfeatures_12 %>% 
    select(-"index_date") %>%
    distinct() %>%
    mutate(value = 1) %>%
    pivot_wider(names_from = "feature", values_from =  "value", values_fill = 0)
  
  rm(subfeatures_12)
  load(here(output_folder, "tempData", "allSubjectsCohort.RData"))
  
  features_lasso12 <- allSubjectsCohort %>% select(-c("cohort_definition_id", "cohort_end_date", "cohort_start_date")) %>%
    filter(period == i) %>%
    select(-"period") %>%
    filter(group %in% c("comparator 2", "comparator 1")) %>%
    inner_join(features_lasso12, by = "subject_id")
  
  features_lasso12$prior_observation <- as.double(features_lasso12$prior_observation)
  features_lasso12 <- features_lasso12 %>% 
    mutate(group = factor(group, c("comparator 2", "comparator 1")))
  
  x <- data.matrix(features_lasso12 %>% select(-c("group", "subject_id")))
  y <- features_lasso12$group
  y<-as.integer(y)
  lambdas <- 10^seq(2, -3, by = -.1)
  lasso_reg_12[[i]] <- cv.glmnet(x, y, lambda = lambdas, standardize = TRUE, nfolds = 5, family = "binomial", alpha = 1)
  rm(x,y)
  
  coef.lasso_reg <- coef(lasso_reg_12[[i]], s = lasso_reg_12[[i]]$lambda.1se)
  selectedLassoFeatures12[[i]] <- names(coef.lasso_reg[(coef.lasso_reg[,1]!=0),1])
  selectedLassoFeatures12[[i]] <- selectedLassoFeatures12[[i]][selectedLassoFeatures12[[i]] != "(Intercept)"]
  selectedLassoFeatures12[[i]] <- c("age", selectedLassoFeatures12[[i]]) %>% unique()
  
  features_lasso12 <- features_lasso12 %>%
    select(all_of(c("subject_id", "group", selectedLassoFeatures12[[i]])))
  
  match_results_12[[i]] <- matchit(group ~ .-subject_id, 
                                   data=features_lasso12,
                                   method="nearest", 
                                   caliper= c(0.20, age = 5), 
                                   std.caliper = c(TRUE, FALSE),
                                   ratio=5)
  subclasses12[[i]] <- match.data(match_results_12[[i]])
  summary12[[i]] <- summary(match_results_12[[i]])
}

save(lasso_reg_12, file = here(output_folder, "tempData", "lasso_reg_12.RData"))
rm(lasso_reg_12)
save(selectedLassoFeatures12, file = here(output_folder, "tempData", "selectedLassoFeatures12.RData"))
rm(selectedLassoFeatures12)
save(match_results_12, file = here(output_folder, "tempData", "match_results_12.RData"))
rm(match_results_12)
save(subclasses12, file = here(output_folder, "tempData", "subclasses12.RData"))
rm(subclasses12)
save(summary12, file = here(output_folder, "tempData", "summary12.RData"))
rm(summary12)
