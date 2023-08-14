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
rm(allSubjectsCohort)

load(here(output_folder, "tempData", "features.RData"))
load(here(output_folder, "tempData", "allSubjectsCohort.RData"))

features_lasso <- features %>% 
  mutate(value = 1) %>%
  pivot_wider(names_from = "feature", values_from = "value", values_fill = 0) %>%
  inner_join(allSubjectsCohort %>% select(-c("cohort_definition_id", "cohort_end_date")), by = c("subject_id", "index_date" = "cohort_start_date"), copy = T) %>% 
  select(-"index_date")

features_lasso <- features_lasso %>% 
  mutate(groups = paste0(group," period ", period)) %>%
  select(-c("group", "period"))

features_lasso_test <- features_lasso %>% 
  filter(groups %in% c("target period 1", "comparator 1 period 1")) 

lasso_num <- features_lasso_test %>% summarise_all(n_distinct)
features_lasso_test<- features_lasso_test[,colnames(lasso_num[, (lasso_num[1, ] > 1)[1,]])]
features_lasso_test$prior_observation <- as.double(features_lasso_test$prior_observation)
features_lasso_test <- features_lasso_test %>%
  mutate(groups = factor(groups, c("target period 1", "comparator 1 period 1")))

features_lasso_test$groups <- as.double(features_lasso_test$groups)

features_lasso_test <- features_lasso_test %>% select(-subject_id)

x <- features_lasso_test %>% select(-groups)
y <- features_lasso_test$groups
y<-as.integer(y)
lambdas <- 10^seq(2, -3, by = -.1)
set.seed(1000)
lasso_reg <- cv.glmnet(x, y, family = "binomial")
