psFolder <- here(output_folder, "tempData")
if (!dir.exists(psFolder)) {
  dir.create(psFolder)
}

### Extracting features
info(logger, "EXTRACTING FEATURES")
allSubjects <- tibble()

for (i in (1:length(targetCohort))){
  allSubjects <- rbind(allSubjects,
                       targetCohort[[i]] %>% select(subject_id, index_date) %>% distinct(),
                       compCohort1[[i]] %>% select(subject_id, index_date) %>% distinct(),
                       compCohort2[[i]] %>% select(subject_id, index_date) %>% distinct())
}

allSubjectsSample <- allSubjects %>% sample_frac(0.001) #sample because it takes too long

features <- cdm$condition_occurrence %>%
  inner_join(allSubjectsSample, by = c("person_id" = "subject_id"), copy = T) %>%
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
      inner_join(allSubjectsSample, by = c("person_id" = "subject_id"), copy = T) %>%
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
      inner_join(allSubjectsSample, by = c("person_id" = "subject_id"), copy = T) %>%
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
      inner_join(allSubjectsSample, by = c("person_id" = "subject_id"), copy = T) %>%
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

save(features, file = here(output_folder, "tempData", "features.RData"))
rm(features)

### Using Patient Profiles and pre-defined functions

cdm[["all_subjects"]] <- cdm[["denominator"]] %>%
  mutate(cohort_start_date = as.Date(as.character(cohort_start_date)),
         cohort_end_date = as.Date(as.character(cohort_end_date))) %>%
  inner_join(allSubjectsSample, by = "subject_id", copy = T) %>%
  select(cohort_definition_id, subject_id, index_date) %>% 
  rename(cohort_start_date = index_date) %>%
  mutate(cohort_end_date = cohort_start_date) %>%
  compute()

allSubjectsCohort <- 
  cdm[["all_subjects"]] %>% 
  addAge(cdm, ageGroup = list(
    c(50,54), c(55,59), c(60,64), c(65,69), c(70,74), c(75,79), c(80,84),
    c(85,89), c(90,150))) %>% 
  addPriorObservation()

allSubjectsCohort <- allSubjectsCohort %>%
  addIntersect(
    tableName = "visit_occurrence",
    value = "count",
    window = list(c(-Inf, -731), c(-730, -181), c(-181, -1)),
    nameStyle = "number_visits_{window_name}"
  ) 

save(allSubjectsCohort, file = here(output_folder, "tempData", "cohort.RData"))
rm(allSubjectsCohort)
