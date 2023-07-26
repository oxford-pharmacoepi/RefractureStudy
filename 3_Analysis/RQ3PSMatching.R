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

features <- cdm$condition_occurrence %>%
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

save(features, file = here(output_folder, "tempData", "features.RData"))
rm(features)
