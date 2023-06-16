# for a fracture table, compute the index date for each person
addIndex <- function (fractureTable){
  fractureTable %>%
    right_join(fractureTable %>% 
                 filter (condition_start_date >= study_start_date) %>%
                 filter (condition_start_date <= study_end_date) %>% 
                 group_by (subject_id) %>% 
                 summarise (index_date = min(condition_start_date, na.rm = T)), by = "subject_id")
}

# for a fracture table, removing individuals with index fracture on the same day as a record of death
noDeathOnIndex <- function (fractureTable){
  fractureTable %>% anti_join(cdm[["death"]], by = c("subject_id" = "person_id", "index_date" = "death_date"), copy = T)
}

# for a fracture table, removing individuals with cancer before or on the same day as the index fracture
noCancerPriorOrOnIndex <- function (fractureTable){
  fractureTable %>% 
    anti_join(fractureTable %>% 
                inner_join(cdm[[exclusionCohortTableName]] %>% 
                             filter(cohort_definition_id == cancerId), by = "subject_id", copy = T, relationship = "many-to-many") %>%
                filter(cohort_start_date<=index_date), by = colnames(fractureTable))
}

# for a fracture table, removing individuals with bone disease before or on the same day as the index fracture
noBoneDiseasePriorOrOnIndex <- function (fractureTable){
  fractureTable %>% 
    anti_join(fractureTable %>% 
                inner_join(cdm[[exclusionCohortTableName]] %>% 
                             filter(cohort_definition_id == BoneDiseaseId), by = "subject_id", copy = T, relationship = "many-to-many") %>%
                filter(cohort_start_date<=index_date), by = colnames(fractureTable))
}

# add an extra variable after_index which is two years after the index date
addInTwoYearsAfter <- function (fractureTable){
  fractureTable %>%
    mutate(after_index = index_date + 730)
}

# add in the end of observation period for each individual
addInObsEndDate <- function (fractureTable){
  fractureTable %>% 
    left_join(cdm[["observation_period"]], by = c("subject_id" = "person_id"), copy = T) %>%
    select(subject_id, condition_concept_id, condition_start_date, fracture_site, index_date, after_index, observation_period_end_date)
}

# add in a column indicating the date of cancer after the index date
addInCancerPostIndex <- function (fractureTable){
  fractureTable %>% left_join(fractureTable %>% 
                                inner_join(cdm[[exclusionCohortTableName]] %>% 
                                             filter(cohort_definition_id == cancerId), 
                                           by = "subject_id", 
                                           copy = T, 
                                           relationship = "many-to-many") %>%
                                filter(index_date < cohort_start_date) %>%   
                                group_by(subject_id, condition_concept_id, condition_start_date, fracture_site, index_date, observation_period_end_date) %>%
                                arrange(cohort_start_date) %>%
                                filter(row_number()==1) %>%
                                ungroup(), by = c("subject_id", "condition_concept_id", "condition_start_date", "fracture_site", "index_date", "after_index", "observation_period_end_date")) %>%
    select(-cohort_definition_id, - cohort_end_date) %>%
    rename(cancer_date_after_index = cohort_start_date)
}

# add in a column indicating the date of bone disease after the index date (has to be done after the cancer one)
addInBoneDiseasePostIndex <- function (fractureTable){
  fractureTable %>% left_join(fractureTable %>% 
                                inner_join(cdm[[exclusionCohortTableName]] %>% 
                                             filter(cohort_definition_id == BoneDiseaseId), 
                                           by = "subject_id", 
                                           copy = T, 
                                           relationship = "many-to-many") %>%
                                filter(index_date < cohort_start_date) %>%   
                                group_by(subject_id, condition_concept_id, condition_start_date, fracture_site, index_date, observation_period_end_date) %>%
                                arrange(cohort_start_date) %>%
                                filter(row_number()==1) %>%
                                ungroup(), by = c("subject_id", "condition_concept_id", "condition_start_date", "fracture_site", "index_date", "after_index", "observation_period_end_date", "cancer_date_after_index")) %>%
    select(-cohort_definition_id, - cohort_end_date) %>%
    rename(bone_disease_date_after_index = cohort_start_date)
}

# add a column of next fracture after the index one
addInNextFracture <- function(fractureTable){
  fractureTable %>% left_join(fractureTable %>% group_by(subject_id) %>% filter(condition_start_date> index_date) %>% summarise(fracture_after_index = min(condition_start_date, na.rm =  T)),
                              by = "subject_id")
}

# add a column of death record date after the index date
addInDeath <- function(fractureTable){
  fractureTable %>% left_join(fractureTable %>% 
                                left_join(cdm[["death"]], by = c("subject_id" = "person_id"), copy = T) %>%
                                select(subject_id:death_date) %>%
                                filter(index_date < death_date), by = c("subject_id", "condition_concept_id", "condition_start_date", "fracture_site", "index_date", "after_index", "observation_period_end_date", "cancer_date_after_index", "bone_disease_date_after_index", "fracture_after_index"))
}

# add in FOLLOWUPEND - only after the relevant columns are added
addInFollowUpEnd <- function(fractureTable){
  fractureTable %>% 
    mutate(follow_up_end = pmin(after_index, observation_period_end_date, cancer_date_after_index, bone_disease_date_after_index, fracture_after_index, death_date, na.rm = T)) %>%
    mutate(follow_up_time = follow_up_end-index_date) %>%
    filter(follow_up_time > 0)
}

# clean out fractures based on their follow up period, used to further analysis 
nextFractureClean <- function (fractureTable){
  fractureTable %>% 
    filter(follow_up_time == 730) %>%
    group_by(subject_id) %>%
    arrange(condition_start_date, .by_group = T) %>%
    filter(!(row_number()==1)) %>%
    select(subject_id, condition_concept_id, condition_start_date, fracture_site)
}

