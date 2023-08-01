# for a fracture table, compute the index date for each person
addIndex <- function (fractureTable){
  fractureTable %>%
    right_join(fractureTable %>% 
                 filter (condition_start_date >= cohort_start_date) %>%
                 filter (condition_start_date <= cohort_end_date) %>% 
                 group_by (subject_id) %>% 
                 summarise (index_date = min(condition_start_date, na.rm = T)), by = "subject_id")
}

# for a fracture table, removing individuals with index fracture on the same day as a record of death
noDeathOnIndex <- function (fractureTable){
  fractureTable %>% anti_join(cdm[["death"]], by = c("subject_id" = "person_id", "index_date" = "death_date"), copy = T)
}

noDeathOnOrAfterIndex <- function(fractureTable){
  fractureTable %>%
    left_join(cdm[["death"]], by = c("subject_id"= "person_id"), copy = T) %>% 
    filter(death_date >= index_date |is.na(death_date)) %>%
    select(colnames(fractureTable))
}

# for a fracture table, removing individuals with cancer before or on the same day as the index fracture
noCancerPriorOrOnIndex <- function (fractureTable){
  fractureTable %>% 
    anti_join(fractureTable %>% 
                select(-cohort_start_date, -cohort_end_date) %>%
                inner_join(cdm[[exclusionCohortTableName]] %>% 
                             filter(cohort_definition_id == cancerId), by = "subject_id", copy = T, relationship = "many-to-many") %>%
                filter(cohort_start_date<=index_date), by = "subject_id")
}

# for a fracture table, removing individuals with bone disease before or on the same day as the index fracture
noBoneDiseasePriorOrOnIndex <- function (fractureTable){
  fractureTable %>% 
    anti_join(fractureTable %>% 
                select(-cohort_start_date, -cohort_end_date) %>%
                inner_join(cdm[[exclusionCohortTableName]] %>% 
                             filter(cohort_definition_id == BoneDiseaseId), by = "subject_id", copy = T, relationship = "many-to-many") %>%
                filter(cohort_start_date<=index_date), by = "subject_id")
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
    select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date, fracture_site, index_date, after_index, observation_period_end_date)
}

# add in a column indicating the date of cancer after the index date
addInCancerPostIndex <- function (fractureTable){
  fractureTable %>% left_join(fractureTable %>% 
                                inner_join(cdm[[exclusionCohortTableName]] %>%
                                             filter(cohort_definition_id == cancerId) %>%
                                             rename(cancer_start_date = cohort_start_date, cancer_end_date = cohort_end_date), 
                                           by = "subject_id", 
                                           copy = T, 
                                           relationship = "many-to-many") %>%
                                filter(index_date < cancer_start_date) %>%   
                                group_by(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date, fracture_site, index_date, observation_period_end_date) %>%
                                arrange(cancer_start_date) %>%
                                filter(row_number()==1) %>%
                                ungroup(), by = c("subject_id", "cohort_start_date", "cohort_end_date", "condition_concept_id", "condition_start_date", "fracture_site", "index_date", "after_index", "observation_period_end_date")) %>%
    select(-cohort_definition_id, - cancer_end_date) 
}

# add in a column indicating the date of bone disease after the index date (has to be done after the cancer one)
addInBoneDiseasePostIndex <- function (fractureTable){
  fractureTable %>% left_join(fractureTable %>% 
                                inner_join(cdm[[exclusionCohortTableName]] %>% 
                                             filter(cohort_definition_id == BoneDiseaseId) %>%
                                             rename(bone_disease_start_date = cohort_start_date, bone_disease_end_date = cohort_end_date), 
                                           by = "subject_id", 
                                           copy = T, 
                                           relationship = "many-to-many") %>%
                                filter(index_date < bone_disease_start_date) %>%   
                                group_by(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date, fracture_site, index_date, observation_period_end_date) %>%
                                arrange(cohort_start_date) %>%
                                filter(row_number()==1) %>%
                                ungroup(), by = c("subject_id", "cohort_start_date", "cohort_end_date", "condition_concept_id", "condition_start_date", "fracture_site", "index_date", "after_index", "observation_period_end_date", "cancer_start_date")) %>%
    select(-cohort_definition_id, -bone_disease_end_date) 
}

# add a column of next fracture after the index one
addInNextFracture <- function(fractureTable){
  fractureTable %>% left_join(fractureTable %>% group_by(subject_id) %>% filter(condition_start_date> index_date) %>% summarise(fracture_after_index = min(condition_start_date, na.rm =  T)),
                              by = "subject_id")
}

# add a column of death record date after the index date
addInDeath <- function(fractureTable){
  fractureTable %>% 
    left_join(cdm[["death"]], by = c("subject_id" = "person_id"), copy = T) %>%
    filter(death_date > index_date | is.na(death_date)) %>%
    select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date, fracture_site, index_date, after_index, observation_period_end_date, cancer_start_date, bone_disease_start_date, fracture_after_index, death_date)
}

# add in FOLLOWUPEND - only after the relevant columns are added
addInFollowUpEnd <- function(fractureTable){
  fractureTable %>% 
    mutate(follow_up_end = pmin(after_index, observation_period_end_date, cancer_start_date, bone_disease_start_date, fracture_after_index, death_date, na.rm = T)) %>%
    mutate(follow_up_time = follow_up_end-index_date) 
}

# clean out fractures based on their follow up period, used to further analysis 
nextFractureClean <- function (fractureTable){
  fractureTable %>% 
    anti_join(fractureTable %>% filter(imminentFracture==1), by = "subject_id") %>%
    group_by(subject_id) %>%
    arrange(condition_start_date, .by_group = T) %>%
    filter(!(row_number()==1)) %>%
    select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date, fracture_site) %>%
    ungroup()
}

# add in immFracture, a function that indicates which fracture is considered imminent in relation to the current index fracture
immFracture <- function (fractureTable){
  fractureTable %>%
    mutate(imminentFracture = as.integer(condition_start_date == follow_up_end))
}

##################################################################################
#                                                                                #
#                  Following functions are due to Nuria and Marti                #
#                                                                                #
##################################################################################
addNumberVisit <- function(x, cdm, window = c(NA, NA), name = "number_visit", compute = TRUE) {
  result <- cdm[["visit_occurrence"]] %>%
    dplyr::select(
      "subject_id" = "person_id", "visit_concept_id", "visit_start_date"
    ) %>%
    dplyr::inner_join(
      x %>%
        dplyr::select("subject_id", "cohort_start_date") %>%
        dplyr::distinct(),
      by = "subject_id"
    )
  if (!is.na(window[1])) {
    result <- result %>%
      dplyr::filter(
        CDMConnector::dateadd("cohort_start_date", window[1]) <=
          .data$visit_start_date
      )
  }
  if (!is.na(window[2])) {
    result <- result %>%
      dplyr::filter(
        CDMConnector::dateadd("cohort_start_date", window[2]) >=
          .data$visit_start_date
      )
  }
  result <- result %>%
    dplyr::group_by(
      .data$subject_id, .data$cohort_start_date
    ) %>%
    dplyr::summarise(!!name := dplyr::n(), .groups = "drop") %>%
    dplyr::compute() %>%
    dplyr::right_join(
      x,
      by = c("subject_id", "cohort_start_date")
    ) %>%
    dplyr::mutate(!!name := dplyr::if_else(is.na(.data[[name]]), 0, .data[[name]])) %>%
    dplyr::select(dplyr::all_of(c(colnames(x), name)))
  if (isTRUE(compute)) {
    result <- result %>% dplyr::compute()
  }
  return(result)
}
