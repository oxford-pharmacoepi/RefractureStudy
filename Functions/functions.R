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
                inner_join(cdm[["cancer"]], by = "subject_id", copy = T, relationship = "many-to-many") %>%
                filter(cancer_date<=index_date), by = "subject_id")
}

# for a fracture table, removing individuals with bone disease before or on the same day as the index fracture
noBoneDiseasePriorOrOnIndex <- function (fractureTable){
  fractureTable %>% 
    anti_join(fractureTable %>% 
                select(-cohort_start_date, -cohort_end_date) %>%
                inner_join(cdm[["mbd"]], by = "subject_id", copy = T, relationship = "many-to-many") %>%
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
                                inner_join(cdm[["cancer"]] %>% rename(cancer_concept_id = condition_concept_id), 
                                           by = c("subject_id", "cohort_start_date", "cohort_end_date"), 
                                           copy = T, 
                                           relationship = "many-to-many") %>%
                                filter(index_date < cancer_date) %>%   
                                group_by(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date, fracture_site, index_date, observation_period_end_date) %>%
                                arrange(cancer_date) %>%
                                filter(row_number()==1) %>%
                                ungroup(), by = c("subject_id", "cohort_start_date", "cohort_end_date", "condition_concept_id", "condition_start_date", "fracture_site", "index_date", "after_index", "observation_period_end_date")) %>%
    select(-cancer_concept_id) 
}

# add in a column indicating the date of bone disease after the index date (has to be done after the cancer one)
addInBoneDiseasePostIndex <- function (fractureTable){
  fractureTable %>% left_join(fractureTable %>% 
                                inner_join(cdm[["mbd"]] %>%
                                             rename(bone_disease_concept_id = condition_concept_id), 
                                           by = c("subject_id", "cohort_start_date", "cohort_end_date"),
                                           copy = T, 
                                           relationship = "many-to-many") %>%
                                filter(index_date < mbd_date) %>%   
                                group_by(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date, fracture_site, index_date, observation_period_end_date) %>%
                                arrange(mbd_date) %>%
                                filter(row_number()==1) %>%
                                ungroup(), by = c("subject_id", "cohort_start_date", "cohort_end_date", "condition_concept_id", "condition_start_date", "fracture_site", "index_date", "after_index", "observation_period_end_date", "cancer_date")) %>%
    select(-bone_disease_concept_id) 
}

# add a column of next fracture after the index one
addInNextFracture <- function(fractureTable){
  fractureTable %>% 
    left_join(fractureTable %>% 
                filter(condition_start_date> index_date) %>% 
                group_by(subject_id) %>% 
                summarise(fracture_after_index = min(condition_start_date, na.rm =  T)),
              by = "subject_id")
}

# add a column of death record date after the index date
addInDeath <- function(fractureTable){
  fractureTable %>% 
    left_join(cdm[["death"]], by = c("subject_id" = "person_id"), copy = T) %>%
    filter(death_date > index_date | is.na(death_date)) %>%
    select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date, fracture_site, index_date, after_index, observation_period_end_date, cancer_date, mbd_date, fracture_after_index, death_date)
}

# add in FOLLOWUPEND - only after the relevant columns are added
addInFollowUpEnd <- function(fractureTable){
  fractureTable %>% 
    mutate(follow_up_end = pmin(after_index, observation_period_end_date, cancer_date, mbd_date, fracture_after_index, death_date, na.rm = T)) %>%
    mutate(follow_up_time = follow_up_end-index_date) 
}

# clean out fractures based on their follow up period, used for further analysis 
nextFractureClean <- function (fractureTable){
  fractureTable %>%
    anti_join(fractureTable %>% filter(imminentFracture==1|follow_up_time <730), by = "subject_id") %>%
    group_by(subject_id) %>%
    arrange(condition_start_date, .by_group = T) %>%
    filter(condition_start_date>index_date) %>%
    select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date, fracture_site) %>%
    ungroup()%>%
    anti_join(fractureTable %>% 
                         mutate(within = condition_start_date<=cohort_end_date) %>%
                         group_by(subject_id) %>%
                         summarise(tot = sum(within), .groups = "drop") %>%
                         filter(tot == 0), by = "subject_id") 
}

# add in immFracture, a function that indicates which fracture is considered imminent in relation to the current index fracture
immFracture <- function (fractureTable){
  fractureTable %>%
    mutate(imminentFracture = as.integer(condition_start_date == follow_up_end))
}

# finding the common columns of target cohort and comp cohort 1 where each has at least lower_bound of covariates 
lowerBoundLasso01 <- function(subfeatures_01, lower_bound){
  subfeatures_01_0 <- subfeatures_01 %>% 
    inner_join(targetCohort[[i]] %>% select(subject_id, index_date), by = c("subject_id", "index_date")) %>%
    distinct() %>%
    group_by(feature) %>%
    tally() %>% filter(n>=lower_bound) %>% 
    pull(feature)
  
  subfeatures_01_1 <- subfeatures_01 %>%
    inner_join(compCohort1[[i]] %>% select(subject_id, index_date), by = c("subject_id", "index_date")) %>%
    distinct() %>%
    group_by(feature) %>%
    tally() %>% filter(n>=lower_bound) %>% 
    pull(feature)
  
  comms <- intersect(subfeatures_01_0, subfeatures_01_1)
  rm(subfeatures_01_0, subfeatures_01_1)
  subfeatures_01 <- subfeatures_01 %>%
    filter(feature %in% comms)
  return(subfeatures_01)
}

# finding the common columns of target cohort and comp cohort 1 where each has at least lower_bound of covariates 
lowerBoundLasso12 <- function(subfeatures_12, lower_bound){
  subfeatures_12_1 <- subfeatures_12 %>% 
    inner_join(compCohort1[[i]] %>% select(subject_id, index_date), by = c("subject_id", "index_date")) %>%
    distinct() %>%
    group_by(feature) %>%
    tally() %>% filter(n>=lower_bound) %>% 
    pull(feature)
  
  subfeatures_12_2 <- subfeatures_12 %>%
    inner_join(compCohort2[[i]] %>% select(subject_id, index_date), by = c("subject_id", "index_date")) %>%
    distinct() %>%
    group_by(feature) %>%
    tally() %>% filter(n>=lower_bound) %>% 
    pull(feature)
  
  comms <- intersect(subfeatures_12_1, subfeatures_12_2)
  rm(subfeatures_12_1, subfeatures_12_2)
  subfeatures_12 <- subfeatures_12 %>%
    filter(feature %in% comms)
  return(subfeatures_12)
}

# reformating of table ones
reformat_table_one <- function(result_imm, result_no_imm, result_frac){
  
  table_one_no_imm <- result_no_imm
  table_one_imm <- result_imm
  table_one_frac <- result_frac
  reformatted_table1 <- data.frame(x = NA, y= NA, z = NA, w = NA)
  n1<- table_one_no_imm %>% filter(variable == "Number subjects") %>% pull(estimate)
  n2 <- table_one_imm %>% filter(variable == "Number subjects") %>% pull(estimate)
  
  # variables assembled by min/max etc
  cat_var <- table_one_no_imm %>% filter(estimate_type == "min") %>% select(variable) %>% distinct() %>% pull(variable)
  
  for (i in (1:length(cat_var))){
    reformatted_table1 <- rbind(reformatted_table1, 
                                data.frame(x = paste0(cat_var[[i]], ", median (IQR)"), 
                                           y = paste0(table_one_frac %>% filter(variable == cat_var[[i]]) %>% filter(estimate_type == "median") %>% pull(estimate),
                                                      " (",
                                                      table_one_frac %>% filter(variable == cat_var[[i]]) %>% filter(estimate_type == "q25") %>% pull(estimate),
                                                      "-",
                                                      table_one_frac %>% filter(variable == cat_var[[i]]) %>% filter(estimate_type == "q75") %>% pull(estimate),
                                                      ")"),
                                           z = paste0(table_one_imm %>% filter(variable == cat_var[[i]]) %>% filter(estimate_type == "median") %>% pull(estimate),
                                                      " (",
                                                      table_one_imm %>% filter(variable == cat_var[[i]]) %>% filter(estimate_type == "q25") %>% pull(estimate),
                                                      "-",
                                                      table_one_imm %>% filter(variable == cat_var[[i]]) %>% filter(estimate_type == "q75") %>% pull(estimate),
                                                      ")"),
                                           w = paste0(table_one_no_imm %>% filter(variable == cat_var[[i]]) %>% filter(estimate_type == "median") %>% pull(estimate),
                                                      " (",
                                                      table_one_no_imm %>% filter(variable == cat_var[[i]]) %>% filter(estimate_type == "q25") %>% pull(estimate),
                                                      "-",
                                                      table_one_no_imm %>% filter(variable == cat_var[[i]]) %>% filter(estimate_type == "q75") %>% pull(estimate),
                                                      ")"))
    )
  }
  
  # age group variables
  age_var <- table_one_frac %>% 
    filter(variable == "Age group") %>% 
    select(variable_level) %>%
    distinct() %>%
    pull()
  
  for (i in (1:length(age_var))){
    reformatted_table1 <- rbind(reformatted_table1, data.frame(x = paste0("Age Group, ", age_var[[i]], ", n(%)"),
                                                               y = paste0(table_one_frac %>% filter(variable_level == age_var[[i]]) %>% filter(estimate_type == "count") %>% pull(estimate),
                                                                          " (",
                                                                          round(as.numeric(table_one_frac %>% filter(variable_level == age_var[[i]]) %>% filter(estimate_type == "%") %>% pull(estimate)), digits = 1),
                                                                          ")"),
                                                               z = paste0(table_one_imm %>% filter(variable_level == age_var[[i]]) %>% filter(estimate_type == "count") %>% pull(estimate),
                                                                          " (",
                                                                          round(as.numeric(table_one_imm %>% filter(variable_level == age_var[[i]]) %>% filter(estimate_type == "%") %>% pull(estimate)), digits = 1),
                                                                          ")"),
                                                               w = paste0(table_one_no_imm %>% filter(variable_level == age_var[[i]]) %>% filter(estimate_type == "count") %>% pull(estimate),
                                                                          " (",
                                                                          round(as.numeric(table_one_no_imm %>% filter(variable_level == age_var[[i]]) %>% filter(estimate_type == "%") %>% pull(estimate)), digits = 1),
                                                                          ")")))
  }
  
  
  #condition variables
  condition_var <- table_one_frac %>% 
    filter(variable == "Conditions flag -inf to 0 days") %>%
    filter(!variable_level == "Malignant neoplastic disease") %>%
    select(variable_level) %>%
    distinct() %>%
    pull(variable_level)
  
  for (i in (1:length(condition_var))){
    reformatted_table1 <- rbind(reformatted_table1, data.frame(x = paste0(condition_var[[i]], ", n(%)"),
                                                               y = paste0(table_one_frac %>% filter(variable_level == condition_var[[i]]) %>% filter(estimate_type == "count") %>% pull(estimate),
                                                                          " (",
                                                                          round(as.numeric(table_one_frac %>% filter(variable_level == condition_var[[i]]) %>% filter(estimate_type == "%") %>% pull(estimate)), digits = 1),
                                                                          ")"),
                                                               z = paste0(table_one_imm %>% filter(variable_level == condition_var[[i]]) %>% filter(estimate_type == "count") %>% pull(estimate),
                                                                          " (",
                                                                          round(as.numeric(table_one_imm %>% filter(variable_level == condition_var[[i]]) %>% filter(estimate_type == "%") %>% pull(estimate)), digits = 1),
                                                                          ")"),
                                                               w = paste0(table_one_no_imm %>% filter(variable_level == condition_var[[i]]) %>% filter(estimate_type == "count") %>% pull(estimate),
                                                                          " (",
                                                                          round(as.numeric(table_one_no_imm %>% filter(variable_level == condition_var[[i]]) %>% filter(estimate_type == "%") %>% pull(estimate)), digits = 1),
                                                                          ")")))
  }
  
  #medication variables
  medication_var <- table_one_frac %>% 
    filter(stringr::str_detect(variable, 'Ref')) %>%
    filter(!variable == "Ref medications antineoplastic agents flag m365 to 0") %>%
    select(variable) %>%
    distinct() %>%
    pull(variable)
  
  for (i in (1:length(medication_var))){
    reformatted_table1 <- rbind(reformatted_table1, data.frame(x = paste0(medication_var[[i]], ", n(%)"),
                                                               y = paste0(table_one_frac %>% filter(variable == medication_var[[i]]) %>% filter(estimate_type == "count") %>% pull(estimate),
                                                                          " (",
                                                                          round(as.numeric(table_one_frac %>% filter(variable == medication_var[[i]]) %>% filter(estimate_type == "%") %>% pull(estimate)), digits = 1),
                                                                          ")"),
                                                               z = paste0(table_one_imm %>% filter(variable == medication_var[[i]]) %>% filter(estimate_type == "count") %>% pull(estimate),
                                                                          " (",
                                                                          round(as.numeric(table_one_imm %>% filter(variable == medication_var[[i]]) %>% filter(estimate_type == "%") %>% pull(estimate)), digits = 1),
                                                                          ")"),
                                                               w = paste0(table_one_no_imm %>% filter(variable == medication_var[[i]]) %>% filter(estimate_type == "count") %>% pull(estimate),
                                                                          " (",
                                                                          round(as.numeric(table_one_no_imm %>% filter(variable == medication_var[[i]]) %>% filter(estimate_type == "%") %>% pull(estimate)), digits = 1),
                                                                          ")")))
  }
  reformatted_table1 <- reformatted_table1 %>% distinct()
  ### adjusting colnames
  reformatted_table1_meds <- reformatted_table1 %>%
    filter(stringr::str_detect(x, 'Ref'))
  reformatted_table1_meds[["x"]]<-substr(reformatted_table1_meds$x, 16, nchar(reformatted_table1_meds$x)-21)
  reformatted_table1_meds <- reformatted_table1_meds %>% mutate(x = paste0(x, ", n(%)"))
  
  reformatted_table1 <- rbind(reformatted_table1 %>% filter(!stringr::str_detect(x, 'Ref')), reformatted_table1_meds)
  
  reformatted_table1 <- reformatted_table1 %>%
    mutate(y = ifelse((stringr::str_detect(z, '<5')|stringr::str_detect(w, '<5')), "obsecured", y))
  
  ###rename columns
  colnames(reformatted_table1) <- c(
    "Characteristic",
    paste0("Overall (n = ", as.integer(n1)+as.integer(n2), ")"),
    paste0("Imminent Fracture (n = ", n2, ")"),
    paste0("No Imminent Fracture (n = ", n1, ")")
  )
  return(reformatted_table1)
}
