# for a fracture table, compute the index date for each person
addIndex <- function (fractureTable){
  fractureTable %>% 
    dplyr::inner_join(fractureTable %>% 
                        dplyr::filter(class == "index") %>%
                        dplyr::group_by(subject_id) %>% 
                        dplyr::summarise(index_date = min(condition_start_date)),
                      by = "subject_id"
    )
}

# for a fracture table, removing individuals with index fracture on the same day as a record of death
noDeathOnIndex <- function (fractureTable){
  fractureTable %>% dplyr::anti_join(cdm[["death"]], by = c("subject_id" = "person_id", "index_date" = "death_date"), copy = T)
}

noDeathOnOrAfterIndex <- function(fractureTable){
  fractureTable %>%
    dplyr::left_join(cdm[["death"]], by = c("subject_id"= "person_id"), copy = T) %>% 
    dplyr::filter(death_date >= index_date |is.na(death_date)) %>%
    dplyr::select(colnames(fractureTable))
}

# for a fracture table, removing individuals with cancer before or on the same day as the index fracture
noCancerPriorOrOnIndex <- function (fractureTable){
  fractureTable %>% 
    dplyr::anti_join(fractureTable %>% 
                dplyr::select(-cohort_start_date, -cohort_end_date) %>%
                dplyr::inner_join(cdm[["cancer"]], by = "subject_id", copy = T, relationship = "many-to-many") %>%
                dplyr::filter(cancer_date<=index_date), by = "subject_id")
}

# for a fracture table, removing individuals with bone disease before or on the same day as the index fracture
noBoneDiseasePriorOrOnIndex <- function (fractureTable){
  fractureTable %>% 
    dplyr::anti_join(fractureTable %>% 
    dplyr::select(-cohort_start_date, -cohort_end_date) %>%
    dplyr::inner_join(cdm[["mbd"]], by = "subject_id", copy = T, relationship = "many-to-many") %>%
    dplyr::filter(cohort_start_date<=index_date), by = "subject_id")
}

# add an extra variable after_index which is two years after the index date
addInTwoYearsAfter <- function (fractureTable){
  fractureTable %>%
    dplyr::mutate(after_index = index_date + 730)
}

# add in the end of observation period for each individual
addInObsEndDate <- function (fractureTable){
  fractureTable %>% 
    dplyr::left_join(cdm[["observation_period"]], by = c("subject_id" = "person_id"), copy = T) %>%
    dplyr::select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date, fracture_site, class, index_date, after_index, observation_period_end_date)
}

# add in a column indicating the date of cancer after the index date
addInCancerPostIndex <- function (fractureTable){
  fractureTable %>% dplyr::left_join(fractureTable %>% 
                                dplyr::inner_join(cdm[["cancer"]] %>% rename(cancer_concept_id = condition_concept_id), 
                                           by = c("subject_id", "cohort_start_date", "cohort_end_date"), 
                                           copy = T, 
                                           relationship = "many-to-many") %>%
                                dplyr::filter(index_date < cancer_date) %>%   
                                dplyr::group_by(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date, fracture_site, class, index_date, observation_period_end_date) %>%
                                dplyr::arrange(cancer_date) %>%
                                dplyr::filter(row_number()==1) %>%
                                dplyr::ungroup(), by = c("subject_id", "cohort_start_date", "cohort_end_date", "condition_concept_id", "condition_start_date", "fracture_site", "class", "index_date", "after_index", "observation_period_end_date")) %>%
    dplyr::select(-cancer_concept_id) 
}

# add in a column indicating the date of bone disease after the index date (has to be done after the cancer one)
addInBoneDiseasePostIndex <- function (fractureTable){
  fractureTable %>% dplyr::left_join(fractureTable %>% 
                                dplyr::inner_join(cdm[["mbd"]] %>%
                                             rename(bone_disease_concept_id = condition_concept_id), 
                                           by = c("subject_id", "cohort_start_date", "cohort_end_date"),
                                           copy = T, 
                                           relationship = "many-to-many") %>%
                                dplyr::filter(index_date < mbd_date) %>%   
                                dplyr::group_by(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date, fracture_site, class, index_date, observation_period_end_date) %>%
                                dplyr::arrange(mbd_date) %>%
                                dplyr::filter(row_number()==1) %>%
                                dplyr::ungroup(), by = c("subject_id", "cohort_start_date", "cohort_end_date", "condition_concept_id", "condition_start_date", "fracture_site", "class", "index_date", "after_index", "observation_period_end_date", "cancer_date")) %>%
    dplyr::select(-bone_disease_concept_id) 
}

# add a column of next fracture after the index one
addInNextFracture <- function(fractureTable){
  fractureTable %>% 
    dplyr::left_join(fractureTable %>% 
    dplyr::filter(condition_start_date> index_date) %>% 
    dplyr::group_by(subject_id) %>% 
    dplyr::summarise(fracture_after_index = min(condition_start_date, na.rm =  T)),
              by = "subject_id")
}

# add a column of death record date after the index date
addInDeath <- function(fractureTable){
  fractureTable %>% 
    dplyr::left_join(cdm[["death"]], by = c("subject_id" = "person_id"), copy = T) %>%
    dplyr::filter(death_date > index_date | is.na(death_date)) %>%
    dplyr::select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date, fracture_site, class, index_date, after_index, observation_period_end_date, cancer_date, mbd_date, fracture_after_index, death_date)
}

# add in FOLLOWUPEND - only after the relevant columns are added
addInFollowUpEnd <- function(fractureTable){
  fractureTable %>% 
    dplyr::mutate(follow_up_end = pmin(.data$after_index, .data$observation_period_end_date, .data$cancer_date, .data$mbd_date, .data$fracture_after_index, .data$death_date, na.rm = T)) %>%
    dplyr::mutate(follow_up_time = follow_up_end-index_date) 
}

# clean out fractures based on their follow up period, used for further analysis 
nextFractureClean <- function (fractureTable){
  pre <- 
    fractureTable %>%
    dplyr::anti_join(fractureTable %>% dplyr::filter(imminentFracture==1|follow_up_time <730|(mbd_date == follow_up_end)|(cancer_date == follow_up_end)|(death_date == follow_up_end)|(observation_period_end_date == follow_up_end)), by = "subject_id") %>% 
    dplyr::group_by(subject_id) %>%
    dplyr::arrange(condition_start_date, .by_group = T) %>%
    dplyr::filter(condition_start_date>index_date) %>%
    dplyr::select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date, fracture_site, class) %>%
    dplyr::ungroup()
  
  index_ids <- pre %>% 
    dplyr::filter(class == "index") %>%
    pull(subject_id)
  
  pre <- pre %>%
    dplyr::filter(subject_id %in% index_ids)
  return(pre)
}

# add in immFracture, a function that indicates which fracture is considered imminent in relation to the current index fracture
immFracture <- function (fractureTable){
  fractureTable %>%
    dplyr::mutate(imminentFracture = as.integer(condition_start_date == follow_up_end))
}

# finding the common columns of target cohort and comp cohort 1 where each has at least lower_bound of covariates 
lowerBoundLasso01 <- function(subfeatures_01, lower_bound){
  subfeatures_01_0 <- subfeatures_01 %>% 
    dplyr::inner_join(targetCohort[[l]] %>% dplyr::select(subject_id, index_date), by = c("subject_id", "index_date")) %>%
    dplyr::distinct() %>%
    dplyr::group_by(feature) %>%
    dplyr::tally() %>% 
    dplyr::filter(n>=lower_bound) %>% 
    dplyr::pull(feature)
  
  subfeatures_01_1 <- subfeatures_01 %>%
    dplyr::inner_join(compCohort1[[l]] %>% dplyr::select(subject_id, index_date), by = c("subject_id", "index_date")) %>%
    dplyr::distinct() %>%
    dplyr::group_by(feature) %>%
    dplyr::tally() %>% dplyr::filter(n>=lower_bound) %>% 
    dplyr::pull(feature)
  
  comms <- intersect(subfeatures_01_0, subfeatures_01_1)
  rm(subfeatures_01_0, subfeatures_01_1)
  subfeatures_01 <- subfeatures_01 %>%
    dplyr::filter(feature %in% comms)
  return(subfeatures_01)
}

# finding the common columns of target cohort and comp cohort 1 where each has at least lower_bound of covariates 
lowerBoundLasso12 <- function(subfeatures_12, lower_bound){
  subfeatures_12_1 <- subfeatures_12 %>% 
    dplyr::inner_join(compCohort1[[l]] %>% dplyr::select(subject_id, index_date), by = c("subject_id", "index_date")) %>%
    dplyr::distinct() %>%
    dplyr::group_by(feature) %>%
    dplyr::tally() %>% dplyr::filter(n>=lower_bound) %>% 
    dplyr::pull(feature)
  
  subfeatures_12_2 <- subfeatures_12 %>%
    dplyr::inner_join(compCohort2[[l]] %>% dplyr::select(subject_id, index_date), by = c("subject_id", "index_date")) %>%
    dplyr::distinct() %>%
    dplyr::group_by(feature) %>%
    dplyr::tally() %>% 
    dplyr::filter(n>=lower_bound) %>% 
    dplyr::pull(feature)
  
  comms <- intersect(subfeatures_12_1, subfeatures_12_2)
  rm(subfeatures_12_1, subfeatures_12_2)
  subfeatures_12 <- subfeatures_12 %>%
    dplyr::filter(feature %in% comms)
  return(subfeatures_12)
}

# reformating of table ones
reformat_table_one <- function(result_imm, result_no_imm, result_frac){
  
  table_one_no_imm <- result_no_imm
  table_one_imm <- result_imm
  table_one_frac <- result_frac
  reformatted_table1 <- data.frame(x = NA, y= NA, z = NA, w = NA)
  n1<- table_one_no_imm %>% dplyr::filter(variable == "Number subjects") %>% dplyr::pull(estimate)
  n2 <- table_one_imm %>% dplyr::filter(variable == "Number subjects") %>% dplyr::pull(estimate)
  
  # variables assembled by min/max etc
  cat_var <- table_one_no_imm %>% dplyr::filter(estimate_type == "min") %>% dplyr::select(variable) %>% dplyr::distinct() %>% dplyr::pull(variable)
  
  for (i in (1:length(cat_var))){
    reformatted_table1 <- rbind(reformatted_table1, 
                                data.frame(x = paste0(cat_var[[i]], ", median (IQR)"), 
                                           y = paste0(table_one_frac %>% dplyr::filter(variable == cat_var[[i]]) %>% dplyr::filter(estimate_type == "median") %>% dplyr::pull(estimate),
                                                      " (",
                                                      table_one_frac %>% dplyr::filter(variable == cat_var[[i]]) %>% dplyr::filter(estimate_type == "q25") %>% dplyr::pull(estimate),
                                                      "-",
                                                      table_one_frac %>% dplyr::filter(variable == cat_var[[i]]) %>% dplyr::filter(estimate_type == "q75") %>% dplyr::pull(estimate),
                                                      ")"),
                                           z = paste0(table_one_imm %>% dplyr::filter(variable == cat_var[[i]]) %>% dplyr::filter(estimate_type == "median") %>% dplyr::pull(estimate),
                                                      " (",
                                                      table_one_imm %>% dplyr::filter(variable == cat_var[[i]]) %>% dplyr::filter(estimate_type == "q25") %>% dplyr::pull(estimate),
                                                      "-",
                                                      table_one_imm %>% dplyr::filter(variable == cat_var[[i]]) %>% dplyr::filter(estimate_type == "q75") %>% dplyr::pull(estimate),
                                                      ")"),
                                           w = paste0(table_one_no_imm %>% dplyr::filter(variable == cat_var[[i]]) %>% dplyr::filter(estimate_type == "median") %>% dplyr::pull(estimate),
                                                      " (",
                                                      table_one_no_imm %>% dplyr::filter(variable == cat_var[[i]]) %>% dplyr::filter(estimate_type == "q25") %>% dplyr::pull(estimate),
                                                      "-",
                                                      table_one_no_imm %>% dplyr::filter(variable == cat_var[[i]]) %>% dplyr::filter(estimate_type == "q75") %>% dplyr::pull(estimate),
                                                      ")"))
    )
  }
  
  # age group variables
  age_var <- table_one_frac %>% 
    dplyr::filter(variable == "Age group") %>% 
    dplyr::select(variable_level) %>%
    dplyr::distinct() %>%
    dplyr::pull()
  
  for (i in (1:length(age_var))){
    reformatted_table1 <- rbind(reformatted_table1, data.frame(x = paste0("Age Group, ", age_var[[i]], ", n(%)"),
                                                               y = paste0(table_one_frac %>% dplyr::filter(variable_level == age_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
                                                                          " (",
                                                                          round(as.numeric(table_one_frac %>% dplyr::filter(variable_level == age_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate)), digits = 1),
                                                                          ")"),
                                                               z = paste0(table_one_imm %>% dplyr::filter(variable_level == age_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
                                                                          " (",
                                                                          round(as.numeric(table_one_imm %>% dplyr::filter(variable_level == age_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate)), digits = 1),
                                                                          ")"),
                                                               w = paste0(table_one_no_imm %>% dplyr::filter(variable_level == age_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
                                                                          " (",
                                                                          round(as.numeric(table_one_no_imm %>% dplyr::filter(variable_level == age_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate)), digits = 1),
                                                                          ")")))
  }
  
  
  #condition variables
  condition_var <- table_one_frac %>% 
    dplyr::filter(variable == "Conditions flag -inf to 0 days") %>%
    dplyr::filter(!variable_level == "Malignant neoplastic disease") %>%
    dplyr::select(variable_level) %>%
    dplyr::distinct() %>%
    dplyr::pull(variable_level)
  
  for (i in (1:length(condition_var))){
    reformatted_table1 <- rbind(reformatted_table1, data.frame(x = paste0(condition_var[[i]], ", n(%)"),
                                                               y = paste0(table_one_frac %>% dplyr::filter(variable_level == condition_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
                                                                          " (",
                                                                          round(as.numeric(table_one_frac %>% dplyr::filter(variable_level == condition_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate)), digits = 1),
                                                                          ")"),
                                                               z = paste0(table_one_imm %>% dplyr::filter(variable_level == condition_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
                                                                          " (",
                                                                          round(as.numeric(table_one_imm %>% dplyr::filter(variable_level == condition_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate)), digits = 1),
                                                                          ")"),
                                                               w = paste0(table_one_no_imm %>% dplyr::filter(variable_level == condition_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
                                                                          " (",
                                                                          round(as.numeric(table_one_no_imm %>% dplyr::filter(variable_level == condition_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate)), digits = 1),
                                                                          ")")))
  }
  
  #medication variables
  medication_var <- table_one_frac %>% 
    dplyr::filter(stringr::str_detect(variable, 'medications')) %>%
    dplyr::filter(!stringr::str_detect(variable, 'antineoplastic agents')) %>%
    dplyr::select(variable) %>%
    dplyr::distinct() %>%
    dplyr::pull(variable)
  
  for (i in (1:length(medication_var))){
    reformatted_table1 <- rbind(reformatted_table1, data.frame(x = paste0(medication_var[[i]], ", n(%)"),
                                                               y = paste0(table_one_frac %>% dplyr::filter(variable == medication_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
                                                                          " (",
                                                                          round(as.numeric(table_one_frac %>% dplyr::filter(variable == medication_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate)), digits = 1),
                                                                          ")"),
                                                               z = paste0(table_one_imm %>% dplyr::filter(variable == medication_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
                                                                          " (",
                                                                          round(as.numeric(table_one_imm %>% dplyr::filter(variable == medication_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate)), digits = 1),
                                                                          ")"),
                                                               w = paste0(table_one_no_imm %>% dplyr::filter(variable == medication_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
                                                                          " (",
                                                                          round(as.numeric(table_one_no_imm %>% dplyr::filter(variable == medication_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate)), digits = 1),
                                                                          ")")))
  }
  reformatted_table1 <- reformatted_table1 %>% dplyr::distinct()
  ### adjusting colnames
  reformatted_table1_meds <- reformatted_table1 %>%
    dplyr::filter(stringr::str_detect(x, 'medications'))
  reformatted_table1_meds[["x"]]<-substr(reformatted_table1_meds$x, 16, nchar(reformatted_table1_meds$x)-21)
  reformatted_table1_meds <- reformatted_table1_meds %>% dplyr::mutate(x = paste0(x, ", n(%)"))
  
  reformatted_table1 <- rbind(reformatted_table1 %>% dplyr::filter(!stringr::str_detect(x, 'medications')), reformatted_table1_meds)
  
  # reformatted_table1 <- reformatted_table1 %>%
  #   dplyr::mutate(y = ifelse((stringr::str_detect(z, '<5')|stringr::str_detect(w, '<5')), "obsecured", y))
  
  ###rename columns
  colnames(reformatted_table1) <- c(
    "Characteristic",
    paste0("Overall (n = ", as.integer(n1)+as.integer(n2), ")"),
    paste0("Imminent Fracture (n = ", n2, ")"),
    paste0("No Imminent Fracture (n = ", n1, ")")
  )
  return(reformatted_table1)
}

# reformating of table ones
reformat_table_one_iqvia <- function(result_imm, result_no_imm, result_frac){
  
  table_one_no_imm <- result_no_imm
  table_one_imm <- result_imm
  table_one_frac <- result_frac
  reformatted_table1 <- data.frame(x = NA, y= NA, z = NA, w = NA)
  n1<- table_one_no_imm %>% dplyr::filter(variable == "Number subjects") %>% dplyr::pull(estimate)
  n2 <- table_one_imm %>% dplyr::filter(variable == "Number subjects") %>% dplyr::pull(estimate)
  
  # variables assembled by min/max etc
  cat_var <- table_one_no_imm %>% dplyr::filter(estimate_type == "min") %>% dplyr::select(variable) %>% dplyr::distinct() %>% dplyr::pull(variable)
  
  for (i in (1:length(cat_var))){
    reformatted_table1 <- rbind(reformatted_table1, 
                                data.frame(x = paste0(cat_var[[i]], ", median (IQR)"), 
                                           y = paste0(table_one_frac %>% dplyr::filter(variable == cat_var[[i]]) %>% dplyr::filter(estimate_type == "median") %>% dplyr::pull(estimate),
                                                      " (",
                                                      table_one_frac %>% dplyr::filter(variable == cat_var[[i]]) %>% dplyr::filter(estimate_type == "q25") %>% dplyr::pull(estimate),
                                                      "-",
                                                      table_one_frac %>% dplyr::filter(variable == cat_var[[i]]) %>% dplyr::filter(estimate_type == "q75") %>% dplyr::pull(estimate),
                                                      ")"),
                                           z = paste0(table_one_imm %>% dplyr::filter(variable == cat_var[[i]]) %>% dplyr::filter(estimate_type == "median") %>% dplyr::pull(estimate),
                                                      " (",
                                                      table_one_imm %>% dplyr::filter(variable == cat_var[[i]]) %>% dplyr::filter(estimate_type == "q25") %>% dplyr::pull(estimate),
                                                      "-",
                                                      table_one_imm %>% dplyr::filter(variable == cat_var[[i]]) %>% dplyr::filter(estimate_type == "q75") %>% dplyr::pull(estimate),
                                                      ")"),
                                           w = paste0(table_one_no_imm %>% dplyr::filter(variable == cat_var[[i]]) %>% dplyr::filter(estimate_type == "median") %>% dplyr::pull(estimate),
                                                      " (",
                                                      table_one_no_imm %>% dplyr::filter(variable == cat_var[[i]]) %>% dplyr::filter(estimate_type == "q25") %>% dplyr::pull(estimate),
                                                      "-",
                                                      table_one_no_imm %>% dplyr::filter(variable == cat_var[[i]]) %>% dplyr::filter(estimate_type == "q75") %>% dplyr::pull(estimate),
                                                      ")"))
    )
  }
  
  # age group variables
  age_var <- table_one_frac %>% 
    dplyr::filter(variable == "Age group") %>% 
    dplyr::select(variable_level) %>%
    dplyr::distinct() %>%
    dplyr::pull()
  
  for (i in (1:length(age_var))){
    reformatted_table1 <- rbind(reformatted_table1, data.frame(x = paste0("Age Group, ", age_var[[i]], ", n(%)"),
                                                               y = paste0(table_one_frac %>% dplyr::filter(variable_level == age_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
                                                                          " (",
                                                                          round(as.numeric(table_one_frac %>% dplyr::filter(variable_level == age_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate)), digits = 1),
                                                                          ")"),
                                                               z = paste0(table_one_imm %>% dplyr::filter(variable_level == age_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
                                                                          " (",
                                                                          round(as.numeric(table_one_imm %>% dplyr::filter(variable_level == age_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate)), digits = 1),
                                                                          ")"),
                                                               w = paste0(table_one_no_imm %>% dplyr::filter(variable_level == age_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
                                                                          " (",
                                                                          round(as.numeric(table_one_no_imm %>% dplyr::filter(variable_level == age_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate)), digits = 1),
                                                                          ")")))
  }
  
  
  #condition variables
  condition_var <- table_one_frac %>% 
    dplyr::filter(variable == "Conditions flag -inf to 0 days") %>%
    dplyr::filter(!variable_level == "Malignant neoplastic disease") %>%
    dplyr::select(variable_level) %>%
    dplyr::distinct() %>%
    dplyr::pull(variable_level)
  
  for (i in (1:length(condition_var))){
    reformatted_table1 <- rbind(reformatted_table1, data.frame(x = paste0(condition_var[[i]], ", n(%)"),
                                                               y = paste0(table_one_frac %>% dplyr::filter(variable_level == condition_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
                                                                          " (",
                                                                          round(as.numeric(table_one_frac %>% dplyr::filter(variable_level == condition_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate)), digits = 1),
                                                                          ")"),
                                                               z = paste0(table_one_imm %>% dplyr::filter(variable_level == condition_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
                                                                          " (",
                                                                          round(as.numeric(table_one_imm %>% dplyr::filter(variable_level == condition_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate)), digits = 1),
                                                                          ")"),
                                                               w = paste0(table_one_no_imm %>% dplyr::filter(variable_level == condition_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
                                                                          " (",
                                                                          round(as.numeric(table_one_no_imm %>% dplyr::filter(variable_level == condition_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate)), digits = 1),
                                                                          ")")))
  }
  
  #medication variables
  medication_var <- table_one_frac %>% 
    dplyr::filter(stringr::str_detect(variable, 'medications')) %>%
    dplyr::filter(!variable == "Jb ox ucb s1 medications antineoplastic agents flag m365 to 0") %>%
    dplyr::select(variable) %>%
    dplyr::distinct() %>%
    dplyr::pull(variable)
  
  for (i in (1:length(medication_var))){
    reformatted_table1 <- rbind(reformatted_table1, data.frame(x = paste0(medication_var[[i]], ", n(%)"),
                                                               y = paste0(table_one_frac %>% dplyr::filter(variable == medication_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
                                                                          " (",
                                                                          round(as.numeric(table_one_frac %>% dplyr::filter(variable == medication_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate)), digits = 1),
                                                                          ")"),
                                                               z = paste0(table_one_imm %>% dplyr::filter(variable == medication_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
                                                                          " (",
                                                                          round(as.numeric(table_one_imm %>% dplyr::filter(variable == medication_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate)), digits = 1),
                                                                          ")"),
                                                               w = paste0(table_one_no_imm %>% dplyr::filter(variable == medication_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
                                                                          " (",
                                                                          round(as.numeric(table_one_no_imm %>% dplyr::filter(variable == medication_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate)), digits = 1),
                                                                          ")")))
  }
  reformatted_table1 <- reformatted_table1 %>% dplyr::distinct()
  ### adjusting colnames
  reformatted_table1_meds <- reformatted_table1 %>%
    dplyr::filter(stringr::str_detect(x, 'medications'))
  reformatted_table1_meds[["x"]]<-substr(reformatted_table1_meds$x, 25, nchar(reformatted_table1_meds$x)-21)
  reformatted_table1_meds <- reformatted_table1_meds %>% dplyr::mutate(x = paste0(x, ", n(%)"))
  
  reformatted_table1 <- rbind(reformatted_table1 %>% dplyr::filter(!stringr::str_detect(x, 'medications')), reformatted_table1_meds)
  
  # reformatted_table1 <- reformatted_table1 %>%
  #   dplyr::mutate(y = ifelse((stringr::str_detect(z, '<5')|stringr::str_detect(w, '<5')), "obsecured", y))
  
  ###rename columns
  colnames(reformatted_table1) <- c(
    "Characteristic",
    paste0("Overall (n = ", as.integer(n1)+as.integer(n2), ")"),
    paste0("Imminent Fracture (n = ", n2, ")"),
    paste0("No Imminent Fracture (n = ", n1, ")")
  )
  return(reformatted_table1)
}

###################################################################################
#                                                                                 #
#                             Functions from Gianluca                             # 
#                                                                                 #
###################################################################################
# Analyse primary care visits

analyse_visits <- function(cohort_combined, visit_data) {
  
  ### Filtering visits based on the cohort_combined
  filtered_visits <- visit_data %>%
    dplyr::left_join(cohort_combined, by = "subject_id", relationship = "many-to-many", copy = T) %>%
    dplyr::filter(visit_detail_start_date >= index_date & visit_detail_start_date <= follow_up_end) %>%
    dplyr::group_by(subject_id, index_date, specialty) %>% # we group by index date to ensure each visit is associated with an entry
    dplyr::summarise(visit_count = n(), .groups = "drop") %>%
    dplyr::ungroup() %>% 
    CDMConnector::computeQuery()

  # Initialize provider_cost_inputs_2
  provider_cost_inputs_2 <- provider_cost_inputs
  
  if(country_setting == "Netherlands"){
    provider_cost_inputs_2 <- provider_cost_inputs_2 %>%
      dplyr::rename(specialty = visit_concept_id) %>%
      dplyr::select(specialty, description_athena)
    # Join and create new names for specialties
    filtered_visits <- filtered_visits %>%
      dplyr::left_join(provider_cost_inputs_2, by = "specialty") %>%
      dplyr::mutate(specialty_temp = ifelse(is.na(description_athena), specialty, description_athena)) %>%
      dplyr::select(-description_athena, -specialty) %>% 
      dplyr::rename(specialty = specialty_temp) %>% 
      CDMConnector::computeQuery()
  } else if(country_setting != "UK" & country_setting != "Italy"){
    provider_cost_inputs_2 <- provider_cost_inputs_2 %>%
      dplyr::rename(specialty = specialty_concept_id)
    # Join and create new names for specialties
    filtered_visits <- filtered_visits %>%
      dplyr::left_join(provider_cost_inputs_2, by = "specialty") %>%
      dplyr::mutate(specialty_temp = ifelse(is.na(description_athena), specialty, description_athena)) %>%
      dplyr::select(-description_athena, -specialty) %>% 
      dplyr::rename(specialty = specialty_temp)
  }

  # Remove provider_cost_inputs_2 
  rm(provider_cost_inputs_2)
  
  ### Pivot the data
  visits_count_wide <- filtered_visits %>%
    pivot_wider(names_from = specialty, values_from = visit_count, values_fill = NA) %>% 
    CDMConnector::computeQuery()
  
  ### Join the wide dataframe back to cohort_combined and count tot num visits
  visits_count_wide <- cohort_combined %>% 
    dplyr::left_join(visits_count_wide, by = c("subject_id", "index_date"), copy = T) %>% 
    CDMConnector::computeQuery()
  #%>%   mutate(total_visits = rowSums(select(., 8:ncol(.)), na.rm = TRUE)) # specialties start from 8th column - remember to change if needed
  
  ### summary for user only (subjects/visit= NA, not counted)
  not_in <- colnames(visits_count_wide)[(colnames(visits_count_wide)%in% specialty_names)]
  user_only_summary <- visits_count_wide %>%
    dplyr::mutate(exposed_yrs = as.numeric(follow_up_end - index_date)/ 365.25) %>% 
    pivot_longer(all_of(not_in), names_to = "specialty", values_to = "visits") %>% 
    dplyr::mutate(visits_per_year = visits / exposed_yrs) %>%
    dplyr::filter(visits > 0) %>%
    dplyr::group_by(specialty) %>%
    dplyr::summarise(
      tot_visits = sum(visits),
      tot_exposed_yrs = sum(exposed_yrs),
      mean_visits_per_year = round(tot_visits / tot_exposed_yrs, 2), # Manual calculation of mean
      sd_visits_per_year = round(sd(visits_per_year, na.rm = TRUE), 2),
      min_visits_per_year = round(min(visits_per_year, na.rm = TRUE), 2),
      max_visits_per_year = round(max(visits_per_year, na.rm = TRUE), 2),
      num_subjects_visited = n_distinct(subject_id),
      .groups = "drop"
    ) %>% 
    dplyr::ungroup() %>% 
    CDMConnector::computeQuery()
  
  ### summary for all subjects (subjects/visits = NA, treated as zero)
  all_summary <- visits_count_wide %>%
    dplyr::mutate(exposed_yrs = as.numeric(follow_up_end - index_date)/ 365.25) %>% 
    pivot_longer(all_of(not_in), names_to = "specialty", values_to = "visits") %>% 
    tidyr::complete(subject_id, specialty, fill = list(visits = 0)) %>% # filling missing visits with 0
    dplyr::mutate(visits_per_year = visits / exposed_yrs) %>%
    dplyr::group_by(specialty) %>%
    dplyr::summarise(
      tot_visits = sum(visits),
      tot_exposed_yrs = sum(exposed_yrs),
      mean_visits_per_year = round(tot_visits / tot_exposed_yrs, 2), # Manual calculation of mean
      sd_visits_per_year = round(sd(visits_per_year, na.rm = TRUE), 2),
      min_visits_per_year = round(min(visits_per_year, na.rm = TRUE), 2),
      max_visits_per_year = round(max(visits_per_year, na.rm = TRUE), 2),
      num_subjects_visited = n_distinct(subject_id)
    ) %>% 
    dplyr::ungroup() %>% 
    CDMConnector::computeQuery()
  
  # Calculating non-service users
  visits_count_wide_test <- visits_count_wide[(colnames(visits_count_wide)%in% specialty_names)]
  visits_count_wide_test[is.na(visits_count_wide_test)] <- 0
  visits_count_wide_test$row_sum <- rowSums(visits_count_wide_test)
  non_service_users <- visits_count_wide_test %>% 
    dplyr::filter(row_sum == 0) %>% 
    dplyr::tally() %>%
    dplyr::rename(nonservice=n)
  
  # Adding this so to rename specialty into type for Netherlands
  
  if (country_setting == "Netherlands") {
      user_only_summary <- user_only_summary %>% 
        rename (type = specialty)
      
      all_summary <- all_summary %>% 
        rename (type = specialty)
  }
  
  
  return(list(user_only_summary = user_only_summary, all_summary = all_summary, non_service_users=non_service_users, visits_count_wide=visits_count_wide))
}

# Estimate costs primary care visits 

analyse_visits_cost <- function(cohort_combined, visit_data) {
  
  ### Filtering visits based on the cohort_combined
  filtered_visits <- visit_data %>%
    dplyr::left_join(cohort_combined, by = "subject_id", relationship = "many-to-many", copy = T) %>%
    dplyr::filter(visit_detail_start_date >= index_date & visit_detail_start_date <= follow_up_end) %>%
    dplyr::group_by(subject_id, index_date, specialty, unit_cost) %>% # we group by index date to ensure each visit is associated with an entry
    dplyr::summarise(visit_count = n(), .groups = "drop") %>%
    dplyr::ungroup() %>% 
    CDMConnector::computeQuery()
  
  # create a new dataframe from excel
  
  provider_cost_inputs_2 <- provider_cost_inputs 
  
  # Improved conditional renaming based on the value of country_setting
  if(country_setting != "UK" & country_setting != "Italy"){
    provider_cost_inputs_2 <- provider_cost_inputs_2 %>%
      dplyr::rename(specialty = specialty_concept_id)
    # Join and create new names for specialties
    filtered_visits <- filtered_visits %>%
      dplyr::left_join(provider_cost_inputs_2, by = "specialty") %>%
      dplyr::mutate(specialty_temp = ifelse(is.na(description_athena), specialty, description_athena)) %>%
      dplyr::select(-description_athena, -specialty) %>% 
      dplyr::rename(specialty = specialty_temp)
  }

  ### Compute costs visits
  filtered_visits <- filtered_visits %>%  
    dplyr::mutate (visit_cost = visit_count * unit_cost) %>% 
    dplyr::select (-unit_cost, -visit_count) %>% 
    CDMConnector::computeQuery()
  
  ### Pivot the data
  visits_cost_wide <- filtered_visits %>%
    pivot_wider(names_from = specialty, values_from = visit_cost, values_fill = NA) %>% 
    CDMConnector::computeQuery()
  
  ### Join the wide dataframe back to cohort_combined and count tot num visits
  visits_cost_wide <- cohort_combined %>% 
    dplyr::left_join(visits_cost_wide, by = c("subject_id", "index_date"), copy = T) %>% 
    CDMConnector::computeQuery()
  
  ### summary for user only (subjects/visit= NA, not counted)
  not_in <- colnames(visits_cost_wide)[(colnames(visits_cost_wide)%in% specialty_names)]
  user_only_cost_summary <- visits_cost_wide %>%
    dplyr::mutate(exposed_yrs = as.numeric(follow_up_end - index_date)/ 365.25) %>% 
    pivot_longer(all_of(not_in), names_to = "specialty", values_to = "visits_costs") %>% 
    dplyr::mutate(visits_costs_per_year = visits_costs / exposed_yrs) %>%
    dplyr::filter(visits_costs > 0) %>%
    dplyr::group_by(specialty) %>%
    dplyr::summarise(
      tot_visits_costs = sum(visits_costs),
      tot_exposed_yrs = sum(exposed_yrs),
      mean_cost_visits_per_year = round(tot_visits_costs / tot_exposed_yrs, 2), # Manual calculation of mean
      sd_cost_visits_per_year = round(sd(visits_costs_per_year, na.rm = TRUE), 2),
      min_cost_visits_per_year = round(min(visits_costs_per_year, na.rm = TRUE), 2),
      max_cost_visits_per_year = round(max(visits_costs_per_year, na.rm = TRUE), 2),
      num_subjects_visited = n_distinct(subject_id),
      .groups = "drop"
    ) %>% 
    dplyr::ungroup() %>% 
    CDMConnector::computeQuery()
  
  
  ### summary for all subjects (subjects/visits = NA, treated as zero)
  all_cost_summary <-  visits_cost_wide %>%
    dplyr::mutate(exposed_yrs = as.numeric(follow_up_end - index_date)/ 365.25) %>% 
    pivot_longer(all_of(not_in), names_to = "specialty", values_to = "visits_costs") %>% 
    dplyr::mutate(visits_costs_per_year = visits_costs / exposed_yrs) %>%
    dplyr::group_by(specialty) %>%
    dplyr::mutate(visits_costs = round(visits_costs, digits = 2)) %>% 
    dplyr::summarise(
      tot_visits_costs = sum(visits_costs, na.rm = T),
      tot_exposed_yrs = sum(exposed_yrs),
      mean_cost_visits_per_year = round(tot_visits_costs / tot_exposed_yrs, 2), # Manual calculation of mean
      sd_cost_visits_per_year = round(sd(visits_costs_per_year, na.rm = TRUE), 2),
      min_cost_visits_per_year = round(min(visits_costs_per_year, na.rm = TRUE), 2),
      max_cost_visits_per_year = round(max(visits_costs_per_year, na.rm = TRUE), 2),
      num_subjects_visited = n_distinct(subject_id),
      .groups = "drop"
    )  %>% 
    dplyr::ungroup() %>% 
    CDMConnector::computeQuery()
  
  return(list(user_only_cost_summary = user_only_cost_summary, all_cost_summary = all_cost_summary))
}


# Cohort summary

cohort_summary <- function(data, cohort_name, non_service_users) {
  entries_per_woman <- data %>% 
    dplyr::mutate(exposed_yrs = as.numeric(follow_up_end - index_date)/ 365.25) %>% 
    dplyr::group_by(subject_id) %>% 
    dplyr::summarise(entries_per_woman = n(), 
                     tot_exposed_yrs = sum(exposed_yrs),
                     .groups = "drop") # Drop groups after summarising) 
  
  summary <- tibble(
    cohort = cohort_name,
    num_distinct_women = n_distinct(data$subject_id),
    num_entries = nrow(data),
    tot_exposed_yrs = sum(entries_per_woman$tot_exposed_yrs),
    mean_entries_per_woman = round(mean(entries_per_woman$entries_per_woman), 2),
    sd_entries_per_woman = round(sd(entries_per_woman$entries_per_woman), 2),
    min_entries_per_woman = min(entries_per_woman$entries_per_woman),
    max_entries_per_woman = max(entries_per_woman$entries_per_woman),
    num_non_service_users = unlist(non_service_users),
    perc_non_service_users = unlist(round((non_service_users / n_distinct(data$subject_id) * 100), 2)),
    num_service_users = num_distinct_women - num_non_service_users,
    perc_service_users = 100-perc_non_service_users,
    num_women_1_entry = sum(entries_per_woman$entries_per_woman == 1),
    num_women_2_entries = sum(entries_per_woman$entries_per_woman == 2),
    num_women_3plus_entries = sum(entries_per_woman$entries_per_woman >= 3)
  )
  
  return(summary)
}

### Xihang's greatest function written 
reformat_table_one_rq3<- function(table_one, period, name1, name2, j, k){
  group_level_selected <- c(paste0(as.character(period), " ", "target"),
                            paste0(as.character(period), " ", "cohort1"),
                            paste0(as.character(period), " ", "cohort2"))
  
  sub_table_one <- table_one %>% 
    dplyr::filter(group_level %in% group_level_selected)
  
  sub_table_one_list <- list()
  for (i in (1:3)){
    sub_table_one_list[[i]]<-sub_table_one %>% 
      dplyr::filter(group_level == group_level_selected[[i]])
  }
  table_one_1 <- sub_table_one_list[[j]]
  table_one_2 <- sub_table_one_list[[k]]
  reformatted_table1 <- data.frame(x = NA, y= NA, z = NA, mean1 = NA, sd1 = NA, mean2 = NA, sd2 = NA)
  n1 <- table_one_1 %>% dplyr::filter(variable == "Number subjects") %>% dplyr::pull(estimate)
  n2 <- table_one_2 %>% dplyr::filter(variable == "Number subjects") %>% dplyr::pull(estimate)
  
  # variables assembled by mean
  cont_var <- table_one_1 %>% dplyr::filter(estimate_type == "mean") %>% dplyr::select(variable) %>% dplyr::distinct() %>% dplyr::pull(variable)
  
  for (i in (1:length(cont_var))){
    reformatted_table1 <- rbind(reformatted_table1, 
                                data.frame(x = paste0(cont_var[[i]], ", mean (SD)"), 
                                           y = paste0(round(as.numeric(table_one_1 %>% dplyr::filter(variable == cont_var[[i]]) %>% dplyr::filter(estimate_type == "mean") %>% dplyr::pull(estimate)), digits = 1),
                                                      " (",
                                                      round(as.numeric(table_one_1 %>% dplyr::filter(variable == cont_var[[i]]) %>% dplyr::filter(estimate_type == "sd") %>% dplyr::pull(estimate)), digits = 1),
                                                      ")"),
                                           z = paste0(round(as.numeric(table_one_2 %>% dplyr::filter(variable == cont_var[[i]]) %>% dplyr::filter(estimate_type == "mean") %>% dplyr::pull(estimate)), digits = 1),
                                                      " (",
                                                      round(as.numeric(table_one_2 %>% dplyr::filter(variable == cont_var[[i]]) %>% dplyr::filter(estimate_type == "sd") %>% dplyr::pull(estimate)), digits = 1),
                                                      ")"),
                                           mean1 = table_one_1 %>% dplyr::filter(variable == cont_var[[i]]) %>% dplyr::filter(estimate_type == "mean") %>% dplyr::pull(estimate),
                                           sd1 = table_one_1 %>% dplyr::filter(variable == cont_var[[i]]) %>% dplyr::filter(estimate_type == "sd") %>% dplyr::pull(estimate),
                                           mean2 = table_one_2 %>% dplyr::filter(variable == cont_var[[i]]) %>% dplyr::filter(estimate_type == "mean") %>% dplyr::pull(estimate),
                                           sd2 = table_one_2 %>% dplyr::filter(variable == cont_var[[i]]) %>% dplyr::filter(estimate_type == "sd") %>% dplyr::pull(estimate)
                                )
    )
  }
  reformatted_table1 <- reformatted_table1 %>% 
    dplyr::mutate(smd = (as.numeric(mean1)-as.numeric(mean2))/(sqrt(1/2*(as.numeric(sd1)^2+as.numeric(sd2)^2)))) %>% 
    dplyr::select(x,y,z,smd)
  
  reformatted_table1 <- reformatted_table1[-1, ]
  
  #variables assembled by percentage
  cont_var <- table_one_1 %>% 
    dplyr::filter(estimate_type == "percentage") %>% 
    filter(!variable == "Sex") %>% 
    dplyr::select(variable_level) %>% 
    dplyr::distinct() %>% 
    dplyr::pull(variable_level)  
  
  reformatted_table1_2<-data.frame(x = NA, y= NA, z= NA, percentage1 = NA, percentage2 = NA)
  for (i in (1:length(cont_var))){
    reformatted_table1_2 <- rbind(reformatted_table1_2, data.frame(x = paste0(cont_var[[i]], ", n(%)"),
                                                                   y = paste0(table_one_1 %>% dplyr::filter(variable_level == cont_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
                                                                              " (",
                                                                              round(as.numeric(table_one_1 %>% dplyr::filter(variable_level == cont_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate)), digits = 1),
                                                                              ")"),
                                                                   z = paste0(table_one_2 %>% dplyr::filter(variable_level == cont_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
                                                                              " (",
                                                                              round(as.numeric(table_one_2 %>% dplyr::filter(variable_level == cont_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate)), digits = 1),
                                                                              ")"),
                                                                   percentage1 = (1/100)*as.numeric(table_one_1 %>% dplyr::filter(variable_level == cont_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate)),
                                                                   percentage2 = (1/100)*as.numeric(table_one_2 %>% dplyr::filter(variable_level == cont_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate))))
  }
  
  reformatted_table1_2 <- reformatted_table1_2 %>% 
    dplyr::mutate(smd = (percentage1-percentage2)/(sqrt((1/2)*(percentage1*(1-percentage1)+percentage2*(1-percentage2))))) %>% 
    dplyr::select(x,y,z, smd)
  
  reformatted_table1_2 <- reformatted_table1_2[-1, ]
  
  reformatted_table1 <- rbind(reformatted_table1, reformatted_table1_2)
  reformatted_table1 <- reformatted_table1 %>% 
    dplyr::mutate(smd = round(abs(smd), digits = 3))
  ###rename columns
  colnames(reformatted_table1) <- c(
    "Characteristic",
    paste0(name1, " (n = ", n1, ")"),
    paste0(name2, " (n = ", n2, ")"),
    "SMD"
  )
  return(reformatted_table1)
}
