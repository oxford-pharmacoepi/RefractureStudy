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
    dplyr::filter(variable == "Conditions flag from any time prior to 0") %>%
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
    dplyr::filter(stringr::str_detect(variable, 'Medications')) %>%
    dplyr::filter(!stringr::str_detect(variable, 'antineoplastic agents')) %>%
    dplyr::select(variable_level) %>%
    dplyr::distinct() %>%
    dplyr::pull(variable_level)
  
  for (i in (1:length(medication_var))){
    reformatted_table1 <- rbind(reformatted_table1, data.frame(x = paste0(medication_var[[i]], ", n(%)"),
                                                               y = paste0(table_one_frac %>% dplyr::filter(variable_level == medication_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
                                                                          " (",
                                                                          round(as.numeric(table_one_frac %>% dplyr::filter(variable_level == medication_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate)), digits = 1),
                                                                          ")"),
                                                               z = paste0(table_one_imm %>% dplyr::filter(variable_level == medication_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
                                                                          " (",
                                                                          round(as.numeric(table_one_imm %>% dplyr::filter(variable_level == medication_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate)), digits = 1),
                                                                          ")"),
                                                               w = paste0(table_one_no_imm %>% dplyr::filter(variable_level == medication_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
                                                                          " (",
                                                                          round(as.numeric(table_one_no_imm %>% dplyr::filter(variable_level == medication_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate)), digits = 1),
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
  #nether = visit_start_date + type
  #rest = visit_start_date
  if(country_setting == "UK"){
    filtered_visits <- visit_data %>%
      dplyr::left_join(cohort_combined, by = "subject_id", relationship = "many-to-many", copy = T) %>%
      dplyr::filter(visit_detail_start_date >= index_date & visit_detail_start_date <= follow_up_end) %>%
      dplyr::group_by(subject_id, index_date, specialty) %>% # we group by index date to ensure each visit is associated with an entry
      dplyr::summarise(visit_count = n(), .groups = "drop") %>%
      dplyr::ungroup() %>% 
      CDMConnector::computeQuery()
  } else if(country_setting == "Netherlands"){
    filtered_visits <- visit_data %>%
      dplyr::left_join(cohort_combined, by = "subject_id", relationship = "many-to-many", copy = T) %>%
      dplyr::filter(visit_start_date >= index_date & visit_start_date <= follow_up_end) %>%
      dplyr::group_by(subject_id, index_date, type) %>% # we group by index date to ensure each visit is associated with an entry
      dplyr::summarise(visit_count = n(), .groups = "drop") %>%
      dplyr::ungroup() %>% 
      CDMConnector::computeQuery()
  } else {
    filtered_visits <- visit_data %>%
      dplyr::left_join(cohort_combined, by = "subject_id", relationship = "many-to-many", copy = T) %>%
      dplyr::filter(visit_start_date >= index_date & visit_start_date <= follow_up_end) %>%
      dplyr::group_by(subject_id, index_date, specialty) %>% # we group by index date to ensure each visit is associated with an entry
      dplyr::summarise(visit_count = n(), .groups = "drop") %>%
      dplyr::ungroup() %>% 
      CDMConnector::computeQuery()
  }
  
  # Initialize provider_cost_inputs_2
  provider_cost_inputs_2 <- provider_cost_inputs
  
  if(country_setting == "Netherlands"){
    provider_cost_inputs_2 <- provider_cost_inputs_2 %>%
      dplyr::rename(specialty = visit_concept_id) %>%
      dplyr::select(specialty, description_athena)
    # Join and create new names for specialties
    filtered_visits <- filtered_visits %>%
      dplyr::rename(specialty = type) %>%
      dplyr::left_join(provider_cost_inputs_2, by = "specialty", copy = T) %>%
      dplyr::mutate(specialty = as.character(specialty),
                    specialty_temp = ifelse(is.na(description_athena), specialty, description_athena)) %>%
      dplyr::select(-description_athena, -specialty) %>% 
      dplyr::rename(specialty = specialty_temp) %>% 
      CDMConnector::computeQuery()
  } else if(country_setting != "UK" & country_setting != "Italy"){
    provider_cost_inputs_2 <- provider_cost_inputs_2 %>%
      dplyr::rename(specialty = specialty_concept_id)
    # Join and create new names for specialties
    filtered_visits <- filtered_visits %>%
      dplyr::left_join(provider_cost_inputs_2, by = "specialty", copy = T) %>%
      dplyr::mutate(specialty = as.character(specialty),
                    specialty_temp = ifelse(is.na(description_athena), specialty, description_athena)) %>%
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

  ### summary for user only (subjects/visit= NA, not counted)
  included_cols <- colnames(visits_count_wide)[(colnames(visits_count_wide)%in% specialty_names)]
  tot_exposed_yrs_all <- visits_count_wide %>%
    dplyr::mutate(exposed_yrs = as.numeric(follow_up_end - index_date)/ 365.25) %>% 
    dplyr::summarise(tot_exposed_yrs = sum(exposed_yrs)) %>% 
    dplyr::pull(tot_exposed_yrs)
  
  tot_exposed_yrs_user <- visits_count_wide %>%
    dplyr::mutate(exposed_yrs = as.numeric(follow_up_end - index_date)/ 365.25) %>% 
    pivot_longer(all_of(included_cols), names_to = "specialty", values_to = "visits") %>% 
    dplyr::mutate(visits_per_year = visits / exposed_yrs) %>%
    dplyr::filter(visits > 0) %>% 
    dplyr::group_by(subject_id, index_date) %>% 
    dplyr::filter(row_number()==1) %>% 
    dplyr::ungroup() %>% 
    dplyr::summarise(tot_exposed_yrs = sum(exposed_yrs)) %>% 
    dplyr::pull(tot_exposed_yrs)
  
  user_only_summary <- visits_count_wide %>%
    dplyr::mutate(exposed_yrs = as.numeric(follow_up_end - index_date)/ 365.25) %>% 
    pivot_longer(all_of(included_cols), names_to = "specialty", values_to = "visits") %>% 
    dplyr::mutate(visits_per_year = visits / exposed_yrs) %>%
    dplyr::filter(visits > 0) %>%
    dplyr::group_by(specialty) %>%
    dplyr::summarise(
      tot_visits = sum(visits),
      mean_visits_per_year = signif(tot_visits / tot_exposed_yrs_user, 4), # Manual calculation of mean
      sd_visits_per_year = signif(sd(visits_per_year, na.rm = TRUE), 4),
      min_visits_per_year = signif(min(visits_per_year, na.rm = TRUE), 4),
      max_visits_per_year = signif(max(visits_per_year, na.rm = TRUE), 4),
      num_subjects_visited = n_distinct(subject_id),
      .groups = "drop"
    ) %>% 
    dplyr::ungroup() %>% 
    CDMConnector::computeQuery() 
  
  ### summary for all subjects (subjects/visits = NA, treated as zero)
  all_summary <- visits_count_wide %>%
    dplyr::mutate(exposed_yrs = as.numeric(follow_up_end - index_date)/ 365.25) %>% 
    pivot_longer(all_of(included_cols), names_to = "specialty", values_to = "visits") %>% 
    tidyr::complete(subject_id, specialty, fill = list(visits = 0)) %>% # filling missing visits with 0
    dplyr::mutate(visits_per_year = visits / exposed_yrs) %>%
    dplyr::group_by(specialty) %>%
    dplyr::summarise(
      tot_visits = sum(visits),
      mean_visits_per_year = signif(tot_visits / tot_exposed_yrs_all, 4), # Manual calculation of mean
      sd_visits_per_year = signif(sd(visits_per_year, na.rm = TRUE), 4),
      min_visits_per_year = signif(min(visits_per_year, na.rm = TRUE), 4),
      max_visits_per_year = signif(max(visits_per_year, na.rm = TRUE), 4),
      num_subjects_visited = n_distinct(subject_id),
      .groups = "drop"
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
  
  return(list(user_only_summary = user_only_summary, all_summary = all_summary, non_service_users=non_service_users, visits_count_wide=visits_count_wide, tot_exposed_yrs_all=tot_exposed_yrs_all, tot_exposed_yrs_user = tot_exposed_yrs_user))
}

# Estimate costs primary care visits 

analyse_visits_cost <- function(cohort_combined, visit_data) {
  
  ### Filtering visits based on the cohort_combined
  if (country_setting == "UK"){
    filtered_visits <- visit_data %>%
      dplyr::left_join(cohort_combined, by = "subject_id", relationship = "many-to-many", copy = T) %>%
      dplyr::filter(visit_detail_start_date >= index_date & visit_detail_start_date <= follow_up_end) %>%
      dplyr::group_by(subject_id, index_date, specialty, unit_cost) %>% # we group by index date to ensure each visit is associated with an entry
      dplyr::summarise(visit_count = n(), .groups = "drop") %>%
      dplyr::ungroup() %>% 
      CDMConnector::computeQuery()
  } else {
    filtered_visits <- visit_data %>%
      dplyr::left_join(cohort_combined, by = "subject_id", relationship = "many-to-many", copy = T) %>%
      dplyr::filter(visit_start_date >= index_date & visit_start_date <= follow_up_end) %>%
      dplyr::group_by(subject_id, index_date, specialty, unit_cost) %>% # we group by index date to ensure each visit is associated with an entry
      dplyr::summarise(visit_count = n(), .groups = "drop") %>%
      dplyr::ungroup() %>% 
      CDMConnector::computeQuery()
  }
  
  # create a new dataframe from excel
  
  provider_cost_inputs_2 <- provider_cost_inputs 
  
  # Improved conditional renaming based on the value of country_setting
  if(country_setting != "UK" & country_setting != "Italy"){
    provider_cost_inputs_2 <- provider_cost_inputs_2 %>%
      dplyr::rename(specialty = specialty_concept_id)
    # Join and create new names for specialties
    filtered_visits <- filtered_visits %>%
      dplyr::left_join(provider_cost_inputs_2, by = "specialty", copy = T) %>%
      dplyr::mutate(specialty = as.character(specialty),
                    specialty_temp = ifelse(is.na(description_athena), specialty, description_athena))%>%
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
    pivot_wider(names_from = specialty, values_from = visit_cost, values_fill = 0) %>% 
    CDMConnector::computeQuery()
  
  ### Join the wide dataframe back to cohort_combined and count tot num visits
  visits_cost_wide <- cohort_combined %>% 
    dplyr::left_join(visits_cost_wide, by = c("subject_id", "index_date"), copy = T) %>% 
    CDMConnector::computeQuery()
  
  ### summary for user only (subjects/visit= NA, not counted)
  included_cols <- colnames(visits_cost_wide)[(colnames(visits_cost_wide)%in% specialty_names)]
  tot_exposed_yrs_all <- visits_cost_wide %>%
    dplyr::mutate(exposed_yrs = as.numeric(follow_up_end - index_date)/ 365.25) %>% 
    dplyr::summarise(tot_exposed_yrs = sum(exposed_yrs)) %>% 
    dplyr::pull(tot_exposed_yrs)
  
  tot_exposed_yrs_user <- visits_cost_wide %>%
    dplyr::mutate(exposed_yrs = as.numeric(follow_up_end - index_date)/ 365.25) %>% 
    pivot_longer(all_of(included_cols), names_to = "specialty", values_to = "visits") %>% 
    dplyr::mutate(visits_per_year = visits / exposed_yrs) %>%
    dplyr::filter(visits > 0) %>% 
    dplyr::group_by(subject_id, index_date) %>% 
    dplyr::filter(row_number()==1) %>% 
    dplyr::ungroup() %>% 
    dplyr::summarise(tot_exposed_yrs = sum(exposed_yrs)) %>% 
    dplyr::pull(tot_exposed_yrs)
  
  user_only_cost_summary <- visits_cost_wide %>%
    dplyr::mutate(exposed_yrs = as.numeric(follow_up_end - index_date)/ 365.25) %>% 
    pivot_longer(all_of(included_cols), names_to = "specialty", values_to = "visits_costs") %>% 
    dplyr::mutate(visits_costs_per_year = visits_costs / exposed_yrs) %>%
    dplyr::filter(visits_costs > 0) %>%
    dplyr::group_by(specialty) %>%
    dplyr::summarise(
      tot_visits_costs = sum(visits_costs),
      mean_cost_visits_per_year = signif(tot_visits_costs / tot_exposed_yrs_user, 4), # Manual calculation of mean
      sd_cost_visits_per_year = signif(sd(visits_costs_per_year, na.rm = TRUE), 4),
      min_cost_visits_per_year = signif(min(visits_costs_per_year, na.rm = TRUE), 4),
      max_cost_visits_per_year = signif(max(visits_costs_per_year, na.rm = TRUE), 4),
      num_subjects_visited = n_distinct(subject_id),
      .groups = "drop"
    ) %>% 
    dplyr::ungroup() %>% 
    CDMConnector::computeQuery()
  
  ### summary for all subjects (subjects/visits = NA, treated as zero)
  all_cost_summary <-  visits_cost_wide %>%
    dplyr::mutate(exposed_yrs = as.numeric(follow_up_end - index_date)/ 365.25) %>% 
    pivot_longer(all_of(included_cols), names_to = "specialty", values_to = "visits_costs") %>% 
    dplyr::mutate(visits_costs_per_year = visits_costs / exposed_yrs) %>%
    dplyr::group_by(specialty) %>%
    dplyr::mutate(visits_costs = signif(visits_costs, digits = 2)) %>% 
    dplyr::summarise(
      tot_visits_costs = sum(visits_costs, na.rm = T),
      mean_cost_visits_per_year = signif(tot_visits_costs / tot_exposed_yrs_all, 4), # Manual calculation of mean
      sd_cost_visits_per_year = signif(sd(visits_costs_per_year, na.rm = TRUE), 4),
      min_cost_visits_per_year = signif(min(visits_costs_per_year, na.rm = TRUE), 4),
      max_cost_visits_per_year = signif(max(visits_costs_per_year, na.rm = TRUE), 4),
      num_subjects_visited = n_distinct(subject_id),
      .groups = "drop"
    )  %>% 
    dplyr::ungroup() %>% 
    CDMConnector::computeQuery()
  
  return(list(user_only_cost_summary = user_only_cost_summary, all_cost_summary = all_cost_summary, visits_cost_wide=visits_cost_wide, tot_exposed_yrs_all=tot_exposed_yrs_all, tot_exposed_yrs_user=tot_exposed_yrs_user))
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
    mean_entries_per_woman = signif(mean(entries_per_woman$entries_per_woman), 4),
    sd_entries_per_woman = signif(sd(entries_per_woman$entries_per_woman), 4),
    min_entries_per_woman = min(entries_per_woman$entries_per_woman),
    max_entries_per_woman = max(entries_per_woman$entries_per_woman),
    num_non_service_users = unlist(non_service_users),
    perc_non_service_users = unlist(signif((non_service_users / n_distinct(data$subject_id) * 100), 4)),
    num_service_users = num_distinct_women - num_non_service_users,
    perc_service_users = 100-perc_non_service_users,
    num_women_1_entry = sum(entries_per_woman$entries_per_woman == 1),
    num_women_2_entries = sum(entries_per_woman$entries_per_woman == 2),
    num_women_3plus_entries = sum(entries_per_woman$entries_per_woman >= 3)
  )
  
  return(summary)
}

### Xihang's greatest functions written 
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
                                           mean1 = round(as.numeric(table_one_1 %>% dplyr::filter(variable == cont_var[[i]]) %>% dplyr::filter(estimate_type == "mean") %>% dplyr::pull(estimate)), digits = 8),
                                           sd1 = round(as.numeric(table_one_1 %>% dplyr::filter(variable == cont_var[[i]]) %>% dplyr::filter(estimate_type == "sd") %>% dplyr::pull(estimate)), digits = 8), 
                                           mean2 = round(as.numeric(table_one_2 %>% dplyr::filter(variable == cont_var[[i]]) %>% dplyr::filter(estimate_type == "mean") %>% dplyr::pull(estimate)), digits = 8),
                                           sd2 = round(as.numeric(table_one_2 %>% dplyr::filter(variable == cont_var[[i]]) %>% dplyr::filter(estimate_type == "sd") %>% dplyr::pull(estimate)), digits = 8)
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
    dplyr::filter(!variable == "Sex") %>% 
    dplyr::select(variable_level) %>% 
    dplyr::distinct() %>% 
    dplyr::filter(!is.na(variable_level)) %>% 
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

reformat_table_one_rq3_01<- function(table_one, period, name1, name2){
  group_level_selected <- c(paste0(as.character(period), " target"),
                            paste0(as.character(period+tot_periods_target), " comparator 1"))
  
  sub_table_one <- table_one %>% 
    dplyr::filter(group_level %in% group_level_selected)
  
  sub_table_one_list <- list()
  for (i in (1:2)){
    sub_table_one_list[[i]]<-sub_table_one %>% 
      dplyr::filter(group_level == group_level_selected[[i]])
  }
  table_one_1 <- sub_table_one_list[[1]]
  table_one_2 <- sub_table_one_list[[2]]
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
                                           mean1 = round(as.numeric(table_one_1 %>% dplyr::filter(variable == cont_var[[i]]) %>% dplyr::filter(estimate_type == "mean") %>% dplyr::pull(estimate)), digits = 8),
                                           sd1 = round(as.numeric(table_one_1 %>% dplyr::filter(variable == cont_var[[i]]) %>% dplyr::filter(estimate_type == "sd") %>% dplyr::pull(estimate)), digits = 8), 
                                           mean2 = round(as.numeric(table_one_2 %>% dplyr::filter(variable == cont_var[[i]]) %>% dplyr::filter(estimate_type == "mean") %>% dplyr::pull(estimate)), digits = 8),
                                           sd2 = round(as.numeric(table_one_2 %>% dplyr::filter(variable == cont_var[[i]]) %>% dplyr::filter(estimate_type == "sd") %>% dplyr::pull(estimate)), digits = 8)
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

reformat_table_one_rq3_12<- function(table_one, period, name1, name2){
  group_level_selected <- c(paste0(as.character(period), " comparator 1"),
                            paste0(as.character(period+tot_periods_c1), " comparator 2"))
  
  sub_table_one <- table_one %>% 
    dplyr::filter(group_level %in% group_level_selected)
  
  sub_table_one_list <- list()
  for (i in (1:2)){
    sub_table_one_list[[i]]<-sub_table_one %>% 
      dplyr::filter(group_level == group_level_selected[[i]])
  }
  table_one_1 <- sub_table_one_list[[1]]
  table_one_2 <- sub_table_one_list[[2]]
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
                                           mean1 = round(as.numeric(table_one_1 %>% dplyr::filter(variable == cont_var[[i]]) %>% dplyr::filter(estimate_type == "mean") %>% dplyr::pull(estimate)), digits = 8),
                                           sd1 = round(as.numeric(table_one_1 %>% dplyr::filter(variable == cont_var[[i]]) %>% dplyr::filter(estimate_type == "sd") %>% dplyr::pull(estimate)), digits = 8), 
                                           mean2 = round(as.numeric(table_one_2 %>% dplyr::filter(variable == cont_var[[i]]) %>% dplyr::filter(estimate_type == "mean") %>% dplyr::pull(estimate)), digits = 8),
                                           sd2 = round(as.numeric(table_one_2 %>% dplyr::filter(variable == cont_var[[i]]) %>% dplyr::filter(estimate_type == "sd") %>% dplyr::pull(estimate)), digits = 8)
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

reformat_table_one_rq3_across<- function(table_one, name1, name2){
  group_level_selected <- c(name1, name2)
  
  sub_table_one <- table_one %>% 
    dplyr::filter(group_level %in% group_level_selected)
  
  sub_table_one_list <- list()
  for (i in (1:length(group_level_selected))){
    sub_table_one_list[[i]]<-sub_table_one %>% 
      dplyr::filter(group_level == group_level_selected[[i]])
  }
  table_one_1 <- sub_table_one_list[[1]]
  table_one_2 <- sub_table_one_list[[2]]
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

###conditions
condition_frequency_table <- function(cohort_freq, table_name, primary = F){
  if (primary== F){
    freq_condition_tbl <- cohort_freq %>% 
      dplyr::inner_join(cdm[[table_name]] %>% dplyr::select(person_id, visit_detail_concept_id, visit_detail_start_date, visit_detail_end_date),
                        by = c("subject_id" = "person_id"),
                        copy = T,
                        relationship = "many-to-many") %>%
      dplyr::inner_join(cdm[["condition_occurrence_hes"]] %>% dplyr::select(person_id, condition_source_value, condition_start_date),
                        by = c("subject_id" = "person_id"),
                        copy = T,
                        relationship = "many-to-many") %>%
      dplyr::filter(condition_start_date >=visit_detail_start_date & condition_start_date <= visit_detail_end_date) %>%
      dplyr::filter(condition_start_date >=index_date & condition_start_date <= follow_up_end) %>%
      dplyr::distinct() %>% 
      dplyr::mutate(LoS = .data$visit_detail_end_date - .data$visit_detail_start_date + 1)
    
    tot_episodes <- nrow(freq_condition_tbl)
    
    multiple <- freq_condition_tbl %>% 
      dplyr::group_by(subject_id, index_date, condition_start_date) %>%
      dplyr::tally() %>% 
      dplyr::filter(n>=2) %>% 
      dplyr::ungroup() %>% 
      dplyr::summarise(sum_of_conditions_more_than_one = sum(n)) %>% 
      dplyr::mutate(tot_conditions = nrow(freq_condition_tbl)) %>% 
      dplyr::mutate(more_than_one_perc = round(sum_of_conditions_more_than_one/nrow(freq_condition_tbl)*100, 2))
    
    freq_condition <- freq_condition_tbl %>% 
      dplyr::group_by(condition_source_value) %>%
      dplyr::summarise(mean_los = mean(.data$LoS),
                       counts = n()) %>% 
      dplyr::ungroup() %>% 
      dplyr::arrange(desc(counts)) %>% 
      dplyr::mutate(percentage = counts/tot_episodes) %>% 
      dplyr::mutate(percentage = round(percentage*100, digits = 2),
                    mean_los = round(mean_los, digits = 2)) 
    
    summary_LoS <- tibble(mean_length_of_stay_per_condition = as.integer(sum(freq_condition_tbl$LoS))/tot_episodes,
                          min_length_of_stay_per_condition = min(freq_condition$mean_los),
                          max_length_of_stay_per_condition = max(freq_condition$mean_los),
                          sd_length_of_stay_per_condition = round(sd(freq_condition$mean_los), 2),
                          median_length_of_stay_per_condition = round(quantile(freq_condition$mean_los, probs = (.5)), 2),
                          lower_q_length_of_stay_per_condition = round(quantile(freq_condition$mean_los, probs = (.25)), 2),
                          upper_q_length_of_stay_per_condition = round(quantile(freq_condition$mean_los, probs = (.75)), 2))
    
    episode_per_person <- freq_condition_tbl %>% 
      dplyr::group_by(subject_id, index_date, exposed_yrs) %>% 
      dplyr::summarise(counts = n()) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(counts_per_yr = counts/exposed_yrs)
    
    summary_episode_per_person_per_year <- tibble(mean_condition_episode_per_person_per_year = sum(episode_per_person$counts)/sum(episode_per_person$exposed_yrs),
                                                  min_condition_episode_per_person_per_year = min(episode_per_person$counts_per_yr),
                                                  max_condition_episode_per_person_per_year = max(episode_per_person$counts_per_yr),
                                                  sd_condition_episode_per_person_per_year = round(sd(episode_per_person$counts_per_yr), 2),
                                                  median_condition_episode_per_person_per_year = round(quantile(episode_per_person$counts_per_yr, probs = (.5)), 2),
                                                  lower_q_condition_episode_per_person_per_year = round(quantile(episode_per_person$counts_per_yr, probs = (.25)), 2),
                                                  upper_q_condition_episode_per_person_per_year = round(quantile(episode_per_person$counts_per_yr, probs = (.75)), 2))
    
    
    freq_condition <- freq_condition %>% 
      dplyr::mutate(mean_los = ifelse((counts < 5 & counts > 0), paste0("<", minimum_counts), mean_los),
                    percentage = ifelse((counts < 5 & counts > 0), paste0("<", minimum_counts), percentage),
                    counts = ifelse((counts < 5 & counts > 0), paste0("<", minimum_counts), counts))
  }
  
  else {
    freq_condition_tbl <- cohort_freq %>% 
      dplyr::inner_join(cdm[[table_name]] %>% dplyr::select(person_id, visit_detail_concept_id, visit_detail_start_date, visit_detail_end_date),
                        by = c("subject_id" = "person_id"),
                        copy = T,
                        relationship = "many-to-many") %>%
      dplyr::inner_join(cdm[["condition_occurrence_hes"]] %>% dplyr::filter(condition_status_source_value == "1") %>% dplyr::select(person_id, condition_source_value, condition_start_date),
                        by = c("subject_id" = "person_id"),
                        copy = T,
                        relationship = "many-to-many") %>%
      dplyr::filter(condition_start_date >=visit_detail_start_date & condition_start_date <= visit_detail_end_date) %>%
      dplyr::filter(condition_start_date >=index_date & condition_start_date <= follow_up_end) %>%
      dplyr::distinct() %>% 
      dplyr::mutate(LoS = .data$visit_detail_end_date - .data$visit_detail_start_date + 1)
    
    tot_episodes <- nrow(freq_condition_tbl)
    
    multiple <- freq_condition_tbl %>% 
      dplyr::group_by(subject_id, index_date, condition_start_date) %>%
      dplyr::tally() %>% 
      dplyr::filter(n>=2) %>% 
      dplyr::ungroup() %>% 
      dplyr::summarise(sum_of_conditions_more_than_one = sum(n)) %>% 
      dplyr::mutate(tot_conditions = nrow(freq_condition_tbl)) %>% 
      dplyr::mutate(more_than_one_perc = round(sum_of_conditions_more_than_one/nrow(freq_condition_tbl)*100, 2))
    
    freq_condition <- freq_condition_tbl %>% 
      dplyr::group_by(condition_source_value) %>%
      dplyr::summarise(mean_los = mean(.data$LoS),
                       counts = n()) %>% 
      dplyr::ungroup() %>% 
      dplyr::arrange(desc(counts)) %>% 
      dplyr::mutate(percentage = counts/tot_episodes) %>% 
      dplyr::mutate(percentage = round(percentage*100, digits = 2),
                    mean_los = round(mean_los, digits = 2)) 
    
    summary_LoS <- tibble(mean_length_of_stay_per_condition=as.integer(sum(freq_condition_tbl$LoS))/tot_episodes,
                          min_length_of_stay_per_condition = min(freq_condition$mean_los),
                          max_length_of_stay_per_condition = max(freq_condition$mean_los),
                          sd_length_of_stay_per_condition = round(sd(freq_condition$mean_los), 2),
                          median_length_of_stay_per_condition = round(quantile(freq_condition$mean_los, probs = (.5)), 2),
                          lower_q_length_of_stay_per_condition = round(quantile(freq_condition$mean_los, probs = (.25)), 2),
                          upper_q_length_of_stay_per_condition = round(quantile(freq_condition$mean_los, probs = (.75)), 2))
    
    
    episode_per_person <- freq_condition_tbl %>% 
      dplyr::group_by(subject_id, index_date, exposed_yrs) %>% 
      dplyr::summarise(counts = n()) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(counts_per_yr = counts/exposed_yrs)
    
    summary_episode_per_person_per_year <- tibble(mean_condition_episode_per_person_per_year = sum(episode_per_person$counts)/sum(episode_per_person$exposed_yrs),
                                                  min_condition_episode_per_person_per_year = min(episode_per_person$counts_per_yr),
                                                  max_condition_episode_per_person_per_year = max(episode_per_person$counts_per_yr),
                                                  sd_condition_episode_per_person_per_year = round(sd(episode_per_person$counts_per_yr), 2),
                                                  median_condition_episode_per_person_per_year = round(quantile(episode_per_person$counts_per_yr, probs = (.5)), 2),
                                                  lower_q_condition_episode_per_person_per_year = round(quantile(episode_per_person$counts_per_yr, probs = (.25)), 2),
                                                  upper_q_condition_episode_per_person_per_year = round(quantile(episode_per_person$counts_per_yr, probs = (.75)), 2))
    
    freq_condition <- freq_condition %>% 
      dplyr::mutate(mean_los = ifelse((counts < 5 & counts > 0), paste0("<", minimum_counts), mean_los),
                    percentage = ifelse((counts < 5 & counts > 0), paste0("<", minimum_counts), percentage),
                    counts = ifelse((counts < 5 & counts > 0), paste0("<", minimum_counts), counts))
  }
  return(list(freq_condition = freq_condition, number_of_episodes = tot_episodes, more_than_one = multiple, summary_LoS = summary_LoS, summary_epi_per_person_per_yr = summary_episode_per_person_per_year))
}

###procedures
procedure_frequency_table <- function(cohort_freq, table_name, primary = F){
  if (primary== F){
    freq_procedure_tbl <- cohort_freq %>% 
      dplyr::inner_join(cdm[[table_name]] %>% dplyr::select(person_id, visit_detail_concept_id, visit_detail_start_date, visit_detail_end_date),
                        by = c("subject_id" = "person_id"),
                        copy = T,
                        relationship = "many-to-many") %>%
      dplyr::inner_join(cdm[["procedure_occurrence_hes"]] %>% dplyr::select(person_id, procedure_source_value, procedure_date),
                        by = c("subject_id" = "person_id"),
                        copy = T,
                        relationship = "many-to-many") %>%
      dplyr::filter(procedure_date >=visit_detail_start_date & procedure_date <= visit_detail_end_date) %>%
      dplyr::filter(procedure_date >=index_date & procedure_date <= follow_up_end) %>%
      dplyr::distinct() %>% 
      dplyr::mutate(LoS = .data$visit_detail_end_date - .data$visit_detail_start_date + 1)
    
    tot_episodes <- nrow(freq_procedure_tbl)
    
    multiple <- freq_procedure_tbl %>% 
      dplyr::group_by(subject_id, index_date, procedure_date) %>%
      dplyr::tally() %>% 
      dplyr::filter(n>=2) %>% 
      dplyr::ungroup() %>% 
      dplyr::summarise(sum_of_procedures_more_than_one = sum(n)) %>% 
      dplyr::mutate(tot_procedures = nrow(freq_procedure_tbl)) %>% 
      dplyr::mutate(more_than_one_perc = round(sum_of_procedures_more_than_one/nrow(freq_procedure_tbl)*100, 2))
    
    freq_procedure <- freq_procedure_tbl %>% 
      dplyr::group_by(procedure_source_value) %>%
      dplyr::summarise(mean_los = mean(.data$LoS),
                       counts = n()) %>% 
      dplyr::ungroup() %>% 
      dplyr::arrange(desc(counts)) %>% 
      dplyr::mutate(percentage = counts/tot_episodes) %>% 
      dplyr::mutate(percentage = round(percentage*100, digits = 2),
                    mean_los = round(mean_los, digits = 2)) 
    
    summary_LoS <- tibble(mean_length_of_stay_per_procedure = as.integer(sum(freq_procedure_tbl$LoS))/tot_episodes,
                          min_length_of_stay_per_procedure = min(freq_procedure$mean_los),
                          max_length_of_stay_per_procedure = max(freq_procedure$mean_los),
                          sd_length_of_stay_per_procedure = round(sd(freq_procedure$mean_los), 2),
                          median_length_of_stay_per_procedure = round(quantile(freq_procedure$mean_los, probs = (.5)), 2),
                          lower_q_length_of_stay_per_procedure = round(quantile(freq_procedure$mean_los, probs = (.25)), 2),
                          upper_q_length_of_stay_per_procedure = round(quantile(freq_procedure$mean_los, probs = (.75)), 2))
    
    episode_per_person <- freq_procedure_tbl %>% 
      dplyr::group_by(subject_id, index_date, exposed_yrs) %>% 
      dplyr::summarise(counts = n()) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(counts_per_yr = counts/exposed_yrs)
    
    summary_episode_per_person_per_year <- tibble(mean_procedure_episode_per_person_per_year = sum(episode_per_person$counts)/sum(episode_per_person$exposed_yrs),
                                                  min_procedure_episode_per_person_per_year = min(episode_per_person$counts_per_yr),
                                                  max_procedure_episode_per_person_per_year = max(episode_per_person$counts_per_yr),
                                                  sd_procedure_episode_per_person_per_year = round(sd(episode_per_person$counts_per_yr), 2),
                                                  median_procedure_episode_per_person_per_year = round(quantile(episode_per_person$counts_per_yr, probs = (.5)), 2),
                                                  lower_q_procedure_episode_per_person_per_year = round(quantile(episode_per_person$counts_per_yr, probs = (.25)), 2),
                                                  upper_q_procedure_episode_per_person_per_year = round(quantile(episode_per_person$counts_per_yr, probs = (.75)), 2))
    
    
    freq_procedure <- freq_procedure %>% 
      dplyr::mutate(mean_los = ifelse((counts < 5 & counts > 0), paste0("<", minimum_counts), mean_los),
                    percentage = ifelse((counts < 5 & counts > 0), paste0("<", minimum_counts), percentage),
                    counts = ifelse((counts < 5 & counts > 0), paste0("<", minimum_counts), counts))
  }
  
  else {
    freq_procedure_tbl <- cohort_freq %>% 
      dplyr::inner_join(cdm[[table_name]] %>% dplyr::select(person_id, visit_detail_concept_id, visit_detail_start_date, visit_detail_end_date),
                        by = c("subject_id" = "person_id"),
                        copy = T,
                        relationship = "many-to-many") %>%
      dplyr::inner_join(cdm[["procedure_occurrence_hes"]] %>% dplyr::filter(modifier_source_value == "1") %>% dplyr::select(person_id, procedure_source_value, procedure_date),
                        by = c("subject_id" = "person_id"),
                        copy = T,
                        relationship = "many-to-many") %>%
      dplyr::filter(procedure_date >=visit_detail_start_date & procedure_date <= visit_detail_end_date) %>%
      dplyr::filter(procedure_date >=index_date & procedure_date <= follow_up_end) %>%
      dplyr::distinct() %>% 
      dplyr::mutate(LoS = .data$visit_detail_end_date - .data$visit_detail_start_date + 1)
    
    tot_episodes <- nrow(freq_procedure_tbl)
    
    multiple <- freq_procedure_tbl %>% 
      dplyr::group_by(subject_id, index_date, procedure_date) %>%
      dplyr::tally() %>% 
      dplyr::filter(n>=2) %>% 
      dplyr::ungroup() %>% 
      dplyr::summarise(sum_of_procedures_more_than_one = sum(n)) %>% 
      dplyr::mutate(tot_procedures = nrow(freq_procedure_tbl)) %>% 
      dplyr::mutate(more_than_one_perc = round(sum_of_procedures_more_than_one/nrow(freq_procedure_tbl)*100, 2))
    
    freq_procedure <- freq_procedure_tbl %>% 
      dplyr::group_by(procedure_source_value) %>%
      dplyr::summarise(mean_los = mean(.data$LoS),
                       counts = n()) %>% 
      dplyr::ungroup() %>% 
      dplyr::arrange(desc(counts)) %>% 
      dplyr::mutate(percentage = counts/tot_episodes) %>% 
      dplyr::mutate(percentage = round(percentage*100, digits = 2),
                    mean_los = round(mean_los, digits = 2)) 
    
    summary_LoS <- tibble(mean_length_of_stay_per_procedure=as.integer(sum(freq_procedure_tbl$LoS))/tot_episodes,
                          min_length_of_stay_per_procedure = min(freq_procedure$mean_los),
                          max_length_of_stay_per_procedure = max(freq_procedure$mean_los),
                          sd_length_of_stay_per_procedure = round(sd(freq_procedure$mean_los), 2),
                          median_length_of_stay_per_procedure = round(quantile(freq_procedure$mean_los, probs = (.5)), 2),
                          lower_q_length_of_stay_per_procedure = round(quantile(freq_procedure$mean_los, probs = (.25)), 2),
                          upper_q_length_of_stay_per_procedure = round(quantile(freq_procedure$mean_los, probs = (.75)), 2))
    
    episode_per_person <- freq_procedure_tbl %>% 
      dplyr::group_by(subject_id, index_date, exposed_yrs) %>% 
      dplyr::summarise(counts = n()) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(counts_per_yr = counts/exposed_yrs)
    
    summary_episode_per_person_per_year <- tibble(mean_procedure_episode_per_person_per_year = sum(episode_per_person$counts)/sum(episode_per_person$exposed_yrs),
                                                  min_procedure_episode_per_person_per_year = min(episode_per_person$counts_per_yr),
                                                  max_procedure_episode_per_person_per_year = max(episode_per_person$counts_per_yr),
                                                  sd_procedure_episode_per_person_per_year = round(sd(episode_per_person$counts_per_yr), 2),
                                                  median_procedure_episode_per_person_per_year = round(quantile(episode_per_person$counts_per_yr, probs = (.5)), 2),
                                                  lower_q_procedure_episode_per_person_per_year = round(quantile(episode_per_person$counts_per_yr, probs = (.25)), 2),
                                                  upper_q_procedure_episode_per_person_per_year = round(quantile(episode_per_person$counts_per_yr, probs = (.75)), 2))
    
    freq_procedure <- freq_procedure %>% 
      dplyr::mutate(mean_los = ifelse((counts < 5 & counts > 0), paste0("<", minimum_counts), mean_los),
                    percentage = ifelse((counts < 5 & counts > 0), paste0("<", minimum_counts), percentage),
                    counts = ifelse((counts < 5 & counts > 0), paste0("<", minimum_counts), counts))
  }
  return(list(freq_procedure = freq_procedure, number_of_episodes = tot_episodes, more_than_one = multiple, summary_LoS = summary_LoS, summary_epi_per_person_per_yr = summary_episode_per_person_per_year))
}

visit_summary <- function(cohort_freq, table_name){
  freq_visit_occurrence_tbl <- cohort_freq %>% 
    dplyr::left_join(cdm[["visit_occurrence_hes"]] %>% dplyr::select(person_id, visit_concept_id, visit_start_date, visit_end_date),
                     by = c("subject_id" = "person_id"),
                     copy = T,
                     relationship = "many-to-many") 
  
  non_user <- freq_visit_occurrence_tbl %>% 
    dplyr::filter(is.na(visit_concept_id))
  
  non_user_count_2 <- non_user %>% dplyr::tally() %>% dplyr::rename("non_user_count" = "n")
  
  user <- freq_visit_occurrence_tbl %>% 
    dplyr::filter(!is.na(visit_concept_id)) %>% 
    dplyr::filter(visit_start_date >=index_date & visit_start_date <= follow_up_end) %>%
    dplyr::distinct() %>% 
    dplyr::mutate(LoS = visit_end_date - visit_start_date + 1)
  
  user_count <- user %>% 
    dplyr::group_by(subject_id, index_date, exposed_yrs) %>% 
    dplyr::summarise(counts = n()) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(counts_per_yr = counts/exposed_yrs)
  
  non_user_count <- non_user %>% 
    dplyr::select(subject_id, index_date, exposed_yrs) %>% 
    dplyr::mutate(counts = 0,
                  counts_per_yr = 0)
  
  tot_count_hos <- union_all(user_count, non_user_count)
  
  summary_hospitalisation_per_person_per_year_all <- tibble(mean_hospitalisation_per_person_per_year=(sum(tot_count_hos$counts)/sum(tot_count_hos$exposed_yrs)),
                                                            min_hospitalisation_per_person_per_year = min(tot_count_hos$counts_per_yr),
                                                            max_hospitalisation_per_person_per_year = max(tot_count_hos$counts_per_yr),
                                                            sd_hospitalisation_per_person_per_year = round(sd(tot_count_hos$counts_per_yr), 2),
                                                            median_hospitalisation_per_person_per_year = round(quantile(tot_count_hos$counts_per_yr, probs = (.5)), 2),
                                                            lower_q_hospitalisation_per_person_per_year = round(quantile(tot_count_hos$counts_per_yr, probs = (.25)), 2),
                                                            upper_q_hospitalisation_per_person_per_year = round(quantile(tot_count_hos$counts_per_yr, probs = (.75)), 2))
  
  summary_hospitalisation_per_person_per_year_user <- tibble(mean_hospitalisation_per_person_per_year=(sum(user_count$counts)/sum(user_count$exposed_yrs)),
                                                             min_hospitalisation_per_person_per_year = min(user_count$counts_per_yr),
                                                             max_hospitalisation_per_person_per_year = max(user_count$counts_per_yr),
                                                             sd_hospitalisation_per_person_per_year = round(sd(user_count$counts_per_yr), 2),
                                                             median_hospitalisation_per_person_per_year = round(quantile(user_count$counts_per_yr, probs = (.5)), 2),
                                                             lower_q_hospitalisation_per_person_per_year = round(quantile(user_count$counts_per_yr, probs = (.25)), 2),
                                                             upper_q_hospitalisation_per_person_per_year = round(quantile(user_count$counts_per_yr, probs = (.75)), 2))
  
  freq_visit_hosp <- cohort_freq %>% 
    dplyr::inner_join(cdm[["visit_occurrence_hes"]] %>% dplyr::select(person_id, visit_concept_id, visit_start_date, visit_end_date),
                      by = c("subject_id" = "person_id"),
                      copy = T,
                      relationship = "many-to-many") %>%
    dplyr::filter(visit_start_date >=index_date & visit_start_date <= follow_up_end) %>%
    dplyr::distinct() %>% 
    dplyr::mutate(length_of_stay = visit_end_date - visit_start_date+1)
  
  summary_LoS_per_person_per_hosp <- tibble(mean_LoS_per_hosp=(mean(freq_visit_hosp$length_of_stay)),
                                            min_LoS_per_hosp = min(freq_visit_hosp$length_of_stay),
                                            max_LoS_per_hosp = max(freq_visit_hosp$length_of_stay),
                                            sd_LoS_per_hosp = round(sd(freq_visit_hosp$length_of_stay), 2),
                                            median_LoS_per_hosp = round(quantile(freq_visit_hosp$length_of_stay, probs = (.5)), 2),
                                            lower_q_LoS_per_hosp = round(quantile(freq_visit_hosp$length_of_stay, probs = (.25)), 2),
                                            upper_q_LoS_per_hosp = round(quantile(freq_visit_hosp$length_of_stay, probs = (.75)), 2))
  
  freq_visit_epi <- cohort_freq %>% 
    dplyr::inner_join(cdm[[table_name]] %>% dplyr::select(person_id, visit_detail_concept_id, visit_detail_start_date, visit_detail_end_date),
                      by = c("subject_id" = "person_id"),
                      copy = T,
                      relationship = "many-to-many") %>%
    dplyr::filter(visit_detail_start_date >=index_date & visit_detail_start_date <= follow_up_end) %>%
    dplyr::distinct() %>% 
    dplyr::mutate(length_of_stay = visit_detail_end_date - visit_detail_start_date+1)
  
  summary_LoS_per_person_per_episode <- tibble(mean_LoS_per_episode=(mean(freq_visit_epi$length_of_stay)),
                                               min_LoS_per_episode = min(freq_visit_epi$length_of_stay),
                                               max_LoS_per_episode = max(freq_visit_epi$length_of_stay),
                                               sd_LoS_per_episode = round(sd(freq_visit_epi$length_of_stay), 2),
                                               median_LoS_per_episode = round(quantile(freq_visit_epi$length_of_stay, probs = (.5)), 2),
                                               lower_q_LoS_per_episode = round(quantile(freq_visit_epi$length_of_stay, probs = (.25)), 2),
                                               upper_q_LoS_per_episode = round(quantile(freq_visit_epi$length_of_stay, probs = (.75)), 2))
  
  return(list(summary_hos_per_pers_yr_all = summary_hospitalisation_per_person_per_year_all,
              summary_hos_per_pers_yr_user = summary_hospitalisation_per_person_per_year_user,
              non_user_count = non_user_count_2,
              summary_LoS_per_hos = summary_LoS_per_person_per_hosp,
              summary_LoS_per_epi = summary_LoS_per_person_per_episode))
}

#################################### SIDIAP #####################################
###conditions
condition_frequency_table_sidiap <- function(cohort_freq, table_name, primary = F){
  if (primary== F){
    freq_condition_tbl <- cohort_freq %>% 
      dplyr::inner_join(cdm[[table_name]] %>% dplyr::select(person_id, visit_concept_id, visit_start_date, visit_end_date),
                        by = c("subject_id" = "person_id"),
                        copy = T,
                        relationship = "many-to-many") %>%
      dplyr::inner_join(cdm[["condition_occurrence_hes"]] %>% dplyr::select(person_id, condition_source_value, condition_start_date),
                        by = c("subject_id" = "person_id"),
                        copy = T,
                        relationship = "many-to-many") %>%
      dplyr::filter(condition_start_date >=visit_start_date & condition_start_date <= visit_end_date) %>%
      dplyr::filter(condition_start_date >=index_date & condition_start_date <= follow_up_end) %>%
      dplyr::distinct() %>% 
      dplyr::mutate(LoS = .data$visit_end_date - .data$visit_start_date + 1)
    
    tot_hospitalisations <- nrow(freq_condition_tbl)
    
    multiple <- freq_condition_tbl %>% 
      dplyr::group_by(subject_id, index_date, condition_start_date) %>%
      dplyr::tally() %>% 
      dplyr::filter(n>=2) %>% 
      dplyr::ungroup() %>% 
      dplyr::summarise(sum_of_conditions_more_than_one = sum(n)) %>% 
      dplyr::mutate(tot_conditions = nrow(freq_condition_tbl)) %>% 
      dplyr::mutate(more_than_one_perc = round(sum_of_conditions_more_than_one/nrow(freq_condition_tbl)*100, 2))
    
    freq_condition <- freq_condition_tbl %>% 
      dplyr::group_by(condition_source_value) %>%
      dplyr::summarise(mean_los = mean(.data$LoS),
                       counts = n()) %>% 
      dplyr::ungroup() %>% 
      dplyr::arrange(desc(counts)) %>% 
      dplyr::mutate(percentage = counts/tot_hospitalisations) %>% 
      dplyr::mutate(percentage = round(percentage*100, digits = 2),
                    mean_los = round(mean_los, digits = 2)) 
    
    summary_LoS <- tibble(mean_length_of_stay_per_condition = as.integer(sum(freq_condition_tbl$LoS))/tot_hospitalisations,
                          min_length_of_stay_per_condition = min(freq_condition$mean_los),
                          max_length_of_stay_per_condition = max(freq_condition$mean_los),
                          sd_length_of_stay_per_condition = round(sd(freq_condition$mean_los), 2),
                          median_length_of_stay_per_condition = round(quantile(freq_condition$mean_los, probs = (.5)), 2),
                          lower_q_length_of_stay_per_condition = round(quantile(freq_condition$mean_los, probs = (.25)), 2),
                          upper_q_length_of_stay_per_condition = round(quantile(freq_condition$mean_los, probs = (.75)), 2))
    
    episode_per_person <- freq_condition_tbl %>% 
      dplyr::group_by(subject_id, index_date, exposed_yrs) %>% 
      dplyr::summarise(counts = n()) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(counts_per_yr = counts/exposed_yrs)
    
    summary_episode_per_person_per_year <- tibble(mean_condition_episode_per_person_per_year = sum(episode_per_person$counts)/sum(episode_per_person$exposed_yrs),
                                                  min_condition_episode_per_person_per_year = min(episode_per_person$counts_per_yr),
                                                  max_condition_episode_per_person_per_year = max(episode_per_person$counts_per_yr),
                                                  sd_condition_episode_per_person_per_year = round(sd(episode_per_person$counts_per_yr), 2),
                                                  median_condition_episode_per_person_per_year = round(quantile(episode_per_person$counts_per_yr, probs = (.5)), 2),
                                                  lower_q_condition_episode_per_person_per_year = round(quantile(episode_per_person$counts_per_yr, probs = (.25)), 2),
                                                  upper_q_condition_episode_per_person_per_year = round(quantile(episode_per_person$counts_per_yr, probs = (.75)), 2))
    
    
    freq_condition <- freq_condition %>% 
      dplyr::mutate(mean_los = ifelse((counts < 5 & counts > 0), paste0("<", minimum_counts), mean_los),
                    percentage = ifelse((counts < 5 & counts > 0), paste0("<", minimum_counts), percentage),
                    counts = ifelse((counts < 5 & counts > 0), paste0("<", minimum_counts), counts))
  }
  
  else {
    freq_condition_tbl <- cohort_freq %>% 
      dplyr::inner_join(cdm[[table_name]] %>% dplyr::select(person_id, visit_concept_id, visit_start_date, visit_end_date),
                        by = c("subject_id" = "person_id"),
                        copy = T,
                        relationship = "many-to-many") %>%
      dplyr::inner_join(cdm[["condition_occurrence_hes"]] %>% dplyr::filter(condition_status_source_value == "1") %>% dplyr::select(person_id, condition_source_value, condition_start_date),
                        by = c("subject_id" = "person_id"),
                        copy = T,
                        relationship = "many-to-many") %>%
      dplyr::filter(condition_start_date >=visit_start_date & condition_start_date <= visit_end_date) %>%
      dplyr::filter(condition_start_date >=index_date & condition_start_date <= follow_up_end) %>%
      dplyr::distinct() %>% 
      dplyr::mutate(LoS = .data$visit_end_date - .data$visit_start_date + 1)
    
    tot_hospitalisations <- nrow(freq_condition_tbl)
    
    multiple <- freq_condition_tbl %>% 
      dplyr::group_by(subject_id, index_date, condition_start_date) %>%
      dplyr::tally() %>% 
      dplyr::filter(n>=2) %>% 
      dplyr::ungroup() %>% 
      dplyr::summarise(sum_of_conditions_more_than_one = sum(n)) %>% 
      dplyr::mutate(tot_conditions = nrow(freq_condition_tbl)) %>% 
      dplyr::mutate(more_than_one_perc = round(sum_of_conditions_more_than_one/nrow(freq_condition_tbl)*100, 2))
    
    freq_condition <- freq_condition_tbl %>% 
      dplyr::group_by(condition_source_value) %>%
      dplyr::summarise(mean_los = mean(.data$LoS),
                       counts = n()) %>% 
      dplyr::ungroup() %>% 
      dplyr::arrange(desc(counts)) %>% 
      dplyr::mutate(percentage = counts/tot_hospitalisations) %>% 
      dplyr::mutate(percentage = round(percentage*100, digits = 2),
                    mean_los = round(mean_los, digits = 2)) 
    
    summary_LoS <- tibble(mean_length_of_stay_per_condition=as.integer(sum(freq_condition_tbl$LoS))/tot_hospitalisations,
                          min_length_of_stay_per_condition = min(freq_condition$mean_los),
                          max_length_of_stay_per_condition = max(freq_condition$mean_los),
                          sd_length_of_stay_per_condition = round(sd(freq_condition$mean_los), 2),
                          median_length_of_stay_per_condition = round(quantile(freq_condition$mean_los, probs = (.5)), 2),
                          lower_q_length_of_stay_per_condition = round(quantile(freq_condition$mean_los, probs = (.25)), 2),
                          upper_q_length_of_stay_per_condition = round(quantile(freq_condition$mean_los, probs = (.75)), 2))
    
    episode_per_person <- freq_condition_tbl %>% 
      dplyr::group_by(subject_id, index_date, exposed_yrs) %>% 
      dplyr::summarise(counts = n()) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(counts_per_yr = counts/exposed_yrs)
    
    summary_episode_per_person_per_year <- tibble(mean_condition_episode_per_person_per_year = sum(episode_per_person$counts)/sum(episode_per_person$exposed_yrs),
                                                  min_condition_episode_per_person_per_year = min(episode_per_person$counts_per_yr),
                                                  max_condition_episode_per_person_per_year = max(episode_per_person$counts_per_yr),
                                                  sd_condition_episode_per_person_per_year = round(sd(episode_per_person$counts_per_yr), 2),
                                                  median_condition_episode_per_person_per_year = round(quantile(episode_per_person$counts_per_yr, probs = (.5)), 2),
                                                  lower_q_condition_episode_per_person_per_year = round(quantile(episode_per_person$counts_per_yr, probs = (.25)), 2),
                                                  upper_q_condition_episode_per_person_per_year = round(quantile(episode_per_person$counts_per_yr, probs = (.75)), 2))
    
    freq_condition <- freq_condition %>% 
      dplyr::mutate(mean_los = ifelse((counts < 5 & counts > 0), paste0("<", minimum_counts), mean_los),
                    percentage = ifelse((counts < 5 & counts > 0), paste0("<", minimum_counts), percentage),
                    counts = ifelse((counts < 5 & counts > 0), paste0("<", minimum_counts), counts))
  }
  return(list(freq_condition = freq_condition, number_of_hospitalisations = tot_hospitalisations, more_than_one = multiple, summary_LoS = summary_LoS, summary_epi_per_person_per_yr = summary_episode_per_person_per_year))
}

###procedures
procedure_frequency_table_sidiap <- function(cohort_freq, table_name, primary = F){
  if (primary== F){
    freq_procedure_tbl <- cohort_freq %>% 
      dplyr::inner_join(cdm[[table_name]] %>% dplyr::select(person_id, visit_concept_id, visit_start_date, visit_end_date),
                        by = c("subject_id" = "person_id"),
                        copy = T,
                        relationship = "many-to-many") %>%
      dplyr::inner_join(cdm[["procedure_occurrence_hes"]] %>% dplyr::select(person_id, procedure_source_value, procedure_date),
                        by = c("subject_id" = "person_id"),
                        copy = T,
                        relationship = "many-to-many") %>%
      dplyr::filter(procedure_date >=visit_start_date & procedure_date <= visit_end_date) %>%
      dplyr::filter(procedure_date >=index_date & procedure_date <= follow_up_end) %>%
      dplyr::distinct() %>% 
      dplyr::mutate(LoS = .data$visit_end_date - .data$visit_start_date + 1)
    
    tot_hospitalisations <- nrow(freq_procedure_tbl)
    
    multiple <- freq_procedure_tbl %>% 
      dplyr::group_by(subject_id, index_date, procedure_date) %>%
      dplyr::tally() %>% 
      dplyr::filter(n>=2) %>% 
      dplyr::ungroup() %>% 
      dplyr::summarise(sum_of_procedures_more_than_one = sum(n)) %>% 
      dplyr::mutate(tot_procedures = nrow(freq_procedure_tbl)) %>% 
      dplyr::mutate(more_than_one_perc = round(sum_of_procedures_more_than_one/nrow(freq_procedure_tbl)*100, 2))
    
    freq_procedure <- freq_procedure_tbl %>% 
      dplyr::group_by(procedure_source_value) %>%
      dplyr::summarise(mean_los = mean(.data$LoS),
                       counts = n()) %>% 
      dplyr::ungroup() %>% 
      dplyr::arrange(desc(counts)) %>% 
      dplyr::mutate(percentage = counts/tot_hospitalisations) %>% 
      dplyr::mutate(percentage = round(percentage*100, digits = 2),
                    mean_los = round(mean_los, digits = 2)) 
    
    summary_LoS <- tibble(mean_length_of_stay_per_procedure = as.integer(sum(freq_procedure_tbl$LoS))/tot_hospitalisations,
                          min_length_of_stay_per_procedure = min(freq_procedure$mean_los),
                          max_length_of_stay_per_procedure = max(freq_procedure$mean_los),
                          sd_length_of_stay_per_procedure = round(sd(freq_procedure$mean_los), 2),
                          median_length_of_stay_per_procedure = round(quantile(freq_procedure$mean_los, probs = (.5)), 2),
                          lower_q_length_of_stay_per_procedure = round(quantile(freq_procedure$mean_los, probs = (.25)), 2),
                          upper_q_length_of_stay_per_procedure = round(quantile(freq_procedure$mean_los, probs = (.75)), 2))
    
    episode_per_person <- freq_procedure_tbl %>% 
      dplyr::group_by(subject_id, index_date, exposed_yrs) %>% 
      dplyr::summarise(counts = n()) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(counts_per_yr = counts/exposed_yrs)
    
    summary_episode_per_person_per_year <- tibble(mean_procedure_episode_per_person_per_year = sum(episode_per_person$counts)/sum(episode_per_person$exposed_yrs),
                                                  min_procedure_episode_per_person_per_year = min(episode_per_person$counts_per_yr),
                                                  max_procedure_episode_per_person_per_year = max(episode_per_person$counts_per_yr),
                                                  sd_procedure_episode_per_person_per_year = round(sd(episode_per_person$counts_per_yr), 2),
                                                  median_procedure_episode_per_person_per_year = round(quantile(episode_per_person$counts_per_yr, probs = (.5)), 2),
                                                  lower_q_procedure_episode_per_person_per_year = round(quantile(episode_per_person$counts_per_yr, probs = (.25)), 2),
                                                  upper_q_procedure_episode_per_person_per_year = round(quantile(episode_per_person$counts_per_yr, probs = (.75)), 2))
    
    
    freq_procedure <- freq_procedure %>% 
      dplyr::mutate(mean_los = ifelse((counts < 5 & counts > 0), paste0("<", minimum_counts), mean_los),
                    percentage = ifelse((counts < 5 & counts > 0), paste0("<", minimum_counts), percentage),
                    counts = ifelse((counts < 5 & counts > 0), paste0("<", minimum_counts), counts))
  }
  
  else {
    freq_procedure_tbl <- cohort_freq %>% 
      dplyr::inner_join(cdm[[table_name]] %>% dplyr::select(person_id, visit_concept_id, visit_start_date, visit_end_date),
                        by = c("subject_id" = "person_id"),
                        copy = T,
                        relationship = "many-to-many") %>%
      dplyr::inner_join(cdm[["procedure_occurrence_hes"]] %>% dplyr::filter(modifier_source_value == "1") %>% dplyr::select(person_id, procedure_source_value, procedure_date),
                        by = c("subject_id" = "person_id"),
                        copy = T,
                        relationship = "many-to-many") %>%
      dplyr::filter(procedure_date >=visit_start_date & procedure_date <= visit_end_date) %>%
      dplyr::filter(procedure_date >=index_date & procedure_date <= follow_up_end) %>%
      dplyr::distinct() %>% 
      dplyr::mutate(LoS = .data$visit_end_date - .data$visit_start_date + 1)
    
    tot_hospitalisations <- nrow(freq_procedure_tbl)
    
    multiple <- freq_procedure_tbl %>% 
      dplyr::group_by(subject_id, index_date, procedure_date) %>%
      dplyr::tally() %>% 
      dplyr::filter(n>=2) %>% 
      dplyr::ungroup() %>% 
      dplyr::summarise(sum_of_procedures_more_than_one = sum(n)) %>% 
      dplyr::mutate(tot_procedures = nrow(freq_procedure_tbl)) %>% 
      dplyr::mutate(more_than_one_perc = round(sum_of_procedures_more_than_one/nrow(freq_procedure_tbl)*100, 2))
    
    freq_procedure <- freq_procedure_tbl %>% 
      dplyr::group_by(procedure_source_value) %>%
      dplyr::summarise(mean_los = mean(.data$LoS),
                       counts = n()) %>% 
      dplyr::ungroup() %>% 
      dplyr::arrange(desc(counts)) %>% 
      dplyr::mutate(percentage = counts/tot_hospitalisations) %>% 
      dplyr::mutate(percentage = round(percentage*100, digits = 2),
                    mean_los = round(mean_los, digits = 2)) 
    
    summary_LoS <- tibble(mean_length_of_stay_per_procedure=as.integer(sum(freq_procedure_tbl$LoS))/tot_hospitalisations,
                          min_length_of_stay_per_procedure = min(freq_procedure$mean_los),
                          max_length_of_stay_per_procedure = max(freq_procedure$mean_los),
                          sd_length_of_stay_per_procedure = round(sd(freq_procedure$mean_los), 2),
                          median_length_of_stay_per_procedure = round(quantile(freq_procedure$mean_los, probs = (.5)), 2),
                          lower_q_length_of_stay_per_procedure = round(quantile(freq_procedure$mean_los, probs = (.25)), 2),
                          upper_q_length_of_stay_per_procedure = round(quantile(freq_procedure$mean_los, probs = (.75)), 2))
    
    episode_per_person <- freq_procedure_tbl %>% 
      dplyr::group_by(subject_id, index_date, exposed_yrs) %>% 
      dplyr::summarise(counts = n()) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(counts_per_yr = counts/exposed_yrs)
    
    summary_episode_per_person_per_year <- tibble(mean_procedure_episode_per_person_per_year = sum(episode_per_person$counts)/sum(episode_per_person$exposed_yrs),
                                                  min_procedure_episode_per_person_per_year = min(episode_per_person$counts_per_yr),
                                                  max_procedure_episode_per_person_per_year = max(episode_per_person$counts_per_yr),
                                                  sd_procedure_episode_per_person_per_year = round(sd(episode_per_person$counts_per_yr), 2),
                                                  median_procedure_episode_per_person_per_year = round(quantile(episode_per_person$counts_per_yr, probs = (.5)), 2),
                                                  lower_q_procedure_episode_per_person_per_year = round(quantile(episode_per_person$counts_per_yr, probs = (.25)), 2),
                                                  upper_q_procedure_episode_per_person_per_year = round(quantile(episode_per_person$counts_per_yr, probs = (.75)), 2))
    
    freq_procedure <- freq_procedure %>% 
      dplyr::mutate(mean_los = ifelse((counts < 5 & counts > 0), paste0("<", minimum_counts), mean_los),
                    percentage = ifelse((counts < 5 & counts > 0), paste0("<", minimum_counts), percentage),
                    counts = ifelse((counts < 5 & counts > 0), paste0("<", minimum_counts), counts))
  }
  return(list(freq_procedure = freq_procedure, number_of_hospitalisations = tot_hospitalisations, more_than_one = multiple, summary_LoS = summary_LoS, summary_epi_per_person_per_yr = summary_episode_per_person_per_year))
}

visit_summary_sidiap <- function(cohort_freq, table_name){
  freq_visit_occurrence_tbl <- cohort_freq %>% 
    dplyr::left_join(cdm[[table_name]] %>% dplyr::select(person_id, visit_concept_id, visit_start_date, visit_end_date),
                     by = c("subject_id" = "person_id"),
                     copy = T,
                     relationship = "many-to-many") 
  
  non_user <- freq_visit_occurrence_tbl %>% 
    dplyr::filter(is.na(visit_concept_id))
  
  non_user_count_2 <- non_user %>% dplyr::tally() %>% dplyr::rename("non_user_count" = "n")
  
  user <- freq_visit_occurrence_tbl %>% 
    dplyr::filter(!is.na(visit_concept_id)) %>% 
    dplyr::filter(visit_start_date >=index_date & visit_start_date <= follow_up_end) %>%
    dplyr::distinct() %>% 
    dplyr::mutate(LoS = visit_end_date - visit_start_date + 1)
  
  user_count <- user %>% 
    dplyr::group_by(subject_id, index_date, exposed_yrs) %>% 
    dplyr::summarise(counts = n()) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(counts_per_yr = counts/exposed_yrs)
  
  non_user_count <- non_user %>% 
    dplyr::select(subject_id, index_date, exposed_yrs) %>% 
    dplyr::mutate(counts = 0,
                  counts_per_yr = 0)
  
  tot_count_hos <- union_all(user_count, non_user_count)
  
  summary_hospitalisation_per_person_per_year_all <- tibble(mean_hospitalisation_per_person_per_year=(sum(tot_count_hos$counts)/sum(tot_count_hos$exposed_yrs)),
                                                            min_hospitalisation_per_person_per_year = min(tot_count_hos$counts_per_yr),
                                                            max_hospitalisation_per_person_per_year = max(tot_count_hos$counts_per_yr),
                                                            sd_hospitalisation_per_person_per_year = round(sd(tot_count_hos$counts_per_yr), 2),
                                                            median_hospitalisation_per_person_per_year = round(quantile(tot_count_hos$counts_per_yr, probs = (.5)), 2),
                                                            lower_q_hospitalisation_per_person_per_year = round(quantile(tot_count_hos$counts_per_yr, probs = (.25)), 2),
                                                            upper_q_hospitalisation_per_person_per_year = round(quantile(tot_count_hos$counts_per_yr, probs = (.75)), 2))
  
  summary_hospitalisation_per_person_per_year_user <- tibble(mean_hospitalisation_per_person_per_year=(sum(user_count$counts)/sum(user_count$exposed_yrs)),
                                                             min_hospitalisation_per_person_per_year = min(user_count$counts_per_yr),
                                                             max_hospitalisation_per_person_per_year = max(user_count$counts_per_yr),
                                                             sd_hospitalisation_per_person_per_year = round(sd(user_count$counts_per_yr), 2),
                                                             median_hospitalisation_per_person_per_year = round(quantile(user_count$counts_per_yr, probs = (.5)), 2),
                                                             lower_q_hospitalisation_per_person_per_year = round(quantile(user_count$counts_per_yr, probs = (.25)), 2),
                                                             upper_q_hospitalisation_per_person_per_year = round(quantile(user_count$counts_per_yr, probs = (.75)), 2))
  
  freq_visit_hosp <- cohort_freq %>% 
    dplyr::inner_join(cdm[[table_name]] %>% dplyr::select(person_id, visit_concept_id, visit_start_date, visit_end_date),
                      by = c("subject_id" = "person_id"),
                      copy = T,
                      relationship = "many-to-many") %>%
    dplyr::filter(visit_start_date >=index_date & visit_start_date <= follow_up_end) %>%
    dplyr::distinct() %>% 
    dplyr::mutate(length_of_stay = visit_end_date - visit_start_date+1)
  
  summary_LoS_per_person_per_hosp <- tibble(mean_LoS_per_hosp=(mean(freq_visit_hosp$length_of_stay)),
                                            min_LoS_per_hosp = min(freq_visit_hosp$length_of_stay),
                                            max_LoS_per_hosp = max(freq_visit_hosp$length_of_stay),
                                            sd_LoS_per_hosp = round(sd(freq_visit_hosp$length_of_stay), 2),
                                            median_LoS_per_hosp = round(quantile(freq_visit_hosp$length_of_stay, probs = (.5)), 2),
                                            lower_q_LoS_per_hosp = round(quantile(freq_visit_hosp$length_of_stay, probs = (.25)), 2),
                                            upper_q_LoS_per_hosp = round(quantile(freq_visit_hosp$length_of_stay, probs = (.75)), 2))
  
  return(list(summary_hos_per_pers_yr_all = summary_hospitalisation_per_person_per_year_all,
              summary_hos_per_pers_yr_user = summary_hospitalisation_per_person_per_year_user,
              non_user_count = non_user_count_2,
              summary_LoS_per_hos = summary_LoS_per_person_per_hosp))
}


######IQVIA fix
insertTable2 <- function(cdm, con, writeSchema, name, table) {
  nlim <- 100000
  if(nrow(table) > nlim) {
    cohorts <- list()
    prefix2 <- stri_rand_strings(n = 1, length = 4, '[a-z]')
    # insert by part
    for (k in (1:ceiling(nrow(table)/nlim))){
      smalltable <- table %>% dplyr::slice((1+(k-1)*nlim):(nlim*k))
      tmpname <- inSchema(schema = writeSchema, table = paste0(prefix2, "_temp_", k), dbms = dbms(con))
      DBI::dbWriteTable(conn = con, name = tmpname, value = smalltable)
      cdm[[paste0(prefix2, "_temp_", k)]] <- dplyr::tbl(attr(cdm,"dbcon"), tmpname)
      cohorts[[k]] <- cdm[[paste0(prefix2, "_temp_", k)]]
    }
    # merge all of them and compute to name
    cdm[[name]] <- Reduce(dplyr::union_all, cohorts) |> 
      dplyr::compute()
    # drop the the temp tables
    cdm <- cdm |> 
      cdm_select_tbl(names(cdm)[!startsWith(names(cdm), prefix = paste0(prefix2, "_temp_"))])
    cdm <- CDMConnector::dropTable(cdm = cdm,
                                   name = starts_with(paste0(prefix2, "_temp_")))
  } else {
    tmpname <- inSchema(schema = writeSchema, table = name, dbms = dbms(con))
    
    DBI::dbWriteTable(conn = con, name = tmpname, value = allSubjects_test)
    
    cdm[[name]] <- dplyr::tbl(attr(cdm,"dbcon"), tmpname)
  }
  return(cdm)
}

#### secondary cost
secondary_cost_sidiap <- function(cohort_freq, table_name, cost_type = "all"){
  # Select the appropriate cost inputs based on cost_type
  if(cost_type == "all"){
    cost_inputs <- ccs_spain_cost_inputs # all costs
  } else if (cost_type == "fx_related") {
    cost_inputs <- ccs_spain_cost_inputs_fx # only fracture related
  }
  
  freq_condition_tbl <- cohort_freq %>% 
    dplyr::left_join(cdm[[table_name]] %>% dplyr::select(person_id, visit_concept_id, visit_start_date, visit_end_date, visit_source_value, visit_occurrence_id),
                     by = c("subject_id" = "person_id"),
                     copy = T,
                     relationship = "many-to-many") %>% 
    dplyr::compute()
  freq_condition_tbl <- freq_condition_tbl %>% 
    dplyr::left_join(cdm[["condition_occurrence_hes"]] %>% dplyr::select(person_id, condition_source_value, condition_start_date, visit_occurrence_id), 
                     by = c("subject_id" = "person_id", "visit_occurrence_id"),
                     copy = T,
                     relationship = "many-to-many") %>% 
    dplyr::compute()
  
  # removing "." in icd.10 codes - in spain they have the "."
  freq_condition_tbl$condition_source_value <- gsub("\\.", "", freq_condition_tbl$condition_source_value)
  
  # Joining costs based on the selected cost inputs
  freq_condition_tbl_all <- freq_condition_tbl %>% 
    dplyr::left_join(cost_inputs %>% dplyr::select(icd10cm_code, cost), 
                     by = c("condition_source_value" = "icd10cm_code"))
  
  ### CHECK PART ###
  
  # 1. this is a CHECK of the ICD10 that do not get matched with a cost, just in case there are issues with num of digits or dots!
  no_cost_conditions <- freq_condition_tbl_all %>%
    dplyr::filter(is.na(cost))
  
  no_cost_conditions <- distinct(no_cost_conditions, condition_source_value)
  
  # 2. identifying if there are duplicate records (so two hospitalisations with same spell number, condition and by the same patient)
  duplicates <- freq_condition_tbl %>%
    dplyr::group_by(condition_source_value, subject_id, visit_source_value, visit_occurrence_id) %>%
    dplyr::summarise(n = n(), .groups = 'drop') %>%
    dplyr::filter(n>1) %>% 
    dplyr::select(condition_source_value, n)
  
  # 3. Are there any conditions outside of the spell?
  
  cond_outside <- freq_condition_tbl %>% 
    dplyr::filter(condition_start_date <visit_start_date | condition_start_date >visit_end_date) %>% 
    tally()
  
  #### END of CHECKs ####
  
  ## replacing cost with zeros for non-users
  all_cost <- freq_condition_tbl_all %>%
    dplyr::mutate(cost = case_when(
      is.na(visit_concept_id) ~ 0, 
      TRUE ~ cost
    )) %>% 
    dplyr::mutate(cost_per_person_year = cost/exposed_yrs) %>% 
    ungroup()
  
  # just a count for the non_users
  non_user <- all_cost %>% 
    dplyr::filter(is.na(visit_concept_id))
  
  non_user_count_2 <- non_user %>% dplyr::tally() %>% dplyr::rename("non_user_count" = "n")
  
  # creating a separate dataframe for users
  freq_condition_tbl_users <- freq_condition_tbl_all %>% 
    dplyr::filter(!is.na(visit_concept_id)) %>% 
    ## filtering so that to include only hospitalisaitons during entry
    dplyr::filter(visit_start_date >=index_date & visit_start_date <= follow_up_end) %>% 
    ## filtering so that conditions are within hospitalisations
    dplyr::filter(condition_start_date >=visit_start_date & condition_start_date <= visit_end_date) %>%
    dplyr::filter(condition_start_date >=index_date & condition_start_date <= follow_up_end) %>%
    dplyr::distinct() # same as the check for duplicates above to deal with duplicated hospitalisations due to enter/leaves on the same day 
  
  user_cost <-freq_condition_tbl_users %>% 
    dplyr::group_by(subject_id, index_date, exposed_yrs) %>% 
    dplyr::mutate(cost_per_person_year = cost/exposed_yrs) %>% 
    dplyr::ungroup()
  
  hosp_cost_distribution<- ggplot(freq_condition_tbl_users, 
                                  aes(x = cost)) +
    geom_histogram(bins = 30, fill = "blue", color = "black") +
    theme_minimal() +
    labs(title = "Histogram of Costs", x = "Cost per hospitalisation", y = "Frequency")
  
  
  summary_cost_per_hosp <- tibble(mean_cost_per_hosp = mean(freq_condition_tbl_users$cost),
                                  min_cost_per_hosp = min(freq_condition_tbl_users$cost),
                                  max_cost_per_hosp = max(freq_condition_tbl_users$cost),
                                  sd_cost_per_hosp = round(sd(freq_condition_tbl_users$cost), 2),
                                  median_cost_per_hosp = round(quantile(freq_condition_tbl_users$cost, probs = (.5)), 2),
                                  lower_cost_per_hosp = round(quantile(freq_condition_tbl_users$cost, probs = (.25)), 2),
                                  upper_cost_per_hosp = round(quantile(freq_condition_tbl_users$cost, probs = (.75)), 2))
  
  # Summary all
  summary_hospitalisation_per_person_per_year_all <- tibble(mean_hospitalisation_per_person_per_year=(sum(all_cost$cost)/sum(all_cost$exposed_yrs)),
                                                            min_hospitalisation_per_person_per_year = min(all_cost$cost_per_person_year),
                                                            max_hospitalisation_per_person_per_year = max(all_cost$cost_per_person_year),
                                                            sd_hospitalisation_per_person_per_year = round(sd(all_cost$cost_per_person_year), 2),
                                                            median_hospitalisation_per_person_per_year = round(quantile(all_cost$cost_per_person_year, probs = (.5)), 2),
                                                            lower_q_hospitalisation_per_person_per_year = round(quantile(all_cost$cost_per_person_year, probs = (.25)), 2),
                                                            upper_q_hospitalisation_per_person_per_year = round(quantile(all_cost$cost_per_person_year, probs = (.75)), 2))
  
  # Summary users
  summary_hospitalisation_per_person_per_year_user <- tibble(mean_hospitalisation_per_person_per_year=(sum(user_cost$cost)/sum(user_cost$exposed_yrs)),
                                                             min_hospitalisation_per_person_per_year = min(user_cost$cost_per_person_year),
                                                             max_hospitalisation_per_person_per_year = max(user_cost$cost_per_person_year),
                                                             sd_hospitalisation_per_person_per_year = round(sd(user_cost$cost_per_person_year), 2),
                                                             median_hospitalisation_per_person_per_year = round(quantile(user_cost$cost_per_person_year, probs = (.5)), 2),
                                                             lower_q_hospitalisation_per_person_per_year = round(quantile(user_cost$cost_per_person_year, probs = (.25)), 2),
                                                             upper_q_hospitalisation_per_person_per_year = round(quantile(user_cost$cost_per_person_year, probs = (.75)), 2))
  
  return(list(# costs of all hospitalisations
    summary_hos_per_pers_yr_all = summary_hospitalisation_per_person_per_year_all,
    summary_hos_per_pers_yr_user = summary_hospitalisation_per_person_per_year_user,
    non_user_count = non_user_count_2,
    summary_cost_per_hosp = summary_cost_per_hosp,
    # check cost assignment
    no_cost_conditions = no_cost_conditions,
    duplicates = duplicates,
    cond_outside = cond_outside,
    hosp_cost_distribution = hosp_cost_distribution
  ))
}

#### secondary cost
secondary_cost_cprd <- function(cohort_freq, table_name){
  
  freq_cond_proc_tbl <- cohort_freq %>% 
    dplyr::left_join(cdm[[table_name]] %>% dplyr::select(person_id, visit_concept_id, visit_start_date, visit_end_date, visit_source_value, visit_occurrence_id),
                     by = c("subject_id" = "person_id"),
                     copy = T,
                     relationship = "many-to-many")  %>%
    dplyr::left_join(cdm[["condition_occurrence_hes"]] %>% dplyr::select(person_id, condition_source_value, condition_start_date, condition_status_source_value, visit_occurrence_id, visit_detail_id),
                     by = c("subject_id" = "person_id", "visit_occurrence_id"),
                     copy = T,
                     relationship = "many-to-many") 
  # count conditions outside visit
  cond_outs <- freq_cond_proc_tbl %>%  
    dplyr::filter(condition_start_date <visit_start_date | condition_start_date > visit_end_date) %>% tally()
  
  freq_cond_proc_tbl <- freq_cond_proc_tbl %>%  
    #  filter conditions and visits
    dplyr::filter(visit_start_date >=index_date & visit_end_date <= follow_up_end) %>%  
    dplyr::filter(condition_start_date >=visit_start_date & condition_start_date <= visit_end_date) %>%
    dplyr::filter(condition_start_date >=index_date & condition_start_date <= follow_up_end) %>%
    dplyr::distinct() %>% 
    dplyr::mutate(LoS = .data$visit_end_date - .data$visit_start_date + 1)
  
  
  freq_cond_proc_tbl <- freq_cond_proc_tbl %>%  
    # Join primary procedures (separately because too big otherwise)
    dplyr::left_join(cdm[["procedure_occurrence_hes"]] %>% 
                       dplyr::filter(modifier_source_value == "1") %>% 
                       dplyr::select(person_id, procedure_source_value, procedure_date, visit_occurrence_id, modifier_source_value, visit_detail_id),
                     by = c("subject_id" = "person_id", "visit_occurrence_id", "visit_detail_id"),
                     copy = T,
                     relationship = "many-to-many")
  
  # count procedures outside visit
  proc_outs <- freq_cond_proc_tbl %>%  
    dplyr::filter(procedure_date <visit_start_date | procedure_date > visit_end_date) %>% 
    tally()
  
  freq_cond_proc_tbl <- freq_cond_proc_tbl  %>%  
    # filter procedures
    dplyr::filter(procedure_date >=visit_start_date & procedure_date <= visit_end_date) %>%
    dplyr::filter(procedure_date >=index_date & procedure_date <= follow_up_end) %>%
    dplyr::distinct() %>% 
    dplyr::mutate(LoS = .data$visit_end_date - .data$visit_start_date + 1) 
  
  # Apply simplified NHS HRG Grouper
  
  # removing "." in icd.10 and opcs codes
  freq_cond_proc_tbl$condition_source_value <- gsub("\\.", "", freq_cond_proc_tbl$condition_source_value)
  freq_cond_proc_tbl$procedure_source_value <- gsub("\\.", "", freq_cond_proc_tbl$procedure_source_value)
  
  # Step 1: If primary diagnostic code = any of those in the <Diag_incl> sheet, then include in costing; if not, exclude
  
  ## 1.1: Identify hospitalisations with at least one primary diagnosis in the <Diag_incl> sheet
  qualifying_hospitalisations <- freq_cond_proc_tbl %>%
    dplyr::filter(condition_status_source_value == 1 & 
                    condition_source_value %in% grouper_uk_Diag_incl$ICD10CM) %>%
    distinct(subject_id, visit_occurrence_id)
  
  ## 1.2: Filter the original dataset to include only hospitalisations from those qualifying hospitalisations
  freq_cond_proc_tbl_hrg <- freq_cond_proc_tbl %>%
    semi_join(qualifying_hospitalisations, by = c("subject_id", "visit_occurrence_id"))
  
  # Step 2: 
  
  ## 2.1: If >1 diagnostic codes from the list in <Diag_incl> are present in the list of diagnoses for a hospitalisation, then apply HRG = VA1A
  # Note: there might be >1 primary diagnostic codes that meet the criterion for the same visit_occurrence_id. This is because primary diagnoses are at episode level, so there are as many as the episodes in a spell
  freq_cond_proc_tbl_hrg <- freq_cond_proc_tbl_hrg %>%
    # Group by subject_id
    dplyr::group_by(subject_id, visit_occurrence_id) %>%
    dplyr::mutate(HRG = ifelse(sum(condition_source_value %in% grouper_uk_Diag_incl$ICD10CM) > 1, 
                               "VA1A", 
                               NA_character_)) %>%
    ungroup()
  
  ## 2.2: Next, look-up primary diagnosis in sheet <Diag_HRG> an apply corresponding HRG
  freq_cond_proc_tbl_hrg <- freq_cond_proc_tbl_hrg %>%
    dplyr::filter(condition_status_source_value == 1) %>%
    dplyr::group_by(subject_id, visit_occurrence_id) %>%
    dplyr::mutate(HRG = if_else(is.na(HRG) & condition_source_value %in% grouper_uk_Diag_HRG$ICD10CM,
                                grouper_uk_Diag_HRG$SpellHRG[match(condition_source_value, grouper_uk_Diag_HRG$ICD10CM)],
                                HRG)) %>%
    ungroup()
  
  # Step 3: Categorise main diagnostic codes - SHOULD I FILTER FOR PRIMARY ONLY?
  library(stringr)
  freq_cond_proc_tbl_hrg <- freq_cond_proc_tbl_hrg %>%
    dplyr::mutate(diag_group = case_when(
      str_starts(condition_source_value, "S") ~ "S", # If primary diagnostic code starts with "S", then categorise as diag_group = "S"
      str_starts(condition_source_value, "T") ~ "T", # If primary diagnostic code starts with "T", then categorise as diag_group = "T"
      str_starts(condition_source_value, "M") ~ "M", # If primary diagnostic code starts with "M", then categorise as diag_group = "M"
      TRUE ~ "O" # If primary diagnostic code starts with any other letter, then categorise as diag_group = "O"
    ))
  
  
  # Step 4: If main procedure code is any of those shown in sheet <Proc_HRG_any>, then replace HRG for corresponding code shown in the sheet
  freq_cond_proc_tbl_hrg <- freq_cond_proc_tbl_hrg %>%    
    dplyr::mutate(HRG = if_else(procedure_source_value %in% grouper_uk_Proc_HRG_any$OPCS,
                                grouper_uk_Proc_HRG_any$HRG[match(procedure_source_value, grouper_uk_Proc_HRG_any$OPCS)],
                                HRG))
  
  # Step 5: Then for Proc_HRG_STM based on diag_group
  freq_cond_proc_tbl_hrg <- freq_cond_proc_tbl_hrg %>%
    dplyr::mutate(HRG = case_when(
      diag_group == "S" & procedure_source_value %in% grouper_uk_Proc_HRG_STM$OPCS ~ 
        grouper_uk_Proc_HRG_STM$`HRG - Diag S or T`[match(procedure_source_value, grouper_uk_Proc_HRG_STM$OPCS)],
      diag_group == "T" & procedure_source_value %in% grouper_uk_Proc_HRG_STM$OPCS ~ 
        grouper_uk_Proc_HRG_STM$`HRG - Diag S or T`[match(procedure_source_value, grouper_uk_Proc_HRG_STM$OPCS)],
      diag_group == "M" & procedure_source_value %in% grouper_uk_Proc_HRG_STM$OPCS ~ 
        grouper_uk_Proc_HRG_STM$`HRG - Diag M`[match(procedure_source_value, grouper_uk_Proc_HRG_STM$OPCS)],
      TRUE ~ HRG
    ))
  
  # Append costs to HRG and compute summary
  freq_cond_proc_tbl_hrg_cost <- freq_cond_proc_tbl_hrg %>% 
    dplyr::left_join(grouper_uk_unit_cost,
                     by = c("HRG" = "SpellHRG"))
  
  # cleaning - when one visit has multiple rows with the same entry, visit_occurrence_id, and HRG keep only one
  freq_cond_proc_tbl_hrg_cost_clean<- freq_cond_proc_tbl_hrg_cost %>% 
    dplyr::distinct(subject_id, index_date, follow_up_end, visit_occurrence_id, HRG, Unit_cost, .keep_all = TRUE)
  
  # Count and checks for multiple entries during same spell
  
  # CASE A: leave/enter on the same day as the spell (spell=1 day)
  case_a <- freq_cond_proc_tbl_hrg_cost_clean %>%
    # First, filter rows where visit_start_date is equal to visit_end_date
    dplyr::filter(visit_start_date == visit_end_date) %>%
    dplyr::group_by(visit_occurrence_id, subject_id) %>%
    # Summarise to collapse each group into a list-column, each element is a dataframe
    summarise(data = list(tibble(index_date = index_date, follow_up_end = follow_up_end)), .groups = 'keep') %>%
    # For each group, check if any index_date matches any follow_up_end across rows
    dplyr::mutate(match_found = map_lgl(data, ~{
      any(sapply(.$index_date, function(id) id %in% .$follow_up_end))
    })) %>%
    dplyr::filter(match_found) %>%
    ungroup() %>%
    # Count unique visit_occurrence_id and subject_id pairs
    summarise(
      distinct_subjects = n_distinct(subject_id),
      distinct_visit_occurrences = n_distinct(visit_occurrence_id)
    )
  
  # CASE A.1: leave enter on the same day as the start of the spell 
  case_a.1 <- freq_cond_proc_tbl_hrg_cost_clean %>%
    # Ensure visit spans more than a single day
    dplyr::filter(as.Date(visit_end_date) - as.Date(visit_start_date) > 1) %>%
    dplyr::group_by(visit_occurrence_id, subject_id) %>%
    # Create a list-column with relevant data for each group
    summarise(
      data = list(tibble(visit_start_date = visit_start_date, 
                         visit_end_date = visit_end_date,
                         index_date = index_date, 
                         follow_up_end = follow_up_end)),
      .groups = 'keep'
    ) %>%
    # Check within each group for the specified conditions
    mutate(match_found = map_lgl(data, ~{
      # Condition 1: Checking for any visit_start_date that matches an index_date
      condition1 <- any(duplicated(c(.$visit_start_date, .$index_date)))
      # Condition 2: Any index_date equals any follow_up_end within the group
      condition2 <- any(sapply(.$index_date, function(id) id %in% .$follow_up_end))
      
      condition1 && condition2
    })) %>%
    filter(match_found) %>%
    ungroup() %>%
    # Count unique visit_occurrence_id and subject_id pairs
    summarise(
      distinct_subjects = n_distinct(subject_id),
      distinct_visit_occurrences = n_distinct(visit_occurrence_id)
    )
  
  
  # CASE B: leave enter on the same day within the spell 
  case_b <- freq_cond_proc_tbl_hrg_cost_clean %>%
    # Ensure visit spans more than a single day
    dplyr::filter(as.Date(visit_end_date) - as.Date(visit_start_date) > 1) %>%
    dplyr::group_by(visit_occurrence_id, subject_id) %>%
    # Create a list-column with relevant data for each group
    summarise(
      data = list(tibble(visit_start_date = visit_start_date, 
                         index_date = index_date, 
                         follow_up_end = follow_up_end)),
      .groups = 'keep'
    ) %>%
    # Check within each group for the specified conditions
    dplyr::mutate(match_found = map_lgl(data, ~{
      # New Condition 1: Check if any visit_start_date is less than any index_date within the group
      condition1 <- any(sapply(.$visit_start_date, function(vsd) any(vsd < .$index_date)))
      # Condition 2 (Unchanged): Any index_date equals any follow_up_end within the group
      condition2 <- any(sapply(.$index_date, function(id) id %in% .$follow_up_end))
      
      condition1 && condition2
    })) %>%
    filter(match_found) %>%
    ungroup() %>%
    # Count unique visit_occurrence_id and subject_id pairs
    summarise(
      distinct_subjects = n_distinct(subject_id),
      distinct_visit_occurrences = n_distinct(visit_occurrence_id)
    )
  
  # CASE C: leave/enter on different days within the same spell
  case_c<-freq_cond_proc_tbl_hrg_cost_clean %>%
    # Ensure visit spans more than a single day
    dplyr::filter(as.Date(visit_end_date) - as.Date(visit_start_date) > 1) %>%
    dplyr::group_by(visit_occurrence_id, subject_id) %>%
    # Create a list-column with relevant data for each group
    summarise(
      data = list(tibble(visit_start_date = visit_start_date, 
                         index_date = index_date, 
                         follow_up_end = follow_up_end)),
      .groups = 'keep'
    ) %>%
    # Check within each group for the specified conditions
    dplyr::mutate(match_found = map_lgl(data, ~{
      # New Condition 1: Check if any visit_start_date is less than any index_date within the group
      condition1 <- any(sapply(.$visit_start_date, function(vsd) any(vsd < .$index_date)))
      # Condition 2: Any index_date equals any follow_up_end within the group
      condition2 <- any(sapply(.$index_date, function(id) id %in% .$follow_up_end))
      # Additional Condition: Ensure no index_date is the same as any follow_up_end within the group
      no_overlap <- !any(sapply(.$index_date, function(id) id %in% .$follow_up_end) & 
                           sapply(.$follow_up_end, function(fue) fue %in% .$index_date))
      
      condition1 && condition2 && no_overlap
    })) %>%
    filter(match_found) %>%
    ungroup() %>%
    # Count unique visit_occurrence_id and subject_id pairs
    summarise(
      distinct_subjects = n_distinct(subject_id),
      distinct_visit_occurrences = n_distinct(visit_occurrence_id)
    )
  
  # Cleaning
  
  ## Case A & A.1 -> we keep the second entry
  freq_cond_proc_tbl_hrg_cost_clean<- freq_cond_proc_tbl_hrg_cost_clean %>% 
    dplyr::group_by(subject_id, visit_occurrence_id) %>%
    dplyr::filter(if (any(visit_start_date == index_date)) visit_start_date == index_date else TRUE)
  
  ## Case B or C -> we keep the first entry
  freq_cond_proc_tbl_hrg_cost_clean <- freq_cond_proc_tbl_hrg_cost_clean %>% 
    dplyr::group_by(subject_id, visit_occurrence_id) %>%
    dplyr::filter(visit_start_date > index_date & visit_end_date < follow_up_end) #the second entry would have index date > than visit_start date
  
  
  # Lastly, when more than one HRG is associated with a visit & entry, keep the HRG with highest unit cost (if there are multiple entries during the same spell (possible for C2 we keep them all in unless the same entry appears more than once with the same hrg))
  freq_cond_proc_tbl_hrg_cost_clean<- freq_cond_proc_tbl_hrg_cost_clean %>%
    dplyr::group_by(visit_occurrence_id, index_date, exposed_yrs) %>%
    slice_max(order_by = Unit_cost, with_ties = FALSE) %>%
    dplyr::ungroup()
  
  # final checks to make sure we have one entry per spell 
  unique_pairs_count <- freq_cond_proc_tbl_hrg_cost_clean %>%
    dplyr::group_by(visit_occurrence_id) %>%
    summarise(
      unique_pairs = n_distinct(paste(index_date, follow_up_end)),
      .groups = 'drop'
    ) %>%
    # Check if any visit_occurrence_id has more than one unique pair
    filter(unique_pairs > 1)
  
  # Check if there are any visit_occurrence_id with more than one unique pair
  if(nrow(unique_pairs_count) > 0) {
    print("There are visit_occurrence_id(s) with more than one unique index_date and follow_up_end date pair.")
    info(logger, "There are visit_occurrence_id(s) with more than one unique index_date and follow_up_end date pair.")
  } else {
    print("Each visit_occurrence_id has a single correspondence with index_date and follow_up_end date pairs.")
    info(logger, "Each visit_occurrence_id has a single correspondence with index_date and follow_up_end date pairs.")
  }
  
  # final checks to make sure we have one hrg per spell  
  
  rows_per_visit <- freq_cond_proc_tbl_hrg_cost_clean %>%
    dplyr::group_by(visit_occurrence_id) %>%
    summarise(row_count = n(), .groups = 'drop') %>%
    # Check if any group has more than one row
    filter(row_count > 1)
  
  # Check if there are any visit_occurrence_id with more than one row
  if(nrow(rows_per_visit) > 0) {
    print("There are visit_occurrence_id(s) with more than one row.")
    info(logger,"There are visit_occurrence_id(s) with more than one row.")
  } else {
    print("Each visit_occurrence_id is unique or appears only once.")
    info(logger, "Each visit_occurrence_id is unique or appears only once.")
  }
  
  
  # Summary costs per fx related spells
  tot_fx_related_hosp <- n_distinct(freq_cond_proc_tbl_hrg_cost_clean$visit_occurrence_id)
  
  summary_cost_per_fx_related_hosp <- tibble(mean_HRG_cost_per_hosp = mean(freq_cond_proc_tbl_hrg_cost_clean$Unit_cost),
                                             min_HRG_cost_per_hosp = min(freq_cond_proc_tbl_hrg_cost_clean$Unit_cost),
                                             max_HRG_cost_per_hosp = max(freq_cond_proc_tbl_hrg_cost_clean$Unit_cost),
                                             sd_HRG_cost_per_hosp = round(sd(freq_cond_proc_tbl_hrg_cost_clean$Unit_cost), 2),
                                             median_HRG_cost_per_hosp = round(quantile(freq_cond_proc_tbl_hrg_cost_clean$Unit_cost, probs = (.5)), 2),
                                             lower_HRG_cost_per_hosp = round(quantile(freq_cond_proc_tbl_hrg_cost_clean$Unit_cost, probs = (.25)), 2),
                                             upper_HRG_cost_per_hosp = round(quantile(freq_cond_proc_tbl_hrg_cost_clean$Unit_cost, probs = (.75)), 2))
  
  # Summary LoS per fx related spells
  summary_LoS_per_fx_related_hosp <- tibble(mean_LoS_per_hosp = mean(freq_cond_proc_tbl_hrg_cost_clean$LoS),
                                            min_LoS_per_hosp = min(freq_cond_proc_tbl_hrg_cost_clean$LoS),
                                            max_LoS_per_hosp = max(freq_cond_proc_tbl_hrg_cost_clean$LoS),
                                            sd_LoS_per_hosp = round(sd(freq_cond_proc_tbl_hrg_cost_clean$LoS), 2),
                                            median_LoS_per_hosp = round(quantile(freq_cond_proc_tbl_hrg_cost_clean$LoS, probs = (.5)), 2),
                                            lower_LoS_per_hosp = round(quantile(freq_cond_proc_tbl_hrg_cost_clean$LoS, probs = (.25)), 2),
                                            upper_LoS_per_hosp = round(quantile(freq_cond_proc_tbl_hrg_cost_clean$LoS, probs = (.75)), 2))
  
  
  # Summary costs per entry-person/year
  
  costs_fx_related_hosp_per_person <- freq_cond_proc_tbl_hrg_cost_clean %>% 
    dplyr::group_by(subject_id, index_date, exposed_yrs) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(costs_per_yr = Unit_cost/exposed_yrs)
  
  summary_cost_fx_related_hosp_per_person_per_year <- tibble(mean_cost_per_person_per_year = sum(freq_cond_proc_tbl_hrg_cost_clean$Unit_cost)/sum(freq_cond_proc_tbl_hrg_cost_clean$exposed_yrs),
                                                             min_cost_per_person_per_year = min(costs_fx_related_hosp_per_person$costs_per_yr),
                                                             max_cost_per_person_per_year = max(costs_fx_related_hosp_per_person$costs_per_yr),
                                                             sd_cost_per_person_per_year = round(sd(costs_fx_related_hosp_per_person$costs_per_yr), 2),
                                                             median_cost_per_person_per_year = round(quantile(costs_fx_related_hosp_per_person$costs_per_yr, probs = (.5)), 2),
                                                             lower_q_cost_per_person_per_year = round(quantile(costs_fx_related_hosp_per_person$costs_per_yr, probs = (.25)), 2),
                                                             upper_q_cost_per_person_per_year = round(quantile(costs_fx_related_hosp_per_person$costs_per_yr, probs = (.75)), 2))
  
  # Sumamry spells per person
  
  mean_spells_per_subject <- freq_cond_proc_tbl_hrg_cost_clean %>%
    dplyr::group_by(subject_id) %>%
    summarise(spells = n_distinct(visit_occurrence_id)) %>%
    summarise(mean_spells = mean(spells))
  
  # frequency table primary fx related diagnosis
  
  primary_cond_freq_table <- freq_cond_proc_tbl_hrg_cost_clean %>%
    dplyr::group_by(condition_source_value) %>%
    summarise(n_spells = n_distinct(visit_occurrence_id),
              n_subjects = n_distinct (subject_id)) %>%
    arrange(desc(n_spells)) %>% 
    ungroup()
  
  # frequency table primary fx related procedure
  
  primary_proc_freq_table <- freq_cond_proc_tbl_hrg_cost_clean %>%
    dplyr::group_by(procedure_source_value) %>%
    summarise(n_spells = n_distinct(visit_occurrence_id),
              n_subjects = n_distinct (subject_id)) %>%
    arrange(desc(n_spells))%>% 
    ungroup()
  
  # frequency table HRG
  
  HRG_freq_table <- freq_cond_proc_tbl_hrg_cost_clean %>%
    dplyr::group_by(HRG) %>%
    dplyr::summarise(n_spells = n_distinct(visit_occurrence_id),
              n_subjects = n_distinct (subject_id)) %>%
    dplyr::arrange(desc(n_spells))%>% 
    dplyr::ungroup()
  
  ###### checking distributions for GLM #####
  
  fx_related_hosp_cost_distribuion<- ggplot(freq_cond_proc_tbl_hrg_cost_clean, aes(x = Unit_cost)) +
    geom_histogram(bins = 30, fill = "blue", color = "black") +
    theme_minimal() +
    labs(title = "Histogram of Unit_cost", x = "Unit_cost", y = "Frequency")
  
  
  ### summary ALL (users and non users) ------
  
  freq_cond_proc_tbl_hrg_cost_clean_all <- cohort_freq %>% 
    left_join(freq_cond_proc_tbl_hrg_cost_clean %>% dplyr::select(subject_id, HRG, Unit_cost),
              by = c("subject_id"),
              copy = T,
              relationship = "many-to-many") 
  
  
  freq_cond_proc_tbl_hrg_cost_clean_all <- freq_cond_proc_tbl_hrg_cost_clean_all %>% 
    dplyr::mutate(Unit_cost = case_when(
      is.na(Unit_cost) ~ 0, 
      TRUE ~ Unit_cost
    )) %>% 
    dplyr::mutate(cost_per_person_year = Unit_cost/exposed_yrs) %>% 
    ungroup()
  
  # Summary costs per entry-person/year
  
  summary_cost_fx_related_hosp_per_person_per_year_all <- tibble(mean_cost_per_person_per_year = sum(freq_cond_proc_tbl_hrg_cost_clean_all$Unit_cost)/sum(freq_cond_proc_tbl_hrg_cost_clean_all$exposed_yrs),
                                                                 min_cost_per_person_per_year = min(freq_cond_proc_tbl_hrg_cost_clean_all$cost_per_person_year),
                                                                 max_cost_per_person_per_year = max(freq_cond_proc_tbl_hrg_cost_clean_all$cost_per_person_year),
                                                                 sd_cost_per_person_per_year = round(sd(freq_cond_proc_tbl_hrg_cost_clean_all$cost_per_person_year), 2),
                                                                 median_cost_per_person_per_year = round(quantile(freq_cond_proc_tbl_hrg_cost_clean_all$cost_per_person_year, probs = (.5)), 2),
                                                                 lower_q_cost_per_person_per_year = round(quantile(freq_cond_proc_tbl_hrg_cost_clean_all$cost_per_person_year, probs = (.25)), 2),
                                                                 upper_q_cost_per_person_per_year = round(quantile(freq_cond_proc_tbl_hrg_cost_clean_all$cost_per_person_year, probs = (.75)), 2))

  return(list(# costs of all hospitalisations
    cond_outs = cond_outs,
    proc_outs = proc_outs,
    case_a = case_a,
    case_a.1 = case_a.1,
    case_b = case_b,
    case_c = case_c,
    tot_fx_related_hosp = tot_fx_related_hosp,
    summ_cost_per_fx_rlt_hosp = summary_cost_per_fx_related_hosp,
    summ_LoS_per_fx_rlt_hosp = summary_LoS_per_fx_related_hosp,
    summ_cost_fx_rlt_hosp_pppy = summary_cost_fx_related_hosp_per_person_per_year,
    mean_spells_per_subject = mean_spells_per_subject,
    primary_cond_freq_table = primary_cond_freq_table,
    primary_proc_freq_table = primary_proc_freq_table,
    fx_related_hosp_cost_dist = fx_related_hosp_cost_distribuion,
    summ_cost_fx_rlt_hosp_pppy_all = summary_cost_fx_related_hosp_per_person_per_year_all

  ))
  
  }
