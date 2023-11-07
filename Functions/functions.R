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
    dplyr::inner_join(targetCohort[[i]] %>% dplyr::select(subject_id, index_date), by = c("subject_id", "index_date")) %>%
    dplyr::distinct() %>%
    dplyr::group_by(feature) %>%
    dplyr::tally() %>% 
    dplyr::filter(n>=lower_bound) %>% 
    dplyr::pull(feature)
  
  subfeatures_01_1 <- subfeatures_01 %>%
    dplyr::inner_join(compCohort1[[i]] %>% dplyr::select(subject_id, index_date), by = c("subject_id", "index_date")) %>%
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
    dplyr::inner_join(compCohort1[[i]] %>% dplyr::select(subject_id, index_date), by = c("subject_id", "index_date")) %>%
    dplyr::distinct() %>%
    dplyr::group_by(feature) %>%
    dplyr::tally() %>% dplyr::filter(n>=lower_bound) %>% 
    dplyr::pull(feature)
  
  subfeatures_12_2 <- subfeatures_12 %>%
    dplyr::inner_join(compCohort2[[i]] %>% dplyr::select(subject_id, index_date), by = c("subject_id", "index_date")) %>%
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
                                                                          round(as.numeric(table_one_frac %>% dplyr::filter(variable_level == age_var[[i]]) %>% dplyr::filter(estimate_type == "%") %>% dplyr::pull(estimate)), digits = 1),
                                                                          ")"),
                                                               z = paste0(table_one_imm %>% dplyr::filter(variable_level == age_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
                                                                          " (",
                                                                          round(as.numeric(table_one_imm %>% dplyr::filter(variable_level == age_var[[i]]) %>% dplyr::filter(estimate_type == "%") %>% dplyr::pull(estimate)), digits = 1),
                                                                          ")"),
                                                               w = paste0(table_one_no_imm %>% dplyr::filter(variable_level == age_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
                                                                          " (",
                                                                          round(as.numeric(table_one_no_imm %>% dplyr::filter(variable_level == age_var[[i]]) %>% dplyr::filter(estimate_type == "%") %>% dplyr::pull(estimate)), digits = 1),
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
                                                                          round(as.numeric(table_one_frac %>% dplyr::filter(variable_level == condition_var[[i]]) %>% dplyr::filter(estimate_type == "%") %>% dplyr::pull(estimate)), digits = 1),
                                                                          ")"),
                                                               z = paste0(table_one_imm %>% dplyr::filter(variable_level == condition_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
                                                                          " (",
                                                                          round(as.numeric(table_one_imm %>% dplyr::filter(variable_level == condition_var[[i]]) %>% dplyr::filter(estimate_type == "%") %>% dplyr::pull(estimate)), digits = 1),
                                                                          ")"),
                                                               w = paste0(table_one_no_imm %>% dplyr::filter(variable_level == condition_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
                                                                          " (",
                                                                          round(as.numeric(table_one_no_imm %>% dplyr::filter(variable_level == condition_var[[i]]) %>% dplyr::filter(estimate_type == "%") %>% dplyr::pull(estimate)), digits = 1),
                                                                          ")")))
  }
  
  #medication variables
  medication_var <- table_one_frac %>% 
    dplyr::filter(stringr::str_detect(variable, 'Ref')) %>%
    dplyr::filter(!variable == "Ref medications antineoplastic agents flag m365 to 0") %>%
    dplyr::select(variable) %>%
    dplyr::distinct() %>%
    dplyr::pull(variable)
  
  for (i in (1:length(medication_var))){
    reformatted_table1 <- rbind(reformatted_table1, data.frame(x = paste0(medication_var[[i]], ", n(%)"),
                                                               y = paste0(table_one_frac %>% dplyr::filter(variable == medication_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
                                                                          " (",
                                                                          round(as.numeric(table_one_frac %>% dplyr::filter(variable == medication_var[[i]]) %>% dplyr::filter(estimate_type == "%") %>% dplyr::pull(estimate)), digits = 1),
                                                                          ")"),
                                                               z = paste0(table_one_imm %>% dplyr::filter(variable == medication_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
                                                                          " (",
                                                                          round(as.numeric(table_one_imm %>% dplyr::filter(variable == medication_var[[i]]) %>% dplyr::filter(estimate_type == "%") %>% dplyr::pull(estimate)), digits = 1),
                                                                          ")"),
                                                               w = paste0(table_one_no_imm %>% dplyr::filter(variable == medication_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
                                                                          " (",
                                                                          round(as.numeric(table_one_no_imm %>% dplyr::filter(variable == medication_var[[i]]) %>% dplyr::filter(estimate_type == "%") %>% dplyr::pull(estimate)), digits = 1),
                                                                          ")")))
  }
  reformatted_table1 <- reformatted_table1 %>% dplyr::distinct()
  ### adjusting colnames
  reformatted_table1_meds <- reformatted_table1 %>%
    dplyr::filter(stringr::str_detect(x, 'Ref'))
  reformatted_table1_meds[["x"]]<-substr(reformatted_table1_meds$x, 16, nchar(reformatted_table1_meds$x)-21)
  reformatted_table1_meds <- reformatted_table1_meds %>% dplyr::mutate(x = paste0(x, ", n(%)"))
  
  reformatted_table1 <- rbind(reformatted_table1 %>% dplyr::filter(!stringr::str_detect(x, 'Ref')), reformatted_table1_meds)
  
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
