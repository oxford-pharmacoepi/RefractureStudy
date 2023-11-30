load("~/RefractureStudy/Results_AURUM/90/tempData/subclasses01.RData")
load("~/RefractureStudy/Results_AURUM/90/tempData/subclasses12.RData")

cdm[["before_matching"]] <- 
  Reduce(dplyr::union_all, targetCohort) %>%
  dplyr::select(subject_id, index_date, period) %>% 
  dplyr::rename(cohort_start_date = index_date) %>% 
  dplyr::mutate(cohort_end_date = cohort_start_date) %>% 
  dplyr::mutate(cohort_name = "target") %>%
  dplyr::distinct() %>% 
  dplyr::mutate(cohort_definition_id = 1) %>%
  dplyr::union_all(Reduce(dplyr::union_all, compCohort1) %>%
                     dplyr::select(subject_id, index_date, period) %>% 
                     dplyr::rename(cohort_start_date = index_date) %>% 
                     dplyr::mutate(cohort_end_date = cohort_start_date) %>% 
                     dplyr::mutate(cohort_name = "cohort1") %>%
                     dplyr::distinct() %>% 
                     dplyr::mutate(cohort_definition_id = 2)) %>%
  dplyr::union_all(Reduce(dplyr::union_all, compCohort2) %>%
                     dplyr::select(subject_id, index_date, period) %>% 
                     dplyr::rename(cohort_start_date = index_date) %>% 
                     dplyr::mutate(cohort_end_date = cohort_start_date) %>% 
                     dplyr::mutate(cohort_name = "cohort2") %>%
                     dplyr::distinct() %>% 
                     dplyr::mutate(cohort_definition_id = 3)) %>%
  computeQuery(
    name = "before_matching", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

cdm[["refracture"]] <- cdm[["before_matching"]] %>%
  dplyr::filter(period==i) %>% 
  dplyr::select(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date) %>%
  computeQuery(
    name = "refracture", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )


fullName <- inSchema(attr(cdm, "write_schema"), "refracture")
dbWriteTable(attr(cdm, "dbcon"), CDMConnector::inSchema(schema = attr(cdm, "write_schema"), table = "refracture"), cdm[["refracture"]], overwrite = T)
cdm[["refracture"]] <- dplyr::tbl(attr(cdm , "dbcon"), fullName)

refracture_set <- cdm[["before_matching"]] %>% 
  dplyr::select("cohort_definition_id", "cohort_name") %>% 
  dplyr::distinct() %>% 
  computeQuery(
    name = "refracture_set", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

#cohort count
refracture_count <- cdm[["before_matching"]] %>%
  dplyr::group_by(cohort_definition_id) %>%
  dplyr::tally() %>%
  dplyr::compute() %>%
  dplyr::rename(number_records = n) %>%
  dplyr::mutate(number_subjects = number_records) %>%
  computeQuery(
    name = "refracture_count", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

stem_table <- "rq3"
conditions <- paste0(stem_table, "_conditions")
medications <- paste0(stem_table, "_medications")
cdm_char <-CDMConnector::cdm_from_con(
  con = db,
  cdm_schema = cdm_database_schema,
  write_schema = results_database_schema
)

cdm_char[["refracture"]] <- newGeneratedCohortSet(cohortRef = cdm[["refracture"]],
                                                    cohortSetRef = refracture_set,
                                                    cohortCountRef = refracture_count,
                                                    overwrite = T)
cdm_char <- CDMConnector::cdmSubsetCohort(cdm_char, "refracture", verbose = T)

# instantiate medications
info(logger, "INSTANTIATE MEDICATIONS - IMMINENT COHORT")
codelistMedications <- codesFromConceptSet(here("1_InstantiateCohorts", "Medications"), cdm_char)
cdm_char <- generateDrugUtilisationCohortSet(cdm = cdm_char, name = medications, conceptSet = codelistMedications)

# instantiate conditions
info(logger, "INSTANTIATE CONDITIONS - IMMINENT COHORT")
codelistConditions <- codesFromConceptSet(here("1_InstantiateCohorts", "Conditions"), cdm_char)
cdm_char <- generateConceptCohortSet(cdm = cdm_char, name = conditions, conceptSet = codelistConditions, overwrite = T)

# create table summary
info(logger, "CREATE SUMMARY - IMMINENT COHORT")
result_matching12 <- cdm_char[["refracture"]] %>%
  summariseCharacteristics(
    ageGroup = list(c(50, 59), c(60, 69), c(70,79), c(80,89), c(90,99), c(100,150)),
    tableIntersect = list(
      "Visits" = list(
        tableName = "visit_occurrence", value = "count", window = c(-365, 0)
      ),
      "Medications" = list(
        tableName = "drug_era", value = "count", window = c(-365, 0)
      )
    ), 
    cohortIntersect = list(
      "Medications" = list(
        targetCohortTable = medications, value = "flag", window = c(-365, 0)
      ),
      "Conditions" = list(
        targetCohortTable = conditions, value = "flag", window = c(-Inf, 0)
      )
    )
  )
write_csv(result_matching12, here(output.folder, "result_matching12", i, ".csv"))

############### C1 VS C2 period 4
cdm[["before_matching"]] <- 
Reduce(dplyr::union_all, compCohort1) %>%
                     dplyr::select(subject_id, index_date, period) %>% 
                     dplyr::rename(cohort_start_date = index_date) %>% 
                     dplyr::mutate(cohort_end_date = cohort_start_date) %>% 
                     dplyr::mutate(cohort_name = "cohort1") %>%
                     dplyr::distinct() %>% 
                     dplyr::mutate(cohort_definition_id = 1) %>%
  dplyr::union_all(Reduce(dplyr::union_all, compCohort2) %>%
                     dplyr::select(subject_id, index_date, period) %>% 
                     dplyr::rename(cohort_start_date = index_date) %>% 
                     dplyr::mutate(cohort_end_date = cohort_start_date) %>% 
                     dplyr::mutate(cohort_name = "cohort2") %>%
                     dplyr::distinct() %>% 
                     dplyr::mutate(cohort_definition_id = 2)) %>%
  computeQuery(
    name = "before_matching", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

cdm[["refracture"]] <- cdm[["before_matching"]] %>%
  dplyr::filter(period==4) %>% 
  dplyr::select(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date) %>%
  computeQuery(
    name = "refracture", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

fullName <- inSchema(attr(cdm, "write_schema"), "refracture")
dbWriteTable(attr(cdm, "dbcon"), CDMConnector::inSchema(schema = attr(cdm, "write_schema"), table = "refracture"), cdm[["refracture"]], overwrite = T)
cdm[["refracture"]] <- dplyr::tbl(attr(cdm , "dbcon"), fullName)

refracture_set <- cdm[["before_matching"]] %>% 
  dplyr::filter(period == 4) %>% 
  dplyr::select("cohort_definition_id", "cohort_name") %>% 
  dplyr::distinct() %>% 
  computeQuery(
    name = "refracture_set", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

#cohort count
refracture_count <- cdm[["before_matching"]] %>%
  dplyr::filter(period == 4) %>% 
  dplyr::group_by(cohort_definition_id) %>%
  dplyr::tally() %>%
  dplyr::compute() %>%
  dplyr::rename(number_records = n) %>%
  dplyr::mutate(number_subjects = number_records) %>%
  computeQuery(
    name = "refracture_count", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

stem_table <- "rq3"
conditions <- paste0(stem_table, "_conditions")
medications <- paste0(stem_table, "_medications")
cdm_char <-CDMConnector::cdm_from_con(
  con = db,
  cdm_schema = cdm_database_schema,
  write_schema = results_database_schema
)

cdm_char[["refracture"]] <- newGeneratedCohortSet(cohortRef = cdm[["refracture"]],
                                                  cohortSetRef = refracture_set,
                                                  cohortCountRef = refracture_count,
                                                  overwrite = T)
cdm_char <- CDMConnector::cdmSubsetCohort(cdm_char, "refracture", verbose = T)

# instantiate medications
info(logger, "INSTANTIATE MEDICATIONS - IMMINENT COHORT")
codelistMedications <- codesFromConceptSet(here("1_InstantiateCohorts", "Medications"), cdm_char)
cdm_char <- generateDrugUtilisationCohortSet(cdm = cdm_char, name = medications, conceptSet = codelistMedications)

# instantiate conditions
info(logger, "INSTANTIATE CONDITIONS - IMMINENT COHORT")
codelistConditions <- codesFromConceptSet(here("1_InstantiateCohorts", "Conditions"), cdm_char)
cdm_char <- generateConceptCohortSet(cdm = cdm_char, name = conditions, conceptSet = codelistConditions, overwrite = T)

# create table summary
info(logger, "CREATE SUMMARY - IMMINENT COHORT")
result_before_matching12 <- cdm_char[["refracture"]] %>%
  summariseCharacteristics(
    ageGroup = list(c(50, 59), c(60, 69), c(70,79), c(80,89), c(90,99), c(100,150)),
    tableIntersect = list(
      "Visits" = list(
        tableName = "visit_occurrence", value = "count", window = c(-365, 0)
      ),
      "Medications" = list(
        tableName = "drug_era", value = "count", window = c(-365, 0)
      )
    ), 
    cohortIntersect = list(
      "Medications" = list(
        targetCohortTable = medications, value = "flag", window = c(-365, 0)
      ),
      "Conditions" = list(
        targetCohortTable = conditions, value = "flag", window = c(-Inf, 0)
      )
    )
  )
write_csv(result_before_matching12, here(output_folder, "result_before_matching12.csv"))

################################ quick check
ids_1 <- cdm_char$refracture %>% 
  dplyr::filter(cohort_definition_id == 1) %>% 
  dplyr::pull(subject_id)

ids_2 <- cdm_char$refracture %>% 
  dplyr::filter(cohort_definition_id == 2) %>% 
  dplyr::pull(subject_id)

ids_1_after <- subclasses12[[4]] %>% 
  dplyr::filter(group == "comparator 1") %>% 
  dplyr::pull(subject_id)

ids_2_after <- subclasses12[[4]] %>% 
  dplyr::filter(group == "comparator 2") %>% 
  dplyr::pull(subject_id)

expect_true(all(ids_1_after %in% ids_1))
expect_true(all(ids_2_after %in% ids_2))

cdm[["after_matching"]] <- 
  Reduce(dplyr::union_all, compCohort1) %>%
  dplyr::filter(subject_id %in% ids_1_after) %>% 
  dplyr::select(subject_id, index_date, period) %>% 
  dplyr::rename(cohort_start_date = index_date) %>% 
  dplyr::mutate(cohort_end_date = cohort_start_date) %>% 
  dplyr::mutate(cohort_name = "cohort1") %>%
  dplyr::distinct() %>% 
  dplyr::mutate(cohort_definition_id = 1) %>%
  dplyr::union_all(Reduce(dplyr::union_all, compCohort2) %>%
                     dplyr::filter(subject_id %in% ids_2_after) %>% 
                     dplyr::select(subject_id, index_date, period) %>% 
                     dplyr::rename(cohort_start_date = index_date) %>% 
                     dplyr::mutate(cohort_end_date = cohort_start_date) %>% 
                     dplyr::mutate(cohort_name = "cohort2") %>%
                     dplyr::distinct() %>% 
                     dplyr::mutate(cohort_definition_id = 2)) %>%
  computeQuery(
    name = "after_matching", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

cdm[["refracture_after"]] <- cdm[["after_matching"]] %>%
  dplyr::filter(period==4) %>% 
  dplyr::select(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date) %>%
  computeQuery(
    name = "refracture_after", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

fullName <- inSchema(attr(cdm, "write_schema"), "refracture_after")
dbWriteTable(attr(cdm, "dbcon"), CDMConnector::inSchema(schema = attr(cdm, "write_schema"), table = "refracture_after"), cdm[["refracture_after"]], overwrite = T)
cdm[["refracture_after"]] <- dplyr::tbl(attr(cdm , "dbcon"), fullName)

refracture_after_set <- cdm[["after_matching"]] %>% 
  dplyr::filter(period == 4) %>% 
  dplyr::select("cohort_definition_id", "cohort_name") %>% 
  dplyr::distinct() %>% 
  computeQuery(
    name = "refracture_after_set", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

#cohort count
refracture_after_count <- cdm[["after_matching"]] %>%
  dplyr::filter(period == 4) %>% 
  dplyr::group_by(cohort_definition_id) %>%
  dplyr::tally() %>%
  dplyr::compute() %>%
  dplyr::rename(number_records = n) %>%
  dplyr::mutate(number_subjects = number_records) %>%
  computeQuery(
    name = "refracture_after_count", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

stem_table <- "rq3"
conditions <- paste0(stem_table, "_conditions")
medications <- paste0(stem_table, "_medications")
cdm_char <-CDMConnector::cdm_from_con(
  con = db,
  cdm_schema = cdm_database_schema,
  write_schema = results_database_schema
)

cdm_char[["refracture_after"]] <- newGeneratedCohortSet(cohortRef = cdm[["refracture_after"]],
                                                  cohortSetRef = refracture_after_set,
                                                  cohortCountRef = refracture_after_count,
                                                  overwrite = T)
cdm_char <- CDMConnector::cdmSubsetCohort(cdm_char, "refracture_after", verbose = T)

# instantiate medications
info(logger, "INSTANTIATE MEDICATIONS - IMMINENT COHORT")
codelistMedications <- codesFromConceptSet(here("1_InstantiateCohorts", "Medications"), cdm_char)
cdm_char <- generateDrugUtilisationCohortSet(cdm = cdm_char, name = medications, conceptSet = codelistMedications)

# instantiate conditions
info(logger, "INSTANTIATE CONDITIONS - IMMINENT COHORT")
codelistConditions <- codesFromConceptSet(here("1_InstantiateCohorts", "Conditions"), cdm_char)
cdm_char <- generateConceptCohortSet(cdm = cdm_char, name = conditions, conceptSet = codelistConditions, overwrite = T)

# create table summary
info(logger, "CREATE SUMMARY - IMMINENT COHORT")
result_after_matching12 <- cdm_char[["refracture_after"]] %>%
  summariseCharacteristics(
    ageGroup = list(c(50, 59), c(60, 69), c(70,79), c(80,89), c(90,99), c(100,150)),
    tableIntersect = list(
      "Visits" = list(
        tableName = "visit_occurrence", value = "count", window = c(-365, 0)
      ),
      "Medications" = list(
        tableName = "drug_era", value = "count", window = c(-365, 0)
      )
    ), 
    cohortIntersect = list(
      "Medications" = list(
        targetCohortTable = medications, value = "flag", window = c(-365, 0)
      ),
      "Conditions" = list(
        targetCohortTable = conditions, value = "flag", window = c(-Inf, 0)
      )
    )
  )
write_csv(result_after_matching12, here(output_folder, "result_after_matching12.csv"))
############### reformat
### reformat 
table_one_1 <- result_before_matching12 %>%
  dplyr::filter(group_level == "Cohort1")

table_one_2 <- result_before_matching12 %>%
  dplyr::filter(group_level == "Cohort2")

reformat_table_one_RQ3 <- function(table_one_1, table_one_2){
  
  reformatted_table1 <- data.frame(x = NA, y= NA, z = NA)
  n1 <- table_one_1 %>% dplyr::filter(variable == "Number subjects") %>% dplyr::pull(estimate)
  n2 <- table_one_2 %>% dplyr::filter(variable == "Number subjects") %>% dplyr::pull(estimate)
  
  # variables assembled by min/max etc
  cat_var <- table_one_1 %>% dplyr::filter(estimate_type == "min") %>% dplyr::select(variable) %>% dplyr::distinct() %>% dplyr::pull(variable)
  
  for (i in (1:length(cat_var))){
    reformatted_table1 <- rbind(reformatted_table1, 
                                data.frame(x = paste0(cat_var[[i]], ", median (IQR)"), 
                                           y = paste0(table_one_1 %>% dplyr::filter(variable == cat_var[[i]]) %>% dplyr::filter(estimate_type == "median") %>% dplyr::pull(estimate),
                                                      " (",
                                                      table_one_1 %>% dplyr::filter(variable == cat_var[[i]]) %>% dplyr::filter(estimate_type == "q25") %>% dplyr::pull(estimate),
                                                      "-",
                                                      table_one_1 %>% dplyr::filter(variable == cat_var[[i]]) %>% dplyr::filter(estimate_type == "q75") %>% dplyr::pull(estimate),
                                                      ")"),
                                           z = paste0(table_one_2 %>% dplyr::filter(variable == cat_var[[i]]) %>% dplyr::filter(estimate_type == "median") %>% dplyr::pull(estimate),
                                                      " (",
                                                      table_one_2 %>% dplyr::filter(variable == cat_var[[i]]) %>% dplyr::filter(estimate_type == "q25") %>% dplyr::pull(estimate),
                                                      "-",
                                                      table_one_2 %>% dplyr::filter(variable == cat_var[[i]]) %>% dplyr::filter(estimate_type == "q75") %>% dplyr::pull(estimate),
                                                      ")")))
  }
  
  # age group variables
  age_var <- table_one_1 %>% 
    dplyr::filter(variable == "Age group") %>% 
    dplyr::select(variable_level) %>%
    dplyr::distinct() %>%
    dplyr::pull()
  
  for (i in (1:length(age_var))){
    reformatted_table1 <- rbind(reformatted_table1, data.frame(x = paste0("Age Group, ", age_var[[i]], ", n(%)"),
                                                               y = paste0(table_one_1 %>% dplyr::filter(variable_level == age_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
                                                                          " (",
                                                                          round(as.numeric(table_one_1 %>% dplyr::filter(variable_level == age_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate)), digits = 1),
                                                                          ")"),
                                                               z = paste0(table_one_2 %>% dplyr::filter(variable_level == age_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
                                                                          " (",
                                                                          round(as.numeric(table_one_2 %>% dplyr::filter(variable_level == age_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate)), digits = 1),
                                                                          ")")
    ))
  }
  
  
  #condition variables
  condition_var <- table_one_1 %>% 
    dplyr::filter(variable == "Conditions flag -inf to 0 days") %>%
    dplyr::select(variable_level) %>%
    dplyr::distinct() %>%
    dplyr::pull(variable_level) 
  
  for (i in (1:length(condition_var))){
    reformatted_table1 <- rbind(reformatted_table1, data.frame(x = paste0(condition_var[[i]], ", n(%)"),
                                                               y = paste0(table_one_1 %>% dplyr::filter(variable_level == condition_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
                                                                          " (",
                                                                          round(as.numeric(table_one_1 %>% dplyr::filter(variable_level == condition_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate)), digits = 1),
                                                                          ")"),
                                                               z = paste0(table_one_2 %>% dplyr::filter(variable_level == condition_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
                                                                          " (",
                                                                          round(as.numeric(table_one_2 %>% dplyr::filter(variable_level == condition_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate)), digits = 1),
                                                                          ")")))
  }
  
  #medication variables
  medication_var <- table_one_1 %>% 
    dplyr::filter(stringr::str_detect(variable, 'medications')) %>%
    dplyr::select(variable) %>%
    dplyr::distinct() %>%
    dplyr::pull(variable)
  
  for (i in (1:length(medication_var))){
    reformatted_table1 <- rbind(reformatted_table1, data.frame(x = paste0(medication_var[[i]], ", n(%)"),
                                                               y = paste0(table_one_1 %>% dplyr::filter(variable == medication_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
                                                                          " (",
                                                                          round(as.numeric(table_one_1 %>% dplyr::filter(variable == medication_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate)), digits = 1),
                                                                          ")"),
                                                               z = paste0(table_one_2 %>% dplyr::filter(variable == medication_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
                                                                          " (",
                                                                          round(as.numeric(table_one_2 %>% dplyr::filter(variable == medication_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate)), digits = 1),
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
    paste0("Comparator Cohort 1 (n = ", n1, ")"),
    paste0("Comparator Cohort 2 (n = ", n2, ")")
  )
  return(reformatted_table1)
}


reformatted_table_1 <- reformat_table_one_RQ3(table_one_1 = table_one_1,
                                          table_one_2 = table_one_2)

write_csv(reformatted_table_1, here(output_folder, "reformatted_table_1_before_matching.csv"))

table_one_1 <- result_after_matching12 %>%
  dplyr::filter(group_level == "Cohort1")

table_one_2 <- result_after_matching12 %>%
  dplyr::filter(group_level == "Cohort2")

reformatted_table_1 <- reformat_table_one_RQ3(table_one_1 = table_one_1,
                                              table_one_2 = table_one_2)

write_csv(reformatted_table_1, here(output_folder, "reformatted_table_1_after_matching.csv"))
