#### characterisation 
info(logger, "CHARACTERISATION: DEMOGRAPHICS")
cdm[["no_imminent_fracture_cohort"]] <- cdm[["denominator"]] %>%
  dplyr::inner_join(withoutImminentFractureCohortTotal, by = "subject_id", copy = T) %>%
  dplyr::left_join(entryTable[[1]] %>% dplyr::select(subject_id, index_date), by = "subject_id", copy = T) %>%
  dplyr::select(-cohort_start_date, -cohort_end_date) %>%
  dplyr::rename(cohort_start_date = index_date) %>% 
  dplyr::mutate(cohort_end_date = cohort_start_date) %>%
  dplyr::distinct() %>%
  computeQuery(
    name = "no_imminent_fracture_cohort", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

cdm[["imminent_fracture_cohort"]] <-cdm[["denominator"]] %>%
  dplyr::inner_join(imminentFractureCohortTotal, by = "subject_id", copy = T) %>%
  dplyr::left_join(entryTable[[1]] %>% dplyr::select(subject_id, index_date), by = "subject_id", copy = T) %>%
  dplyr::select(-cohort_start_date, -cohort_end_date) %>%
  dplyr::rename(cohort_start_date = index_date) %>% 
  dplyr::mutate(cohort_end_date = cohort_start_date) %>%
  dplyr::distinct() %>%
  computeQuery(
    name = "imminent_fracture_cohort", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

cdm[["fracture_cohort"]] <-cdm[["denominator"]] %>%
  dplyr::right_join(entryTable[[1]] %>% dplyr::select(subject_id, index_date), by = "subject_id", copy = T) %>%
  dplyr::select(-cohort_start_date, -cohort_end_date) %>%
  dplyr::rename(cohort_start_date = index_date) %>% 
  dplyr::mutate(cohort_end_date = cohort_start_date) %>%
  dplyr::distinct() %>%
  computeQuery(
    name = "fracture_cohort", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

# Create cohort set
no_imminent_fracture_cohort_set <- cdm[["no_imminent_fracture_cohort"]] %>% 
  dplyr::select("cohort_definition_id") %>% 
  dplyr::distinct() %>% 
  dplyr::mutate(cohort_name = if_else(cohort_definition_id == 1, "no_imminent_fracture", "else")) %>%
  computeQuery(
    name = "no_imminent_fracture_cohort_set", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

imminent_fracture_cohort_set <- cdm[["imminent_fracture_cohort"]] %>% 
  dplyr::select("cohort_definition_id") %>% 
  dplyr::distinct() %>% 
  dplyr::mutate(cohort_name = if_else(cohort_definition_id == 1, "imminent_fracture", "else")) %>%
  computeQuery(
    name = "imminent_fracture_cohort_set", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

fracture_cohort_set <- cdm[["fracture_cohort"]] %>% 
  dplyr::select("cohort_definition_id") %>% 
  dplyr::distinct() %>% 
  dplyr::mutate(cohort_name = if_else(cohort_definition_id == 1, "fracture", "else")) %>%
  computeQuery(
    name = "fracture_cohort_set", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

# Create cohort count
no_imminent_fracture_cohort_count <- cdm[["no_imminent_fracture_cohort"]] %>%
  dplyr::group_by(cohort_definition_id) %>%
  dplyr::tally() %>%
  dplyr::compute() %>%
  dplyr::rename(number_records = n) %>%
  dplyr::mutate(number_subjects = number_records) %>%
  computeQuery(
    name = "no_imminent_fracture_cohort_count", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

imminent_fracture_cohort_count <- cdm[["imminent_fracture_cohort"]] %>%
  dplyr::group_by(cohort_definition_id) %>%
  dplyr::tally() %>%
  dplyr::compute() %>%
  dplyr::rename(number_records = n) %>%
  dplyr::mutate(number_subjects = number_records) %>%
  computeQuery(
    name = "imminent_fracture_cohort_count", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

fracture_cohort_count <- cdm[["fracture_cohort"]] %>%
  dplyr::group_by(cohort_definition_id) %>%
  dplyr::tally() %>%
  dplyr::compute() %>%
  dplyr::rename(number_records = n) %>%
  dplyr::mutate(number_subjects = number_records) %>%
  computeQuery(
    name = "fracture_cohort_count", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

#### commodity and medications
info(logger, "CHARACTERISATION: COMORBIDITIES AND MEDICAL HISTORY - IMMINENT COHORT")
conditions <- paste0(stem_table, "_conditions")
medications <- paste0(stem_table, "_medications")

### 1. With imminent fractures
cdm_char_imm<-CDMConnector::cdm_from_con(
  con = db,
  cdm_schema = cdm_database_schema,
  write_schema = results_database_schema
)

cdm_char_imm[["imminent_fracture_cohort"]] <- newGeneratedCohortSet(cohortRef = cdm[["imminent_fracture_cohort"]],
                                                                  cohortSetRef = imminent_fracture_cohort_set,
                                                                  cohortCountRef = imminent_fracture_cohort_count,
                                                                  overwrite = T)

cdm_char_imm <- cdmSubsetCohort(cdm_char_imm, "imminent_fracture_cohort", verbose = T)

# instantiate medications
info(logger, "INSTANTIATE MEDICATIONS - IMMINENT COHORT")
codelistMedications <- codesFromConceptSet(here("1_InstantiateCohorts", "Medications"), cdm_char_imm)
cdm_char_imm <- generateDrugUtilisationCohortSet(cdm = cdm_char_imm, name = medications, conceptSet = codelistMedications)

# instantiate conditions
info(logger, "INSTANTIATE CONDITIONS - IMMINENT COHORT")
codelistConditions <- codesFromConceptSet(here("1_InstantiateCohorts", "Conditions"), cdm_char_imm)
cdm_char_imm <- generateConceptCohortSet(cdm = cdm_char_imm, name = conditions, conceptSet = codelistConditions, overwrite = T)

# create table summary
info(logger, "CREATE SUMMARY - IMMINENT COHORT")

cdm_char_imm[["drug_era"]] <- cdm_char_imm[["drug_era"]] %>% 
  dplyr::inner_join(cdm_char_imm[["concept"]] %>% dplyr::filter(concept_class_id == "Ingredient", standard_concept == "S"), by = c("drug_concept_id" = "concept_id")) %>%
  dplyr::select(colnames(cdm_char_imm[["drug_era"]])) %>%
  dplyr::filter(lubridate::year(.data$drug_era_start_date) >= 2009) %>%
  dplyr::compute()
  
cdm_char_imm[["visit_occurence"]] <- cdm_char_imm[["visit_occurrence"]] %>%  
  dplyr::filter(lubridate::year(.data$visit_start_date) >= 2009) %>%
  dplyr::compute()

result_imm <- cdm_char_imm[["imminent_fracture_cohort"]] %>%
  summariseCharacteristics(
    ageGroup = list(c(50, 54), c(55, 59), c(60, 64), c(65, 69), c(70, 74), c(75,79), c(80,84), c(85,89), c(90,94), c(95,99), c(100,150)),
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
info(logger, "TABLE 1 IMMINENT COHORT IS DONE")

### 2. Without imminent fractures
cdm_char_no_imm<-CDMConnector::cdm_from_con(
  con = db,
  cdm_schema = cdm_database_schema,
  write_schema = results_database_schema
)

cdm_char_no_imm[["no_imminent_fracture_cohort"]] <- newGeneratedCohortSet(cohortRef = cdm[["no_imminent_fracture_cohort"]],
                                                                       cohortSetRef = no_imminent_fracture_cohort_set,
                                                                       cohortCountRef = no_imminent_fracture_cohort_count,
                                                                       overwrite = T)

cdm_char_no_imm <- cdmSubsetCohort(cdm_char_no_imm, "no_imminent_fracture_cohort", verbose = T)

# instantiate medications
info(logger, "INSTANTIATE MEDICATIONS - NO IMMINENT COHORT")
codelistMedications <- codesFromConceptSet(here("1_InstantiateCohorts", "Medications"), cdm_char_no_imm)
cdm_char_no_imm <- generateDrugUtilisationCohortSet(cdm = cdm_char_no_imm, name = medications, conceptSet = codelistMedications)

# instantiate conditions
info(logger, "INSTANTIATE CONDITIONS - NO IMMINENT COHORT")
codelistConditions <- codesFromConceptSet(here("1_InstantiateCohorts", "Conditions"), cdm_char_no_imm)
cdm_char_no_imm <- generateConceptCohortSet(cdm = cdm_char_no_imm, name = conditions, conceptSet = codelistConditions, overwrite = T)

# create table summary
info(logger, "CREATE SUMMARY - NO IMMINENT COHORT")

cdm_char_no_imm[["drug_era"]] <- cdm_char_no_imm[["drug_era"]] %>% 
  dplyr::inner_join(cdm_char_no_imm[["concept"]] %>% dplyr::filter(concept_class_id == "Ingredient", standard_concept == "S"), by = c("drug_concept_id" = "concept_id")) %>%
  dplyr::select(colnames(cdm_char_no_imm[["drug_era"]])) %>%
  dplyr::filter(lubridate::year(.data$drug_era_start_date) >= 2009) %>%
  dplyr::compute()

cdm_char_no_imm[["visit_occurence"]] <- cdm_char_no_imm[["visit_occurrence"]] %>%  
  dplyr::filter(lubridate::year(.data$visit_start_date) >= 2009) %>%
  dplyr::compute()

result_no_imm <- cdm_char_no_imm[["no_imminent_fracture_cohort"]] %>%
  summariseCharacteristics(
    ageGroup = list(c(50, 54), c(55, 59), c(60, 64), c(65, 69), c(70, 74), c(75,79), c(80,84), c(85,89), c(90,94), c(95,99), c(100,150)),
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
info(logger, "CREATE SUMMARY, NO IMMINENT COHORT, IS DONE")

### 3. Total - fracture cohort
cdm_char_frac<-CDMConnector::cdm_from_con(
  con = db,
  cdm_schema = cdm_database_schema,
  write_schema = results_database_schema
)

cdm_char_frac[["fracture_cohort"]] <- newGeneratedCohortSet(cohortRef = cdm[["fracture_cohort"]],
                                                                    cohortSetRef = fracture_cohort_set,
                                                                    cohortCountRef = fracture_cohort_count,
                                                                    overwrite = T)

cdm_char_frac <- cdmSubsetCohort(cdm_char_frac, "fracture_cohort", verbose = T)

# instantiate medications
info(logger, "INSTANTIATE MEDICATIONS - FRACTURE COHORT")
codelistMedications <- codesFromConceptSet(here("1_InstantiateCohorts", "Medications"), cdm_char_frac)
cdm_char_frac <- generateDrugUtilisationCohortSet(cdm = cdm_char_frac, name = medications, conceptSet = codelistMedications)

# instantiate conditions
info(logger, "INSTANTIATE CONDITIONS - FRACTURE COHORT")
codelistConditions <- codesFromConceptSet(here("1_InstantiateCohorts", "Conditions"), cdm_char_frac)
cdm_char_frac <- generateConceptCohortSet(cdm = cdm_char_frac, name = conditions, conceptSet = codelistConditions, overwrite = T)

# create table summary
info(logger, "CREATE SUMMARY - FRACTURE COHORT")

cdm_char_frac[["drug_era"]] <- cdm_char_frac[["drug_era"]] %>% 
  dplyr::inner_join(cdm_char_frac[["concept"]] %>% dplyr::filter(concept_class_id == "Ingredient", standard_concept == "S"), by = c("drug_concept_id" = "concept_id")) %>%
  dplyr::select(colnames(cdm_char_frac[["drug_era"]])) %>%
  dplyr::filter(lubridate::year(.data$drug_era_start_date) >= 2009) %>%
  dplyr::compute()

cdm_char_frac[["visit_occurence"]] <- cdm_char_frac[["visit_occurrence"]] %>%  
  dplyr::filter(lubridate::year(.data$visit_start_date) >= 2009) %>%
  dplyr::compute()

result_frac <- cdm_char_frac[["fracture_cohort"]] %>%
  summariseCharacteristics(
    ageGroup = list(c(50, 54), c(55, 59), c(60, 64), c(65, 69), c(70, 74), c(75,79), c(80,84), c(85,89), c(90,94), c(95,99), c(100,150)),
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
info(logger, "CREATE SUMMARY, FRACTURE COHORT, IS DONE")

# export results
info(logger, "EXPORT RESULTS")
write_csv(result_imm, here(sub_output_folder, "table_one_imm.csv"))
write_csv(result_no_imm, here(sub_output_folder, "table_one_no_imm.csv"))
write_csv(result_frac, here(sub_output_folder, "table_one_frac.csv"))

rm(fracture_table_rq1, 
   imminentFractureCohortTotal, 
   reverseEntryTable, 
   stratifiedCohort, 
   withoutImminentFractureCohortTotal)

# reformat and export
reformatted_table_1 <- reformat_table_one(result_imm = result_imm, result_no_imm = result_no_imm, result_frac = result_frac)

rm(cdm_char_frac,
   cdm_char_imm,
   cdm_char_no_imm,
   result_frac,
   result_imm,
   result_no_imm,
   imminentFractureCohort,
   imminent_fracture_cohort_set,
   imminent_fracture_cohort_count,
   no_imminent_fracture_cohort_count,
   no_imminent_fracture_cohort_set,
   fracture_cohort_set,
   fracture_cohort_count)
