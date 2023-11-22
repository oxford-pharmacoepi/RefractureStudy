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
result_pip <- cdm_char[["refracture"]] %>%
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
write_csv(result_pip, here(output.folder, "table_one_parkinsonsIP", i, ".csv"))
