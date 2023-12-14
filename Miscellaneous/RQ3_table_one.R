load(here::here(sub_output_folder, "tempData", "subclasses01.RData"))
load(here::here(sub_output_folder, "tempData", "subclasses12.RData"))
load(here::here(sub_output_folder, "tempData", "targetCohort.RData"))
load(here::here(sub_output_folder, "tempData", "compCohort1.RData"))
load(here::here(sub_output_folder, "tempData", "compCohort2.RData"))

tot_periods_target <- length(targetCohort)
tot_periods_c1 <- length(compCohort1)

info(logger, "START TO CREATE THE SUMMARY - BEFORE MATCHING")
cdm[["before_matching"]] <- 
  Reduce(dplyr::union_all, targetCohort) %>%
  dplyr::select(subject_id, index_date, period) %>% 
  dplyr::rename(cohort_start_date = index_date) %>% 
  dplyr::mutate(cohort_end_date = cohort_start_date) %>% 
  dplyr::mutate(cohort_name = "target") %>%
  dplyr::distinct() %>% 
  dplyr::mutate(cohort_definition_id = period) %>%
  dplyr::union_all(Reduce(dplyr::union_all, compCohort1) %>%
                     dplyr::select(subject_id, index_date, period) %>% 
                     dplyr::rename(cohort_start_date = index_date) %>% 
                     dplyr::mutate(cohort_end_date = cohort_start_date) %>% 
                     dplyr::mutate(cohort_name = "cohort1") %>%
                     dplyr::distinct() %>% 
                     dplyr::mutate(cohort_definition_id = tot_periods_target+period)) %>%
  dplyr::union_all(Reduce(dplyr::union_all, compCohort2) %>%
                     dplyr::select(subject_id, index_date, period) %>% 
                     dplyr::rename(cohort_start_date = index_date) %>% 
                     dplyr::mutate(cohort_end_date = cohort_start_date) %>% 
                     dplyr::mutate(cohort_name = "cohort2") %>%
                     dplyr::distinct() %>% 
                     dplyr::mutate(cohort_definition_id = tot_periods_target+tot_periods_c1+period)) %>%
  dplyr::mutate(cohort_name = paste0(period, "_", cohort_name)) %>% 
  dplyr::select(-period) %>% 
  computeQuery(
    name = "before_matching", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

fullName <- inSchema(attr(cdm, "write_schema"), "before_matching")
dbWriteTable(attr(cdm, "dbcon"), CDMConnector::inSchema(schema = attr(cdm, "write_schema"), table = "before_matching"), cdm[["before_matching"]], overwrite = T)
cdm[["before_matching"]] <- dplyr::tbl(attr(cdm,"dbcon"), fullName)

cdm[["table_one_cohort"]] <- cdm[["before_matching"]] %>%
  dplyr::select(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date) %>%
  computeQuery(
    name = "table_one_cohort",
    temporary = FALSE,
    schema = attr(cdm, "write_schema"),
    overwrite = TRUE
  )

table_one_cohort_set <- cdm[["before_matching"]] %>% 
  dplyr::select("cohort_definition_id", "cohort_name") %>% 
  dplyr::distinct() %>% 
  computeQuery(
    name = "table_one_cohort_set", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

#cohort count
table_one_cohort_count <- cdm[["before_matching"]] %>%
  dplyr::group_by(cohort_definition_id) %>%
  dplyr::tally() %>%
  dplyr::compute() %>%
  dplyr::rename(number_records = n) %>%
  dplyr::mutate(number_subjects = number_records) %>%
  computeQuery(
    name = "table_one_cohort_count", 
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

cdm_char[["table_one_cohort"]] <- newGeneratedCohortSet(cohortRef = cdm[["table_one_cohort"]],
                                                    cohortSetRef = table_one_cohort_set,
                                                    cohortCountRef = table_one_cohort_count,
                                                    overwrite = T)

cdm_char <- CDMConnector::cdmSubsetCohort(cdm_char, "table_one_cohort", verbose = T)

# instantiate medications
info(logger, "INSTANTIATE MEDICATIONS - BEFORE MATCHING")
codelistMedications <- codesFromConceptSet(here("1_InstantiateCohorts", "Medications"), cdm_char)
cdm_char <- generateDrugUtilisationCohortSet(cdm = cdm_char, name = medications, conceptSet = codelistMedications)

# instantiate conditions
info(logger, "INSTANTIATE CONDITIONS - BEFORE MATCHING")
codelistConditions <- codesFromConceptSet(here("1_InstantiateCohorts", "Conditions"), cdm_char)
cdm_char <- generateConceptCohortSet(cdm = cdm_char, name = conditions, conceptSet = codelistConditions, overwrite = T)

# create table summary
info(logger, "CREATE SUMMARY - BEFORE MATCHING")
result_before_matching <- cdm_char[["table_one_cohort"]] %>%
  summariseCharacteristics(
    ageGroup = list(c(50, 59), c(60, 69), c(70, 79), c(80, 89), c(90, 99), c(100,150)),
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
write_csv(result_before_matching, here(sub_output_folder, "result_before_matching.csv"))

################################ after matching #################################
##01
info(logger, "START TO CREATE THE SUMMARY - BEFORE MATCHING")
subclasses01_table1 <- subclasses01
for (i in (1:length(subclasses01))){
  subclasses01_table1[[i]] <- subclasses01[[i]] %>%
    dplyr::select(subject_id, group, index_date) %>% 
    dplyr::mutate(period = i)
}

cdm[["after_matching_tc"]] <- 
  Reduce(dplyr::union_all, subclasses01_table1) %>%
  dplyr::select(subject_id, index_date, group, period) %>% 
  dplyr::rename(cohort_start_date = index_date) %>% 
  dplyr::mutate(cohort_end_date = cohort_start_date) %>% 
  dplyr::filter(group == "target") %>% 
  dplyr::distinct() %>% 
  dplyr::mutate(cohort_definition_id = period) %>%
  dplyr::mutate(cohort_name = paste0(cohort_definition_id, "_", group)) %>% 
  dplyr::select(-group, -period) %>% 
  dplyr::select(subject_id, cohort_start_date, cohort_end_date, cohort_name, cohort_definition_id) %>% 
  dplyr::union_all(Reduce(dplyr::union_all, subclasses01_table1) %>%
                     dplyr::select(subject_id, index_date, group, period) %>% 
                     dplyr::rename(cohort_start_date = index_date) %>% 
                     dplyr::mutate(cohort_end_date = cohort_start_date) %>% 
                     dplyr::filter(group == "comparator 1") %>% 
                     dplyr::distinct() %>% 
                     dplyr::mutate(cohort_definition_id = tot_periods_target+period) %>%
                     dplyr::mutate(cohort_name = paste0(cohort_definition_id, "_", group)) %>% 
                     dplyr::select(-group, -period) %>% 
                     dplyr::select(subject_id, cohort_start_date, cohort_end_date, cohort_name, cohort_definition_id)) %>% 
  computeQuery(
    name = "after_matching_tc", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

fullName <- inSchema(attr(cdm, "write_schema"), "after_matching_tc")
dbWriteTable(attr(cdm, "dbcon"), fullName, cdm[["after_matching_tc"]] |> as_tibble(), overwrite = T)
cdm[["after_matching_tc"]] <- dplyr::tbl(attr(cdm , "dbcon"), fullName)

cdm[["after_matching_tc_cohort"]] <- cdm[["after_matching_tc"]] %>%
  dplyr::select(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date) %>%
  computeQuery(
    name = "after_matching_tc_cohort",
    temporary = FALSE,
    schema = attr(cdm, "write_schema"),
    overwrite = TRUE
  )

after_matching_tc_cohort_set <- cdm[["after_matching_tc"]] %>% 
  dplyr::select("cohort_definition_id", "cohort_name") %>% 
  dplyr::distinct() %>% 
  computeQuery(
    name = "after_matching_tc_cohort_set", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

#cohort count
after_matching_tc_cohort_count <- cdm[["after_matching_tc"]] %>%
  dplyr::group_by(cohort_definition_id) %>%
  dplyr::tally() %>%
  dplyr::compute() %>%
  dplyr::rename(number_records = n) %>%
  dplyr::mutate(number_subjects = number_records) %>%
  computeQuery(
    name = "after_matching_tc_cohort_count", 
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

cdm_char[["after_matching_tc"]] <- newGeneratedCohortSet(cohortRef = cdm[["after_matching_tc"]],
                                                        cohortSetRef = after_matching_tc_cohort_set,
                                                        cohortCountRef = after_matching_tc_cohort_count,
                                                        overwrite = T)

cdm_char <- CDMConnector::cdmSubsetCohort(cdm_char, "after_matching_tc", verbose = T)

# instantiate medications
info(logger, "INSTANTIATE MEDICATIONS - BEFORE MATCHING")
codelistMedications <- codesFromConceptSet(here("1_InstantiateCohorts", "Medications"), cdm_char)
cdm_char <- generateDrugUtilisationCohortSet(cdm = cdm_char, name = medications, conceptSet = codelistMedications)

# instantiate conditions
info(logger, "INSTANTIATE CONDITIONS - BEFORE MATCHING")
codelistConditions <- codesFromConceptSet(here("1_InstantiateCohorts", "Conditions"), cdm_char)
cdm_char <- generateConceptCohortSet(cdm = cdm_char, name = conditions, conceptSet = codelistConditions, overwrite = T)

# create table summary
info(logger, "CREATE SUMMARY - BEFORE MATCHING")
result_after_matching01 <- cdm_char[["after_matching_tc"]] %>%
  summariseCharacteristics(
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
write_csv(result_after_matching01, here(sub_output_folder, "result_after_matching01.csv"))

########################## after matching 12 ####################
subclasses12_table1 <- subclasses12
for (i in (1:length(subclasses12))){
  subclasses12_table1[[i]] <- subclasses12[[i]] %>%
    dplyr::select(subject_id, group, index_date) %>% 
    dplyr::mutate(period = i)
}
