##########################
#############before
##########################
print(paste0("Creating before matching cohorts across all periods at ", Sys.time()))
cdm[["before_matching_ap"]] <- 
  Reduce(dplyr::union_all, targetCohort) %>%
  dplyr::select(subject_id, index_date) %>% 
  dplyr::rename(cohort_start_date = index_date) %>% 
  dplyr::mutate(cohort_end_date = cohort_start_date) %>% 
  dplyr::mutate(cohort_name = "target") %>%
  dplyr::distinct() %>% 
  dplyr::mutate(cohort_definition_id = 1) %>%
  dplyr::union_all(Reduce(dplyr::union_all, compCohort1) %>%
                     dplyr::select(subject_id, index_date) %>% 
                     dplyr::rename(cohort_start_date = index_date) %>% 
                     dplyr::mutate(cohort_end_date = cohort_start_date) %>% 
                     dplyr::mutate(cohort_name = "cohort1") %>%
                     dplyr::distinct() %>% 
                     dplyr::mutate(cohort_definition_id = 2)) %>%
  dplyr::union_all(Reduce(dplyr::union_all, compCohort2) %>%
                     dplyr::select(subject_id, index_date) %>% 
                     dplyr::rename(cohort_start_date = index_date) %>% 
                     dplyr::mutate(cohort_end_date = cohort_start_date) %>% 
                     dplyr::mutate(cohort_name = "cohort2") %>%
                     dplyr::distinct() %>% 
                     dplyr::mutate(cohort_definition_id = 3)) %>%
  computeQuery(
    name = "before_matching_ap", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

fullName <- inSchema(attr(cdm, "write_schema"), "before_matching_ap")
dbWriteTable(attr(cdm, "dbcon"), CDMConnector::inSchema(schema = attr(cdm, "write_schema"), table = "before_matching_ap"), cdm[["before_matching_ap"]], overwrite = T)
cdm[["before_matching_ap"]] <- dplyr::tbl(attr(cdm,"dbcon"), fullName)

cdm[["cohort_before_match_ap"]] <- cdm[["before_matching_ap"]] %>%
  dplyr::select(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date) %>%
  computeQuery(
    name = "cohort_before_match_ap",
    temporary = FALSE,
    schema = attr(cdm, "write_schema"),
    overwrite = TRUE
  )

cohort_before_match_ap_set <- cdm[["before_matching_ap"]] %>% 
  dplyr::select("cohort_definition_id", "cohort_name") %>% 
  dplyr::distinct() %>% 
  computeQuery(
    name = "cohort_before_match_ap_set", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

#cohort count
cohort_before_match_ap_count <- cdm[["before_matching_ap"]] %>%
  dplyr::group_by(cohort_definition_id) %>%
  dplyr::tally() %>%
  dplyr::compute() %>%
  dplyr::rename(number_records = n) %>%
  dplyr::mutate(number_subjects = number_records) %>%
  computeQuery(
    name = "cohort_before_match_ap_count", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

cdm_char <-CDMConnector::cdm_from_con(
  con = db,
  cdm_schema = cdm_database_schema,
  write_schema = c("schema" = results_database_schema, 
                   "prefix" = prefix)
)

cdm_char[["cohort_before_match_ap"]] <- newGeneratedCohortSet(cohortRef = cdm[["cohort_before_match_ap"]],
                                                         cohortSetRef = cohort_before_match_ap_set,
                                                         cohortCountRef = cohort_before_match_ap_count,
                                                         overwrite = T)

cdm_char <- CDMConnector::cdmSubsetCohort(cdm_char, "cohort_before_match_ap", verbose = T)

# instantiate medications
info(logger, "INSTANTIATE MEDICATIONS - BEFORE MATCHING")
print(paste0("Instantiating medication cohort at ", Sys.time()))
codelistMedications <- codesFromConceptSet(here("1_InstantiateCohorts", "Medications"), cdm_char)
cdm_char <- generateDrugUtilisationCohortSet(cdm = cdm_char, name = medications, conceptSet = codelistMedications)

# instantiate conditions
info(logger, "INSTANTIATE CONDITIONS - BEFORE MATCHING")
print(paste0("Instantiating condition cohort at ", Sys.time()))
codelistConditions <- codesFromConceptSet(here("1_InstantiateCohorts", "Conditions"), cdm_char)
cdm_char <- generateConceptCohortSet(cdm = cdm_char, name = conditions, conceptSet = codelistConditions, overwrite = T)

# create table summary
info(logger, "CREATE SUMMARY - BEFORE MATCHING ACROSS PERIODS")
print(paste0("Creating table one for before matching cohorts across all periods at ", Sys.time()))
result_before_matching_ap <- cdm_char[["cohort_before_match_ap"]] %>%
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
write_csv(result_before_matching_ap, here(t1_sub_output_folder, "result_before_matching_ap.csv"))

# #################### OST TABLE 1 ACROSS BEFORE ######################
# # instantiate conditions
# info(logger, "INSTANTIATE CONDITIONS - AFTER MATCHING 12")
# print(paste0("Instantiating OST conditions for before matching cohorts across all periods at ", Sys.time()))
# codelistConditionsOst <- codesFromConceptSet(here("1_InstantiateCohorts", "Conditions", "Osteoporosis"), cdm_char)
# cdm_char <- generateConceptCohortSet(cdm = cdm_char, name = conditions, conceptSet = codelistConditionsOst, overwrite = T)
# 
# # create table summary
# info(logger, "CREATE SUMMARY - AFTER MATCHING 12")
# print(paste0("Creating OST table 1 for before matching cohorts across all periods at ", Sys.time()))
# result_before_matching_apOST <- cdm_char[["cohort_before_match_ap"]] %>%
#   summariseCharacteristics(
#     cohortIntersect = list(
#       "Conditions" = list(
#         targetCohortTable = conditions, value = "flag", window = list(c(-Inf, -731), c(-730, -181), c(-180, 0))
#       )
#     )
#   )
# 
# result_before_matching_ap_window1 <- result_before_matching_apOST %>% 
#   dplyr::filter(variable_level == "80502") %>% 
#   dplyr::filter(stringr::str_detect(variable, '-180 to 0')) %>% 
#   dplyr::mutate(variable_level = "f80502_1")
# 
# result_before_matching_ap_window2 <- result_before_matching_apOST %>% 
#   dplyr::filter(variable_level == "80502") %>% 
#   dplyr::filter(stringr::str_detect(variable, '-730 to -181')) %>% 
#   dplyr::mutate(variable_level = "f80502_2")
# 
# result_before_matching_ap_window3 <- result_before_matching_apOST %>% 
#   dplyr::filter(variable_level == "80502") %>% 
#   dplyr::filter(stringr::str_detect(variable, '-731')) %>% 
#   dplyr::mutate(variable_level = "f80502_3")
# 
# result_before_matching_ap_v2 <- rbind(
#   result_before_matching_ap,
#   result_before_matching_ap_window1,
#   result_before_matching_ap_window2,
#   result_before_matching_ap_window3
# )
# write_csv(result_before_matching_ap_v2, here(t1_sub_output_folder, paste0("result_before_matching_ap_v2.csv")))

##########################
############# after 01
##########################
info(logger, "START TO CREATE THE SUMMARY - AFTER MATCHING 01")
print(paste0("Creating after matching cohorts T-C1 at ", Sys.time()))
subclasses01_table1 <- subclasses01
for (i in (1:length(subclasses01))){
  subclasses01_table1[[i]] <- subclasses01[[i]] %>%
    dplyr::select(subject_id, group, index_date) %>% 
    dplyr::mutate(period = i)
}

cdm[["after_matching_tc_ap"]] <- 
  Reduce(dplyr::union_all, subclasses01_table1) %>%
  dplyr::select(subject_id, index_date, group) %>% 
  dplyr::rename(cohort_start_date = index_date) %>% 
  dplyr::mutate(cohort_end_date = cohort_start_date) %>% 
  dplyr::filter(group == "target") %>% 
  dplyr::distinct() %>% 
  dplyr::mutate(cohort_definition_id = 1) %>%
  dplyr::rename("cohort_name" = "group") %>% 
  dplyr::mutate(cohort_name = as.character(cohort_name)) %>% 
  dplyr::select(subject_id, cohort_start_date, cohort_end_date, cohort_name, cohort_definition_id) %>% 
  dplyr::union_all(Reduce(dplyr::union_all, subclasses01_table1) %>%
                     dplyr::select(subject_id, index_date, group) %>% 
                     dplyr::rename(cohort_start_date = index_date) %>% 
                     dplyr::mutate(cohort_end_date = cohort_start_date) %>% 
                     dplyr::filter(group == "comparator 1") %>% 
                     dplyr::distinct() %>% 
                     dplyr::mutate(cohort_definition_id = 2) %>%
                     dplyr::rename("cohort_name" = "group") %>% 
                     dplyr::select(subject_id, cohort_start_date, cohort_end_date, cohort_name, cohort_definition_id)) %>% 
  computeQuery(
    name = "after_matching_tc_ap", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

fullName <- inSchema(attr(cdm, "write_schema"), "after_matching_tc_ap")
dbWriteTable(attr(cdm, "dbcon"), fullName, cdm[["after_matching_tc_ap"]] |> as_tibble(), overwrite = T)
cdm[["after_matching_tc_ap"]] <- dplyr::tbl(attr(cdm , "dbcon"), fullName)

cdm[["after_matching_01_cohort_ap"]] <- cdm[["after_matching_tc_ap"]] %>% 
  dplyr::select(-cohort_name) %>% 
  dplyr::select(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date) %>% 
  computeQuery(
    name = "after_matching_01_cohort_ap", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

after_matching_01_cohort_ap_set <- cdm[["after_matching_tc_ap"]] %>% 
  dplyr::select("cohort_definition_id", "cohort_name") %>% 
  dplyr::distinct() %>% 
  computeQuery(
    name = "after_matching_01_cohort_ap_set", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

#cohort count
after_matching_01_cohort_ap_count <- cdm[["after_matching_tc_ap"]] %>%
  dplyr::group_by(cohort_definition_id) %>%
  dplyr::tally() %>%
  dplyr::compute() %>%
  dplyr::rename(number_records = n) %>%
  dplyr::mutate(number_subjects = number_records) %>%
  computeQuery(
    name = "after_matching_01_cohort_ap_count", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

print(paste0("Creating table1 for after matching cohorts T-C1 across periods at ", Sys.time()))
cdm_char <-CDMConnector::cdm_from_con(
  con = db,
  cdm_schema = cdm_database_schema,
  write_schema = c("schema" = results_database_schema, 
                   "prefix" = prefix)
)

cdm_char[["after_matching_01_cohort_ap"]] <- newGeneratedCohortSet(cohortRef = cdm[["after_matching_01_cohort_ap"]],
                                                                 cohortSetRef = after_matching_01_cohort_ap_set,
                                                                 cohortCountRef = after_matching_01_cohort_ap_count,
                                                                 overwrite = T)

cdm_char <- CDMConnector::cdmSubsetCohort(cdm_char, "after_matching_01_cohort_ap", verbose = T)

# instantiate medications
info(logger, "INSTANTIATE MEDICATIONS - AFTER MATCHING ACROSS PERIDOS T-C1")
print(paste0("Instantiating medication cohort at ", Sys.time()))
codelistMedications <- codesFromConceptSet(here("1_InstantiateCohorts", "Medications"), cdm_char)
cdm_char <- generateDrugUtilisationCohortSet(cdm = cdm_char, name = medications, conceptSet = codelistMedications)

# instantiate conditions
info(logger, "INSTANTIATE CONDITIONS - AFTER MATCHING ACROSS ALL PERIODS")
print(paste0("Instantiating condition cohort at ", Sys.time()))
codelistConditions <- codesFromConceptSet(here("1_InstantiateCohorts", "Conditions"), cdm_char)
cdm_char <- generateConceptCohortSet(cdm = cdm_char, name = conditions, conceptSet = codelistConditions, overwrite = T)

# create table summary
info(logger, "CREATE SUMMARY - AFTER MATCHING")
print(paste0("Using PatientProfiles to create table 1 after matching across periods at ", Sys.time()))
result_after_matching01_ap <- cdm_char[["after_matching_01_cohort_ap"]] %>%
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
write_csv(result_after_matching01_ap, here(t1_sub_output_folder, "result_after_matching01_ap.csv"))

# #################### OST TABLE 1 ACROSS AFTER 01 ######################
# # instantiate conditions
# info(logger, "INSTANTIATE CONDITIONS - AFTER MATCHING 01")
# print(paste0("Instantiating conditions OST cohort at ", Sys.time()))
# codelistConditionsOst <- codesFromConceptSet(here("1_InstantiateCohorts", "Conditions", "Osteoporosis"), cdm_char)
# cdm_char <- generateConceptCohortSet(cdm = cdm_char, name = conditions, conceptSet = codelistConditionsOst, overwrite = T)
# 
# # create table summary
# info(logger, "CREATE SUMMARY - AFTER MATCHING 01")
# print(paste0("Using PatientProfiles to create OST table 1 after matching across periods at ", Sys.time()))
# result_after_matching01_ap_2 <- cdm_char[["after_matching_01_cohort_ap"]] %>%
#   summariseCharacteristics(
#     cohortIntersect = list(
#       "Conditions" = list(
#         targetCohortTable = conditions, value = "flag", window = list(c(-Inf, -731), c(-730, -181), c(-180, 0))
#       )
#     )
#   )
# 
# result_after_matching01_ap2_window1 <- result_after_matching01_ap_2 %>% 
#   dplyr::filter(variable_level == "80502") %>% 
#   dplyr::filter(stringr::str_detect(variable, '-180 to 0')) %>% 
#   dplyr::mutate(variable_level = "f80502_1")
# 
# result_after_matching01_ap2_window2 <- result_after_matching01_ap_2 %>% 
#   dplyr::filter(variable_level == "80502") %>% 
#   dplyr::filter(stringr::str_detect(variable, '-730 to -181')) %>% 
#   dplyr::mutate(variable_level = "f80502_2")
# 
# result_after_matching01_ap2_window3 <- result_after_matching01_ap_2 %>% 
#   dplyr::filter(variable_level == "80502") %>% 
#   dplyr::filter(stringr::str_detect(variable, '-731')) %>% 
#   dplyr::mutate(variable_level = "f80502_3")
# 
# result_after_matching01_ap2_v2 <- rbind(
#   result_after_matching01_ap_2,
#   result_after_matching01_ap2_window1,
#   result_after_matching01_ap2_window2,
#   result_after_matching01_ap2_window3
# )
# write_csv(result_after_matching01_ap2_v2, here(t1_sub_output_folder, paste0("result_after_matching01_ap2_v2.csv")))

#################################
##### after matching 12
#################################
info(logger, "START TO CREATE THE SUMMARY - AFTER MATCHING 12")
print(paste0("Creating after matching cohorts C1-C2 at ", Sys.time()))
subclasses12_table1 <- subclasses12
for (i in (1:length(subclasses12))){
  subclasses12_table1[[i]] <- subclasses12[[i]] %>%
    dplyr::select(subject_id, group, index_date) %>% 
    dplyr::mutate(period = i)
}

cdm[["after_matching_c1_c2_ap"]] <- 
  Reduce(dplyr::union_all, subclasses12_table1) %>%
  dplyr::select(subject_id, index_date, group) %>% 
  dplyr::rename(cohort_start_date = index_date) %>% 
  dplyr::mutate(cohort_end_date = cohort_start_date) %>% 
  dplyr::filter(group == "comparator 1") %>% 
  dplyr::distinct() %>% 
  dplyr::mutate(cohort_definition_id = 1) %>%
  dplyr::rename("cohort_name" = "group") %>% 
  dplyr::mutate(cohort_name = as.character(cohort_name)) %>% 
  dplyr::select(subject_id, cohort_start_date, cohort_end_date, cohort_name, cohort_definition_id) %>% 
  dplyr::union_all(Reduce(dplyr::union_all, subclasses12_table1) %>%
                     dplyr::select(subject_id, index_date, group) %>% 
                     dplyr::rename(cohort_start_date = index_date) %>% 
                     dplyr::mutate(cohort_end_date = cohort_start_date) %>% 
                     dplyr::filter(group == "comparator 2") %>% 
                     dplyr::distinct() %>% 
                     dplyr::mutate(cohort_definition_id = 2) %>%
                     dplyr::rename("cohort_name" = "group") %>% 
                     dplyr::select(subject_id, cohort_start_date, cohort_end_date, cohort_name, cohort_definition_id)) %>% 
  computeQuery(
    name = "after_matching_c1_c2_ap", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

fullName <- inSchema(attr(cdm, "write_schema"), "after_matching_c1_c2_ap")
dbWriteTable(attr(cdm, "dbcon"), fullName, cdm[["after_matching_c1_c2_ap"]] |> as_tibble(), overwrite = T)
cdm[["after_matching_c1_c2_ap"]] <- dplyr::tbl(attr(cdm , "dbcon"), fullName)

cdm[["after_matching_12_cohort_ap"]] <- cdm[["after_matching_c1_c2_ap"]] %>% 
  dplyr::select(-cohort_name) %>% 
  dplyr::select(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date) %>% 
  computeQuery(
    name = "after_matching_12_cohort_ap", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

after_matching_12_cohort_ap_set <- cdm[["after_matching_c1_c2_ap"]] %>% 
  dplyr::select("cohort_definition_id", "cohort_name") %>% 
  dplyr::distinct() %>% 
  computeQuery(
    name = "after_matching_12_cohort_ap_set", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

#cohort count
after_matching_12_cohort_ap_count <- cdm[["after_matching_c1_c2_ap"]] %>%
  dplyr::group_by(cohort_definition_id) %>%
  dplyr::tally() %>%
  dplyr::compute() %>%
  dplyr::rename(number_records = n) %>%
  dplyr::mutate(number_subjects = number_records) %>%
  computeQuery(
    name = "after_matching_12_cohort_ap_count", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

print(paste0("Creating table1 for after matching cohorts C1-C2 across periods at ", Sys.time()))
cdm_char <-CDMConnector::cdm_from_con(
  con = db,
  cdm_schema = cdm_database_schema,
  write_schema = c("schema" = results_database_schema, 
                   "prefix" = prefix)
)

cdm_char[["after_matching_12_cohort_ap"]] <- newGeneratedCohortSet(cohortRef = cdm[["after_matching_12_cohort_ap"]],
                                                                 cohortSetRef = after_matching_12_cohort_ap_set,
                                                                 cohortCountRef = after_matching_12_cohort_ap_count,
                                                                 overwrite = T)

cdm_char <- CDMConnector::cdmSubsetCohort(cdm_char, "after_matching_12_cohort_ap", verbose = T)

# instantiate medications
info(logger, "INSTANTIATE MEDICATIONS - AFTER MATCHING 12 ACROSS")
print(paste0("Instantiating medication cohort at ", Sys.time()))
codelistMedications <- codesFromConceptSet(here("1_InstantiateCohorts", "Medications"), cdm_char)
cdm_char <- generateDrugUtilisationCohortSet(cdm = cdm_char, name = medications, conceptSet = codelistMedications)

# instantiate conditions
info(logger, "INSTANTIATE CONDITIONS - AFTER MATCHING 12 ACROSS")
print(paste0("Instantiating condition cohort at ", Sys.time()))
codelistConditions <- codesFromConceptSet(here("1_InstantiateCohorts", "Conditions"), cdm_char)
cdm_char <- generateConceptCohortSet(cdm = cdm_char, name = conditions, conceptSet = codelistConditions, overwrite = T)

# create table summary
info(logger, "CREATE SUMMARY - BEFORE MATCHING")
result_after_matching12_ap <- cdm_char[["after_matching_12_cohort_ap"]] %>%
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
write_csv(result_after_matching12_ap, here(t1_sub_output_folder, "result_after_matching12_ap.csv"))

# #################### OST TABLE 1 ACROSS AFTER 12 ######################
# # instantiate conditions
# info(logger, "INSTANTIATE CONDITIONS - AFTER MATCHING 12 ACROSS PERIODS")
# print(paste0("Instantiating OST condition cohort at ", Sys.time()))
# codelistConditionsOst <- codesFromConceptSet(here("1_InstantiateCohorts", "Conditions", "Osteoporosis"), cdm_char)
# cdm_char <- generateConceptCohortSet(cdm = cdm_char, name = conditions, conceptSet = codelistConditionsOst, overwrite = T)
# 
# # create table summary
# info(logger, "CREATE SUMMARY - AFTER MATCHING 12")
# result_after_matching12_ap_2 <- cdm_char[["after_matching_12_cohort_ap"]] %>%
#   summariseCharacteristics(
#     cohortIntersect = list(
#       "Conditions" = list(
#         targetCohortTable = conditions, value = "flag", window = list(c(-Inf, -731), c(-730, -181), c(-180, 0))
#       )
#     )
#   )
# 
# result_after_matching12_ap_window1 <- result_after_matching12_ap_2 %>% 
#   dplyr::filter(variable_level == "80502") %>% 
#   dplyr::filter(stringr::str_detect(variable, '-180 to 0')) %>% 
#   dplyr::mutate(variable_level = "f80502_1")
# 
# result_after_matching12_ap_window2 <- result_after_matching12_ap_2 %>% 
#   dplyr::filter(variable_level == "80502") %>% 
#   dplyr::filter(stringr::str_detect(variable, '-730 to -181')) %>% 
#   dplyr::mutate(variable_level = "f80502_2")
# 
# result_after_matching12_ap_window3 <- result_after_matching12_ap_2 %>% 
#   dplyr::filter(variable_level == "80502") %>% 
#   dplyr::filter(stringr::str_detect(variable, '-731')) %>% 
#   dplyr::mutate(variable_level = "f80502_3")
# 
# result_after_matching12_ap_v2 <- rbind(
#   result_after_matching12_ap,
#   result_after_matching12_ap_window1,
#   result_after_matching12_ap_window2,
#   result_after_matching12_ap_window3
# )
# 
# write_csv(result_after_matching12_ap_v2, here(t1_sub_output_folder, paste0("result_after_matching12_ap_v2.csv")))

##### Compiling 
# output<-reformat_table_one_rq3_across(result_before_matching2_v2, name1 = "Target", name2 = "Cohort1") %>% 
#   dplyr::filter(!Characteristic %in% c("Fractures, n(%)", "Malignant neoplastic disease, n(%)"))
# write_csv(output, here(t1_sub_output_folder, paste0("t_c1_before_matching.csv")))
# 
# output<-reformat_table_one_rq3_across(result_before_matching2_v2, name1 = "Cohort1", name2 = "Cohort2") %>% 
#   dplyr::filter(!Characteristic %in% c("Fractures, n(%)", "Malignant neoplastic disease, n(%)"))
# write_csv(output, here(t1_sub_output_folder, paste0("c1_c2_before_matching.csv")))
# 
# output<-reformat_table_one_rq3_across(result_after_matching12_2_v2, name1 = "Target", name2 = "Comparator 1") %>% 
#   dplyr::filter(!Characteristic %in% c("Fractures, n(%)", "Malignant neoplastic disease, n(%)"))
# write_csv(output, here(t1_sub_output_folder, paste0("t_c1_after_matching.csv")))
# 
# output<-reformat_table_one_rq3_across(result_after_matching12_2_v2, name1 = "Comparator 1", name2 = "Comparator 2") %>% 
#   dplyr::filter(!Characteristic %in% c("Fractures, n(%)", "Malignant neoplastic disease, n(%)"))
# write_csv(output, here(t1_sub_output_folder, paste0("c1_c2_after_matching.csv")))
