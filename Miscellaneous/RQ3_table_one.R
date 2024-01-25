########################## LOADING NECESSARY DATA #############################
load(here::here(sub_output_folder, "tempData", "subclasses01.RData"))
load(here::here(sub_output_folder, "tempData", "subclasses12.RData"))
load(here::here(sub_output_folder, "tempData", "targetCohort.RData"))
load(here::here(sub_output_folder, "tempData", "compCohort1.RData"))
load(here::here(sub_output_folder, "tempData", "compCohort2.RData"))

t1_sub_output_folder <- here(sub_output_folder, "tableones_rq3")
if (!dir.exists(t1_sub_output_folder)) {
  dir.create(t1_sub_output_folder)
}

tot_periods_target <- length(targetCohort)
tot_periods_c1 <- length(compCohort1)

info(logger, "START TO CREATE THE SUMMARY - BEFORE MATCHING")
print(paste0("Creating before matching cohorts at ", Sys.time()))

############################## CDM CREATE COHORTS - BEFORE MATCHING ###############################
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

###################### OVERALL TABLE 1 BEFORE MATCHING ##############################
print(paste0("Creating table1 for before matching cohorts at ", Sys.time()))
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
write_csv(result_before_matching, here(t1_sub_output_folder, "result_before_matching.csv"))

####################### OVERALL TABLE 1 BEFORE MATCHING OSTEO ONLY ################
# instantiate conditions
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

info(logger, "INSTANTIATE CONDITIONS - BEFORE MATCHING")
codelistConditionsOst <- codesFromConceptSet(here("1_InstantiateCohorts", "Conditions", "Osteoporosis"), cdm_char)
cdm_char <- generateConceptCohortSet(cdm = cdm_char, name = conditions, conceptSet = codelistConditionsOst, overwrite = T)

# create table summary
info(logger, "CREATE SUMMARY - BEFORE MATCHING")
result_before_matching2 <- cdm_char[["table_one_cohort"]] %>%
  summariseCharacteristics(
    cohortIntersect = list(
      "Conditions" = list(
        targetCohortTable = conditions, value = "flag", window = list(c(-Inf, -731), c(-730, -181), c(-180, 0))
      )
    )
  )

result_before_matching2_window1 <- result_before_matching2 %>% 
  dplyr::filter(variable_level == "80502") %>% 
  dplyr::filter(stringr::str_detect(variable, '-180 to 0')) %>% 
  dplyr::mutate(variable_level = "f80502_1")

result_before_matching2_window2 <- result_before_matching2 %>% 
  dplyr::filter(variable_level == "80502") %>% 
  dplyr::filter(stringr::str_detect(variable, '-730 to -181')) %>% 
  dplyr::mutate(variable_level = "f80502_2")

result_before_matching2_window3 <- result_before_matching2 %>% 
  dplyr::filter(variable_level == "80502") %>% 
  dplyr::filter(stringr::str_detect(variable, '-731')) %>% 
  dplyr::mutate(variable_level = "f80502_3")

result_before_matching_v2 <- rbind(
  result_before_matching,
  result_before_matching2_window1,
  result_before_matching2_window2,
  result_before_matching2_window3
)

for (i in (1:tot_periods_target)){
  output<-reformat_table_one_rq3(result_before_matching_v2, period = i, name1 = "target", name2 = "comparator 1", j = 1, k = 2) %>% 
    dplyr::filter(!Characteristic %in% c("Fractures, n(%)", "Malignant neoplastic disease, n(%)"))
  write_csv(output, here(t1_sub_output_folder, paste0("target_c1_", i, "_before_matching.csv")))
}

for (i in (1:tot_periods_c1)){
  output<-reformat_table_one_rq3(result_before_matching_v2, period = i, name1 = "comparator 1", name2 = "comparator 2", j = 2, k = 3) %>% 
    dplyr::filter(!Characteristic %in% c("Fractures, n(%)", "Malignant neoplastic disease, n(%)"))
  write_csv(output, here(t1_sub_output_folder, paste0("c1_c2_", i, "_before_matching.csv")))
}

################################ after matching 01 #################################
##01
info(logger, "START TO CREATE THE SUMMARY - AFTER MATCHING 01")
print(paste0("Creating after matching cohorts T-C1 at ", Sys.time()))
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

cdm[["after_matching_01_cohort"]] <- cdm[["after_matching_tc"]] %>% 
  dplyr::select(-cohort_name) %>% 
  dplyr::select(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date) %>% 
  computeQuery(
    name = "after_matching_01_cohort", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

after_matching_01_cohort_set <- cdm[["after_matching_tc"]] %>% 
  dplyr::select("cohort_definition_id", "cohort_name") %>% 
  dplyr::distinct() %>% 
  computeQuery(
    name = "after_matching_01_cohort_set", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

#cohort count
after_matching_01_cohort_count <- cdm[["after_matching_tc"]] %>%
  dplyr::group_by(cohort_definition_id) %>%
  dplyr::tally() %>%
  dplyr::compute() %>%
  dplyr::rename(number_records = n) %>%
  dplyr::mutate(number_subjects = number_records) %>%
  computeQuery(
    name = "after_matching_01_cohort_count", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

########################## AFTER MATCHING 01 OVERALL TABLE 1####################
print(paste0("Creating table1 for after matching cohorts T-C1 at ", Sys.time()))
stem_table <- "rq3"
conditions <- paste0(stem_table, "_conditions")
medications <- paste0(stem_table, "_medications")
cdm_char <-CDMConnector::cdm_from_con(
  con = db,
  cdm_schema = cdm_database_schema,
  write_schema = results_database_schema
)

cdm_char[["after_matching_01_cohort"]] <- newGeneratedCohortSet(cohortRef = cdm[["after_matching_01_cohort"]],
                                                        cohortSetRef = after_matching_01_cohort_set,
                                                        cohortCountRef = after_matching_01_cohort_count,
                                                        overwrite = T)

cdm_char <- CDMConnector::cdmSubsetCohort(cdm_char, "after_matching_01_cohort", verbose = T)

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
result_after_matching01 <- cdm_char[["after_matching_01_cohort"]] %>%
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
write_csv(result_after_matching01, here(t1_sub_output_folder, "result_after_matching01.csv"))

############################### OST TABLE 1 01###############################
# instantiate conditions
stem_table <- "rq3"
conditions <- paste0(stem_table, "_conditions")
medications <- paste0(stem_table, "_medications")
cdm_char <-CDMConnector::cdm_from_con(
  con = db,
  cdm_schema = cdm_database_schema,
  write_schema = results_database_schema
)

cdm_char[["after_matching_01_cohort"]] <- newGeneratedCohortSet(cohortRef = cdm[["after_matching_01_cohort"]],
                                                                 cohortSetRef = after_matching_01_cohort_set,
                                                                 cohortCountRef = after_matching_01_cohort_count,
                                                                 overwrite = T)

cdm_char <- CDMConnector::cdmSubsetCohort(cdm_char, "after_matching_01_cohort", verbose = T)

info(logger, "INSTANTIATE CONDITIONS - BEFORE MATCHING")
codelistConditionsOst <- codesFromConceptSet(here("1_InstantiateCohorts", "Conditions", "Osteoporosis"), cdm_char)
cdm_char <- generateConceptCohortSet(cdm = cdm_char, name = conditions, conceptSet = codelistConditionsOst, overwrite = T)

# create table summary
info(logger, "CREATE SUMMARY - BEFORE MATCHING")
result_after_matching01_2 <- cdm_char[["after_matching_01_cohort"]] %>%
  summariseCharacteristics(
    cohortIntersect = list(
      "Conditions" = list(
        targetCohortTable = conditions, value = "flag", window = list(c(-Inf, -731), c(-730, -181), c(-180, 0))
      )
    )
  )

result_after_matching01_window1 <- result_after_matching01_2 %>% 
  dplyr::filter(variable_level == "80502") %>% 
  dplyr::filter(stringr::str_detect(variable, '-180 to 0')) %>% 
  dplyr::mutate(variable_level = "f80502_1")

result_after_matching01_window2 <- result_after_matching01_2 %>% 
  dplyr::filter(variable_level == "80502") %>% 
  dplyr::filter(stringr::str_detect(variable, '-730 to -181')) %>% 
  dplyr::mutate(variable_level = "f80502_2")

result_after_matching01_window3 <- result_after_matching01_2 %>% 
  dplyr::filter(variable_level == "80502") %>% 
  dplyr::filter(stringr::str_detect(variable, '-731')) %>% 
  dplyr::mutate(variable_level = "f80502_3")

result_before_matching_v2 <- rbind(
  result_before_matching,
  result_before_matching2_window1,
  result_before_matching2_window2,
  result_before_matching2_window3
)

for (i in (1:tot_periods_target)){
  output<-reformat_table_one_rq3(result_before_matching_v2, period = i, name1 = "target", name2 = "comparator 1", j = 1, k = 2) %>% 
    dplyr::filter(!Characteristic %in% c("Fractures, n(%)", "Malignant neoplastic disease, n(%)"))
  write_csv(output, here(t1_sub_output_folder, paste0("target_c1_", i, "_before_matching.csv")))
}

for (i in (1:tot_periods_c1)){
  output<-reformat_table_one_rq3(result_before_matching_v2, period = i, name1 = "comparator 1", name2 = "comparator 2", j = 2, k = 3) %>% 
    dplyr::filter(!Characteristic %in% c("Fractures, n(%)", "Malignant neoplastic disease, n(%)"))
  write_csv(output, here(t1_sub_output_folder, paste0("c1_c2_", i, "_before_matching.csv")))
}

for (i in (1:tot_periods_target)){
  output<-reformat_table_one_rq3_01(result_after_matching01, period = i, name1 = "target", name2 = "comparator 1") %>% 
    dplyr::filter(!Characteristic %in% c("Fractures, n(%)", "Malignant neoplastic disease, n(%)"))
  write_csv(output, here(t1_sub_output_folder, paste0("target_c1_", i, "_after_matching.csv")))
}

########################## after matching 12 ####################
##12
info(logger, "START TO CREATE THE SUMMARY - AFTER MATCHING 12")
print(paste0("Creating after matching cohorts C1-C2 at ", Sys.time()))
subclasses12_table1 <- subclasses12
for (i in (1:length(subclasses12))){
  subclasses12_table1[[i]] <- subclasses12[[i]] %>%
    dplyr::select(subject_id, group, index_date) %>% 
    dplyr::mutate(period = i)
}

cdm[["after_matching_c1c2"]] <- 
  Reduce(dplyr::union_all, subclasses12_table1) %>%
  dplyr::select(subject_id, index_date, group, period) %>% 
  dplyr::rename(cohort_start_date = index_date) %>% 
  dplyr::mutate(cohort_end_date = cohort_start_date) %>% 
  dplyr::filter(group == "comparator 1") %>% 
  dplyr::distinct() %>% 
  dplyr::mutate(cohort_definition_id = period) %>%
  dplyr::mutate(cohort_name = paste0(cohort_definition_id, "_", group)) %>% 
  dplyr::select(-group, -period) %>% 
  dplyr::select(subject_id, cohort_start_date, cohort_end_date, cohort_name, cohort_definition_id) %>% 
  dplyr::union_all(Reduce(dplyr::union_all, subclasses12_table1) %>%
                     dplyr::select(subject_id, index_date, group, period) %>% 
                     dplyr::rename(cohort_start_date = index_date) %>% 
                     dplyr::mutate(cohort_end_date = cohort_start_date) %>% 
                     dplyr::filter(group == "comparator 2") %>% 
                     dplyr::distinct() %>% 
                     dplyr::mutate(cohort_definition_id = tot_periods_target+period) %>%
                     dplyr::mutate(cohort_name = paste0(cohort_definition_id, "_", group)) %>% 
                     dplyr::select(-group, -period) %>% 
                     dplyr::select(subject_id, cohort_start_date, cohort_end_date, cohort_name, cohort_definition_id)) %>% 
  computeQuery(
    name = "after_matching_c1c2", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

fullName <- inSchema(attr(cdm, "write_schema"), "after_matching_c1c2")
dbWriteTable(attr(cdm, "dbcon"), fullName, cdm[["after_matching_c1c2"]] |> as_tibble(), overwrite = T)
cdm[["after_matching_c1c2"]] <- dplyr::tbl(attr(cdm , "dbcon"), fullName)

cdm[["after_matching_12_cohort"]] <- cdm[["after_matching_c1c2"]] %>% 
  dplyr::select(-cohort_name) %>% 
  dplyr::select(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date) %>% 
  computeQuery(
    name = "after_matching_12_cohort", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

after_matching_12_cohort_set <- cdm[["after_matching_c1c2"]] %>% 
  dplyr::select("cohort_definition_id", "cohort_name") %>% 
  dplyr::distinct() %>% 
  computeQuery(
    name = "after_matching_12_cohort_set", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

#cohort count
after_matching_12_cohort_count <- cdm[["after_matching_c1c2"]] %>%
  dplyr::group_by(cohort_definition_id) %>%
  dplyr::tally() %>%
  dplyr::compute() %>%
  dplyr::rename(number_records = n) %>%
  dplyr::mutate(number_subjects = number_records) %>%
  computeQuery(
    name = "after_matching_12_cohort_count", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

print(paste0("Creating table1 for after matching cohorts C1-C2 at ", Sys.time()))
stem_table <- "rq3"
conditions <- paste0(stem_table, "_conditions")
medications <- paste0(stem_table, "_medications")
cdm_char <-CDMConnector::cdm_from_con(
  con = db,
  cdm_schema = cdm_database_schema,
  write_schema = results_database_schema
)

cdm_char[["after_matching_12_cohort"]] <- newGeneratedCohortSet(cohortRef = cdm[["after_matching_12_cohort"]],
                                                                 cohortSetRef = after_matching_12_cohort_set,
                                                                 cohortCountRef = after_matching_12_cohort_count,
                                                                 overwrite = T)

cdm_char <- CDMConnector::cdmSubsetCohort(cdm_char, "after_matching_12_cohort", verbose = T)

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
result_after_matching12 <- cdm_char[["after_matching_12_cohort"]] %>%
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
write_csv(result_after_matching12, here(t1_sub_output_folder, "result_after_matching12.csv"))

for (i in (1:tot_periods_target)){
  output<-reformat_table_one_rq3_12(result_after_matching12, period = i, name1 = "comparator 1", name2 = "comparator 2") %>% 
    dplyr::filter(!Characteristic %in% c("Fractures, n(%)", "Malignant neoplastic disease, n(%)"))
  write_csv(output, here(t1_sub_output_folder, paste0("c1_c2_", i, "_after_matching.csv")))
}

###### across periods
cdm[["before_matching2"]] <- 
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
    name = "before_matching2", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

fullName <- inSchema(attr(cdm, "write_schema"), "before_matching2")
dbWriteTable(attr(cdm, "dbcon"), CDMConnector::inSchema(schema = attr(cdm, "write_schema"), table = "before_matching2"), cdm[["before_matching2"]], overwrite = T)
cdm[["before_matching2"]] <- dplyr::tbl(attr(cdm,"dbcon"), fullName)

cdm[["table_one_cohort2"]] <- cdm[["before_matching2"]] %>%
  dplyr::select(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date) %>%
  computeQuery(
    name = "table_one_cohort2",
    temporary = FALSE,
    schema = attr(cdm, "write_schema"),
    overwrite = TRUE
  )

table_one_cohort2_set <- cdm[["before_matching2"]] %>% 
  dplyr::select("cohort_definition_id", "cohort_name") %>% 
  dplyr::distinct() %>% 
  computeQuery(
    name = "table_one_cohort2_set", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

#cohort count
table_one_cohort2_count <- cdm[["before_matching2"]] %>%
  dplyr::group_by(cohort_definition_id) %>%
  dplyr::tally() %>%
  dplyr::compute() %>%
  dplyr::rename(number_records = n) %>%
  dplyr::mutate(number_subjects = number_records) %>%
  computeQuery(
    name = "table_one_cohort2_count", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

cdm_char <-CDMConnector::cdm_from_con(
  con = db,
  cdm_schema = cdm_database_schema,
  write_schema = results_database_schema
)

cdm_char[["table_one_cohort2"]] <- newGeneratedCohortSet(cohortRef = cdm[["table_one_cohort2"]],
                                                        cohortSetRef = table_one_cohort2_set,
                                                        cohortCountRef = table_one_cohort2_count,
                                                        overwrite = T)

cdm_char <- CDMConnector::cdmSubsetCohort(cdm_char, "table_one_cohort2", verbose = T)

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
result_before_matching2 <- cdm_char[["table_one_cohort2"]] %>%
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
write_csv(result_before_matching2, here(t1_sub_output_folder, "result_before_matching2.csv"))

################################ after matching #################################
##01
info(logger, "START TO CREATE THE SUMMARY - AFTER MATCHING 01")
print(paste0("Creating after matching cohorts T-C1 at ", Sys.time()))
subclasses01_table1 <- subclasses01
for (i in (1:length(subclasses01))){
  subclasses01_table1[[i]] <- subclasses01[[i]] %>%
    dplyr::select(subject_id, group, index_date) %>% 
    dplyr::mutate(period = i)
}

cdm[["after_matching_tc2"]] <- 
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
    name = "after_matching_tc2", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

fullName <- inSchema(attr(cdm, "write_schema"), "after_matching_tc2")
dbWriteTable(attr(cdm, "dbcon"), fullName, cdm[["after_matching_tc2"]] |> as_tibble(), overwrite = T)
cdm[["after_matching_tc2"]] <- dplyr::tbl(attr(cdm , "dbcon"), fullName)

cdm[["after_matching_01_cohort2"]] <- cdm[["after_matching_tc2"]] %>% 
  dplyr::select(-cohort_name) %>% 
  dplyr::select(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date) %>% 
  computeQuery(
    name = "after_matching_01_cohort2", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

after_matching_01_cohort2_set <- cdm[["after_matching_tc2"]] %>% 
  dplyr::select("cohort_definition_id", "cohort_name") %>% 
  dplyr::distinct() %>% 
  computeQuery(
    name = "after_matching_01_cohort2_set", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

#cohort count
after_matching_01_cohort2_count <- cdm[["after_matching_tc2"]] %>%
  dplyr::group_by(cohort_definition_id) %>%
  dplyr::tally() %>%
  dplyr::compute() %>%
  dplyr::rename(number_records = n) %>%
  dplyr::mutate(number_subjects = number_records) %>%
  computeQuery(
    name = "after_matching_01_cohort2_count", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

print(paste0("Creating table1 for after matching cohorts T-C1 at ", Sys.time()))
stem_table <- "rq3"
conditions <- paste0(stem_table, "_conditions")
medications <- paste0(stem_table, "_medications")
cdm_char <-CDMConnector::cdm_from_con(
  con = db,
  cdm_schema = cdm_database_schema,
  write_schema = results_database_schema
)

cdm_char[["after_matching_01_cohort2"]] <- newGeneratedCohortSet(cohortRef = cdm[["after_matching_01_cohort2"]],
                                                                 cohortSetRef = after_matching_01_cohort2_set,
                                                                 cohortCountRef = after_matching_01_cohort2_count,
                                                                 overwrite = T)

cdm_char <- CDMConnector::cdmSubsetCohort(cdm_char, "after_matching_01_cohort2", verbose = T)

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
result_after_matching01 <- cdm_char[["after_matching_01_cohort2"]] %>%
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
write_csv(result_after_matching01_2, here(t1_sub_output_folder, "result_after_matching01_2.csv"))

####################################################################################
##12
info(logger, "START TO CREATE THE SUMMARY - AFTER MATCHING 12")
print(paste0("Creating after matching cohorts C1-C2 at ", Sys.time()))
subclasses12_table1 <- subclasses12
for (i in (1:length(subclasses12))){
  subclasses12_table1[[i]] <- subclasses12[[i]] %>%
    dplyr::select(subject_id, group, index_date) %>% 
    dplyr::mutate(period = i)
}

cdm[["after_matching_tc2"]] <- 
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
    name = "after_matching_tc2", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

fullName <- inSchema(attr(cdm, "write_schema"), "after_matching_tc2")
dbWriteTable(attr(cdm, "dbcon"), fullName, cdm[["after_matching_tc2"]] |> as_tibble(), overwrite = T)
cdm[["after_matching_tc2"]] <- dplyr::tbl(attr(cdm , "dbcon"), fullName)

cdm[["after_matching_12_cohort2"]] <- cdm[["after_matching_tc2"]] %>% 
  dplyr::select(-cohort_name) %>% 
  dplyr::select(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date) %>% 
  computeQuery(
    name = "after_matching_12_cohort2", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

after_matching_12_cohort2_set <- cdm[["after_matching_tc2"]] %>% 
  dplyr::select("cohort_definition_id", "cohort_name") %>% 
  dplyr::distinct() %>% 
  computeQuery(
    name = "after_matching_12_cohort2_set", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

#cohort count
after_matching_12_cohort2_count <- cdm[["after_matching_tc2"]] %>%
  dplyr::group_by(cohort_definition_id) %>%
  dplyr::tally() %>%
  dplyr::compute() %>%
  dplyr::rename(number_records = n) %>%
  dplyr::mutate(number_subjects = number_records) %>%
  computeQuery(
    name = "after_matching_12_cohort2_count", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

print(paste0("Creating table1 for after matching cohorts C1-C2 at ", Sys.time()))
stem_table <- "rq3"
conditions <- paste0(stem_table, "_conditions")
medications <- paste0(stem_table, "_medications")
cdm_char <-CDMConnector::cdm_from_con(
  con = db,
  cdm_schema = cdm_database_schema,
  write_schema = results_database_schema
)

cdm_char[["after_matching_12_cohort2"]] <- newGeneratedCohortSet(cohortRef = cdm[["after_matching_12_cohort2"]],
                                                                  cohortSetRef = after_matching_12_cohort2_set,
                                                                  cohortCountRef = after_matching_12_cohort2_count,
                                                                  overwrite = T)

cdm_char <- CDMConnector::cdmSubsetCohort(cdm_char, "after_matching_12_cohort2", verbose = T)

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
result_after_matching12_2 <- cdm_char[["after_matching_12_cohort2"]] %>%
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
write_csv(result_after_matching12_2, here(t1_sub_output_folder, "result_after_matching12_2.csv"))

##### Compiling 
output<-reformat_table_one_rq3_across(result_before_matching2, name1 = "Target", name2 = "Cohort1") %>% 
  dplyr::filter(!Characteristic %in% c("Fractures, n(%)", "Malignant neoplastic disease, n(%)"))
write_csv(output, here(t1_sub_output_folder, paste0("t_c1_before_matching.csv")))

output<-reformat_table_one_rq3_across(result_before_matching2, name1 = "Cohort1", name2 = "Cohort2") %>% 
  dplyr::filter(!Characteristic %in% c("Fractures, n(%)", "Malignant neoplastic disease, n(%)"))
write_csv(output, here(t1_sub_output_folder, paste0("c1_c2_before_matching.csv")))

output<-reformat_table_one_rq3_across(result_after_matching01_2, name1 = "Target", name2 = "Comparator 1") %>% 
  dplyr::filter(!Characteristic %in% c("Fractures, n(%)", "Malignant neoplastic disease, n(%)"))
write_csv(output, here(t1_sub_output_folder, paste0("t_c1_after_matching.csv")))

output<-reformat_table_one_rq3_across(result_after_matching12_2, name1 = "Comparator 1", name2 = "Comparator 2") %>% 
  dplyr::filter(!Characteristic %in% c("Fractures, n(%)", "Malignant neoplastic disease, n(%)"))
write_csv(output, here(t1_sub_output_folder, paste0("c1_c2_after_matching.csv")))
