print(paste0("Creating before-matching table ones at ", Sys.time()))

info(logger, "START TO CREATE THE SUMMARY - BEFORE MATCHING")
print(paste0("Creating before matching cohorts at ", Sys.time()))

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

info(logger, "START TO CREATE THE SUMMARY - BEFORE MATCHING")
cdm_char <-CDMConnector::cdm_from_con(
  con = db,
  cdm_schema = cdm_database_schema,
  write_schema = c("schema" = results_database_schema, 
                   "prefix" = "sebfo")
)

cdm_char[["table_one_cohort"]] <- newGeneratedCohortSet(cohortRef = cdm[["table_one_cohort"]],
                                                        cohortSetRef = table_one_cohort_set,
                                                        cohortCountRef = table_one_cohort_count,
                                                        overwrite = T)
# cdm_char <-CDMConnector::cdm_from_con(
#   con = db,
#   cdm_schema = cdm_database_schema,
#   write_schema = c("schema" = results_database_schema, 
#                    "prefix" = "gskhd")
# )

cdm_char <- CDMConnector::cdmSubsetCohort(cdm_char, "table_one_cohort", verbose = T)

# instantiate medications
info(logger, "INSTANTIATE MEDICATIONS - BEFORE MATCHING")
print(paste0("Instantiating medication cohorts for before matching cohorts at ", Sys.time()))
codelistMedications <- codesFromConceptSet(here("1_InstantiateCohorts", "Medications"), cdm_char)
cdm_char <- generateDrugUtilisationCohortSet(cdm = cdm_char, name = medications, conceptSet = codelistMedications)

# instantiate conditions
info(logger, "INSTANTIATE CONDITIONS - BEFORE MATCHING")
print(paste0("Instantiating condition cohorts for before matching cohorts at ", Sys.time()))
codelistConditions <- codesFromConceptSet(here("1_InstantiateCohorts", "Conditions"), cdm_char)
cdm_char <- generateConceptCohortSet(cdm = cdm_char, name = conditions, conceptSet = codelistConditions, overwrite = T)

# create table summary
info(logger, "CREATE SUMMARY - BEFORE MATCHING")
print(paste0("Using PatientProfiles to create table one at ", Sys.time()))

# cdm_char[["table_one_cohort"]] %>%
#   addTableIntersectCount(
#     "visit_occurrence", window = c(-365, 0)
#   ) %>% 
#   addAge() %>% 
#   addPriorObservation() %>% 
#   dplyr::compute()
  
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

a <- result_before_matching

#create smd before matching
#unequal number of rows per period caused by small numbers in age group 100-150#
a <- a %>%
  dplyr::filter(!variable %in% c("Age group", "Sex", "Cohort start date", "Cohort end date", "Prior observation", "Future observation")) %>%
  dplyr::mutate(period = as.numeric(gsub( " .*$", "", group_level)),
                period = ifelse(period >16, period - 16, period),
                group = gsub(".*? ", "", group_level)) 

#Continuous variable
a1 <- a %>%
  dplyr::filter(estimate_type %in% c("mean", "sd")) %>%
  dplyr::mutate(estimate = as.numeric(estimate)) %>%
  pivot_wider(
    names_from = estimate_type,
    values_from = estimate
  )%>%
  dplyr::select(-group_level)%>%
  pivot_wider(
    names_from = group,
    values_from = c(mean, sd)
  )%>%
  dplyr::mutate(smd1 = (mean_cohort1 - mean_cohort2)/(sqrt((1/2)*((sd_cohort1)^2+(sd_cohort2)^2))),
                smd2 = (mean_target - mean_cohort1)/(sqrt((1/2)*((sd_target)^2+(sd_cohort1)^2))),
                asmd_c1_c2 = round(abs(smd1), digits = 3),
                asmd_t_c1 = round(abs(smd2), digits = 3))



#Binary variable
a2 <- a %>%
  dplyr::filter(estimate_type == "percentage") %>%
  dplyr::select(-group_level)%>%
  pivot_wider(
    names_from = group,
    values_from = estimate
  )%>%
  dplyr::mutate(x1 = as.numeric(cohort1)/100,
                x2 = as.numeric(cohort2)/100,
                smd1 = (x1 - x2)/(sqrt((1/2)*(x1*(1-x1)+x2*(1-x2)))),
                asmd_c1_c2 = round(abs(smd1), digits = 3),
                x3 = as.numeric(target)/100,
                smd2 = (x3 - x1)/(sqrt((1/2)*(x3*(1-x3)+x1*(1-x1)))),
                asmd_t_c1 = round(abs(smd2), digits = 3))

a1 <- a1 %>% 
  dplyr::select(cdm_name, variable, variable_level, period, asmd_c1_c2, asmd_t_c1)
a2 <- a2 %>% 
  dplyr::select(cdm_name, variable, variable_level, period, asmd_c1_c2, asmd_t_c1)
smd_pre_match <- rbind(a1,a2)

#####################################################################
#                                                                   #
#                              OSTEOPOROSIS                         #
#                                                                   #
#####################################################################
# # instantiate conditions
# info(logger, "INSTANTIATE OST CONDITIONS - BEFORE MATCHING")
# print(paste0("Instantiating osteopororsis condition cohorts for before matching cohorts at ", Sys.time()))
# codelistConditionsOst <- codesFromConceptSet(here("1_InstantiateCohorts", "Conditions", "Osteoporosis"), cdm_char)
# cdm_char <- generateConceptCohortSet(cdm = cdm_char, name = conditions, conceptSet = codelistConditionsOst, overwrite = T)
# 
# # create table summary
# info(logger, "CREATE SUMMARY OST - BEFORE MATCHING")
# print(paste0("Using PatientProfiles to create table one for osteoporosis at ", Sys.time()))
# result_before_matchingOST <- cdm_char[["table_one_cohort"]] %>%
#   summariseCharacteristics(
#     cohortIntersect = list(
#       "Conditions" = list(
#         targetCohortTable = conditions, value = "flag", window = list(c(-Inf, -731), c(-730, -181), c(-180, 0))
#       )
#     )
#   )
# 
# result_before_matching2_window1 <- result_before_matchingOST %>% 
#   dplyr::filter(variable_level == "80502") %>% 
#   dplyr::filter(stringr::str_detect(variable, '-180 to 0')) %>% 
#   dplyr::mutate(variable_level = "f80502_1")
# 
# result_before_matching2_window2 <- result_before_matchingOST %>% 
#   dplyr::filter(variable_level == "80502") %>% 
#   dplyr::filter(stringr::str_detect(variable, '-730 to -181')) %>% 
#   dplyr::mutate(variable_level = "f80502_2")
# 
# result_before_matching2_window3 <- result_before_matchingOST %>% 
#   dplyr::filter(variable_level == "80502") %>% 
#   dplyr::filter(stringr::str_detect(variable, '-731')) %>% 
#   dplyr::mutate(variable_level = "f80502_3")
# 
# result_before_matching_v2 <- rbind(
#   result_before_matching,
#   result_before_matching2_window1,
#   result_before_matching2_window2,
#   result_before_matching2_window3
# )
# write_csv(result_before_matching_v2, here(t1_sub_output_folder, "result_before_matching_v2.csv"))

# 
# # print(paste0("Nicer Table1 for before matching at ", Sys.time()))
# # for (i in (1:tot_periods_target)){
# #   output<-reformat_table_one_rq3(result_before_matching_v2, period = i, name1 = "target", name2 = "comparator 1", j = 1, k = 2) %>% 
# #     dplyr::filter(!Characteristic %in% c("Fractures, n(%)", "Malignant neoplastic disease, n(%)"))
# #   write_csv(output, here(t1_sub_output_folder, paste0("target_c1_", i, "_before_matching.csv")))
# # }
# 
# # for (i in (1:tot_periods_c1)){
# #   output<-reformat_table_one_rq3(result_before_matching_v2, period = i, name1 = "comparator 1", name2 = "comparator 2", j = 2, k = 3) %>% 
# #     dplyr::filter(!Characteristic %in% c("Fractures, n(%)", "Malignant neoplastic disease, n(%)"))
# #   write_csv(output, here(t1_sub_output_folder, paste0("c1_c2_", i, "_before_matching.csv")))
# # }

# print(paste0("Nicer Table1 for before matching at ", Sys.time()))
# for (i in (1:tot_periods_target)){
#   output<-reformat_table_one_rq3(result_before_matching_v2, period = i, name1 = "target", name2 = "comparator 1", j = 1, k = 2) %>% 
#     dplyr::filter(!Characteristic %in% c("Fractures, n(%)", "Malignant neoplastic disease, n(%)"))
#   write_csv(output, here(t1_sub_output_folder, paste0("target_c1_", i, "_before_matching.csv")))
# }

# for (i in (1:tot_periods_c1)){
#   output<-reformat_table_one_rq3(result_before_matching_v2, period = i, name1 = "comparator 1", name2 = "comparator 2", j = 2, k = 3) %>% 
#     dplyr::filter(!Characteristic %in% c("Fractures, n(%)", "Malignant neoplastic disease, n(%)"))
#   write_csv(output, here(t1_sub_output_folder, paste0("c1_c2_", i, "_before_matching.csv")))
# }

suppressWarnings(
  rm(fullName,
     table_one_cohort_set,
     table_one_cohort_count)
  )
