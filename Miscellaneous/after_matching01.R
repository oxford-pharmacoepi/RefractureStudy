print(paste0("Creating after-matching 01 table ones at ", Sys.time()))
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
cdm_char <-CDMConnector::cdm_from_con(
  con = db,
  cdm_schema = cdm_database_schema,
  write_schema = c("schema" = results_database_schema, 
                   "prefix" = "ukfos"),
  cohort_tables = c(medications, conditions)
)

cdm_char[["after_matching_01_cohort"]] <- newGeneratedCohortSet(cohortRef = cdm[["after_matching_01_cohort"]],
                                                                cohortSetRef = after_matching_01_cohort_set,
                                                                cohortCountRef = after_matching_01_cohort_count,
                                                                overwrite = T)

cdm_char <- CDMConnector::cdmSubsetCohort(cdm_char, "after_matching_01_cohort", verbose = T)

# instantiate medications
info(logger, "INSTANTIATE MEDICATIONS - AFTER MATCHING")
print(paste0("Instantiating medications at ", Sys.time()))
codelistMedications <- codesFromConceptSet(here("1_InstantiateCohorts", "Medications"), cdm_char)
cdm_char <- generateDrugUtilisationCohortSet(cdm = cdm_char, name = medications, conceptSet = codelistMedications)

# instantiate conditions
info(logger, "INSTANTIATE CONDITIONS - AFTER MATCHING")
print(paste0("Instantiating conditions at ", Sys.time()))
codelistConditions <- codesFromConceptSet(here("1_InstantiateCohorts", "Conditions"), cdm_char)
cdm_char <- generateConceptCohortSet(cdm = cdm_char, name = conditions, conceptSet = codelistConditions, overwrite = T)

# create table summary
info(logger, "CREATE SUMMARY - AFTER MATCHING01")
print(paste0("Using PatientProfiles to create table one at ", Sys.time()))
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

c <- result_after_matching01

#create smd after matching t-c1
c <- c %>% 
  dplyr::mutate(group_level = gsub("comparator ", "cohort", group_level)) %>%
  dplyr::filter(!variable %in% c("Age group", "Sex", "Cohort start date", "Cohort end date", "Prior observation", "Future observation")) %>%
  dplyr::mutate(period = as.numeric(gsub( " .*$", "", group_level)),
                period = ifelse(period >16, period - 16, period),
                group = gsub(".*? ", "", group_level)) 

#Continuous variable
c1 <- c %>%
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
  dplyr::mutate(smd1 = (mean_target - mean_cohort1)/(sqrt((1/2)*((sd_target)^2+(sd_cohort1)^2))),
                asmd_t_c1 = round(abs(smd1), digits = 3))

#Binary variable
c2 <- c %>%
  dplyr::filter(estimate_type == "percentage") %>%
  dplyr::select(-group_level)%>%
  pivot_wider(
    names_from = group,
    values_from = estimate
  )%>%
  dplyr::mutate(x1 = as.numeric(target)/100,
                x2 = as.numeric(cohort1)/100,
                smd1 = (x1 - x2)/(sqrt((1/2)*(x1*(1-x1)+x2*(1-x2)))),
                asmd_t_c1 = round(abs(smd1), digits = 3))

c1 <- c1 %>% 
  dplyr::select(cdm_name, variable, variable_level, period, asmd_t_c1)
c2 <- c2 %>% 
  dplyr::select(cdm_name, variable, variable_level, period, asmd_t_c1)
smd_post_match_t_c1 <- rbind(c1,c2)

exclude <- c("Fractures", "Hiv", "Hormonal contraceptives syst", "Malignant neoplastic disease", "Antineoplastic agents")
imbal_t_c1 <- smd_post_match_t_c1 %>%
  dplyr::filter(!(variable_level %in% exclude))%>%
  dplyr::filter(variable!="Age") %>% #already age-matched and included in PS model
  dplyr::filter(asmd_t_c1 >0.1) %>%
  dplyr::select(variable_level)%>%
  unique()%>%
  mutate(variable_level = to_snake_case(variable_level))

#### glm
non_binary_var <- c("visit_occurrence", "drug_era")
imbal_t_c1_nb <- imbal_t_c1 %>% dplyr::filter(variable_level %in% non_binary_var) %>% dplyr::pull("variable_level")

if ("visit_occurrence" %in% imbal_t_c1_nb){
  cdm_char[["after_matching_01_cohort"]] <- cdm_char[["after_matching_01_cohort"]] |>
    PatientProfiles::addIntersect(
      tableName = "visit_occurrence",
      value = "count",
      window = list(c(-365, 0)),
      nameStyle = "number_visits_{window_name}"
    ) %>% 
    dplyr::compute()
} 

if ("drug_era" %in% imbal_t_c1_nb){
  cdm_char[["after_matching_01_cohort"]] <- cdm_char[["after_matching_01_cohort"]] |>
    PatientProfiles::addIntersect(
      tableName = "drug_era",
      value = "count",
      window = list(c(-365, 0)),
      nameStyle = "number_drug_era_{window_name}"
    ) %>% 
    dplyr::compute()
}

imbal_t_c1_nnb <- imbal_t_c1 %>% dplyr::filter(!variable_level %in% non_binary_var) %>% dplyr::pull("variable_level")
meds_glm_ids <- cohortSet(cdm_char[[medications]]) %>% 
  dplyr::collect() %>% 
  filter(cohort_name %in% imbal_t_c1_nnb) %>% 
  dplyr::select(cohort_definition_id) %>% 
  dplyr::pull(cohort_definition_id)

cdm_char[["after_matching_01_cohort"]] <- cdm_char[["after_matching_01_cohort"]] %>% 
  PatientProfiles::addCohortIntersectFlag(
    targetCohortTable = medications,
    targetCohortId = meds_glm_ids,
    window = list(c(-365, 0))
  ) %>% 
  dplyr::compute()

conditions_glm_ids <- cohortSet(cdm_char[[conditions]]) %>% 
  dplyr::collect() %>% 
  filter(cohort_name %in% imbal_t_c1_nnb) %>% 
  dplyr::select(cohort_definition_id) %>% 
  dplyr::pull(cohort_definition_id)

cdm_char[["after_matching_01_cohort"]] <- cdm_char[["after_matching_01_cohort"]] %>% 
  PatientProfiles::addCohortIntersectFlag(
    targetCohortTable = conditions,
    targetCohortId = conditions_glm_ids,
    window = list(c(-Inf, 0))
  ) %>% 
  dplyr::compute()

# #####################################################################
# #                                                                   #
# #                              OSTEOPOROSIS                         #
# #                                                                   #
# #####################################################################
# # instantiate conditions
# info(logger, "INSTANTIATE OST CONDITIONS - AFTER MATCHING 01")
# print(paste0("Instantiating ost conditions at ", Sys.time()))
# codelistConditionsOst <- codesFromConceptSet(here("1_InstantiateCohorts", "Conditions", "Osteoporosis"), cdm_char)
# cdm_char <- generateConceptCohortSet(cdm = cdm_char, name = conditions, conceptSet = codelistConditionsOst, overwrite = T)
# 
# # create table summary
# info(logger, "CREATE SUMMARY - AFTER MATCHING 01")
# print(paste0("Using PatientProfiles for OST table one at ", Sys.time()))
# result_after_matchingOST <- cdm_char[["after_matching_01_cohort"]] %>%
#   summariseCharacteristics(
#     cohortIntersect = list(
#       "Conditions" = list(
#         targetCohortTable = conditions, value = "flag", window = list(c(-Inf, -731), c(-730, -181), c(-180, 0))
#       )
#     )
#   )
# 
# result_after_matching01_window1 <- result_after_matchingOST %>% 
#   dplyr::filter(variable_level == "80502") %>% 
#   dplyr::filter(stringr::str_detect(variable, '-180 to 0')) %>% 
#   dplyr::mutate(variable_level = "f80502_1")
# 
# result_after_matching01_window2 <- result_after_matchingOST %>% 
#   dplyr::filter(variable_level == "80502") %>% 
#   dplyr::filter(stringr::str_detect(variable, '-730 to -181')) %>% 
#   dplyr::mutate(variable_level = "f80502_2")
# 
# result_after_matching01_window3 <- result_after_matchingOST %>% 
#   dplyr::filter(variable_level == "80502") %>% 
#   dplyr::filter(stringr::str_detect(variable, '-731')) %>% 
#   dplyr::mutate(variable_level = "f80502_3")
# 
# result_after_matching01_v2 <- rbind(
#   result_after_matching01,
#   result_after_matching01_window1,
#   result_after_matching01_window2,
#   result_after_matching01_window3
# )
# write_csv(result_after_matching01_v2, here(t1_sub_output_folder, "result_after_matching01_v2.csv"))
# 
# # print(paste0("Nicer Table1 for before matching at ", Sys.time()))
# # 
# # for (i in (1:tot_periods_target)){
# #   output<-reformat_table_one_rq3_01(result_after_matching01_v2, period = i, name1 = "target", name2 = "comparator 1") %>% 
# #     dplyr::filter(!Characteristic %in% c("Fractures, n(%)", "Malignant neoplastic disease, n(%)"))
# #   write_csv(output, here(t1_sub_output_folder, paste0("target_c1_", i, "_after_matching.csv")))
# # }

suppressWarnings(
  rm(fullName,
     after_matching_01_cohort_set,
     after_matching_01_cohort_count,
     imbal_t_c1,
     imbal_t_c1_nb,
     imbal_t_c1_nnb,
     meds_glm,
     conditions_glm
     )
)