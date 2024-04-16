print(paste0("Creating after-matching 12 table ones at ", Sys.time()))
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

########################## AFTER MATCHING 12 OVERALL TABLE 1####################
print(paste0("Creating table1 for after matching cohorts C1-C2 at ", Sys.time()))
cdm_char12 <-CDMConnector::cdm_from_con(
  con = db,
  cdm_schema = cdm_database_schema,
  write_schema = c("schema" = results_database_schema, 
                   "prefix" = prefix)
)

cdm_char12[["after_matching_12_cohort"]] <- newGeneratedCohortSet(cohortRef = cdm[["after_matching_12_cohort"]],
                                                                cohortSetRef = after_matching_12_cohort_set,
                                                                cohortCountRef = after_matching_12_cohort_count,
                                                                overwrite = T)

cdm_char12 <- CDMConnector::cdmSubsetCohort(cdm_char12, "after_matching_12_cohort", verbose = T)

# instantiate medications
info(logger, "INSTANTIATE MEDICATIONS - AFTER MATCHING 12")
print(paste0("Instantiating medications at ", Sys.time()))
codelistMedications <- codesFromConceptSet(here("1_InstantiateCohorts", "Medications"), cdm_char12)
cdm_char12 <- generateDrugUtilisationCohortSet(cdm = cdm_char12, name = medications, conceptSet = codelistMedications)

# instantiate conditions
info(logger, "INSTANTIATE CONDITIONS - AFTER MATCHING 12")
print(paste0("Instantiating conditions at ", Sys.time()))
codelistConditions <- codesFromConceptSet(here("1_InstantiateCohorts", "Conditions"), cdm_char12)
cdm_char12 <- generateConceptCohortSet(cdm = cdm_char12, name = conditions, conceptSet = codelistConditions, overwrite = T)

# create table summary
info(logger, "CREATE SUMMARY - AFTER MATCHING 12")
print(paste0("Using PatientProfiles to create table one at ", Sys.time()))
result_after_matching12 <- cdm_char12[["after_matching_12_cohort"]] %>%
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

b <- result_after_matching12

#create smd after matching c1-c2
b <- b %>% 
  dplyr::mutate(group_level = gsub("comparator ", "cohort", group_level)) %>%
  dplyr::filter(!variable %in% c("Age group", "Sex", "Cohort start date", "Cohort end date", "Prior observation", "Future observation")) %>%
  dplyr::mutate(period = as.numeric(gsub( " .*$", "", group_level)),
                period = ifelse(period >16, period - 16, period),
                group = gsub(".*? ", "", group_level)) 

#Continuous variable
b1 <- b %>%
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
                asmd_c1_c2 = round(abs(smd1), digits = 3))

#Binary variable
b2 <- b %>%
  dplyr::filter(estimate_type == "percentage") %>%
  dplyr::select(-group_level)%>%
  pivot_wider(
    names_from = group,
    values_from = estimate
  )%>%
  dplyr::mutate(x1 = as.numeric(cohort1)/100,
                x2 = as.numeric(cohort2)/100,
                smd1 = (x1 - x2)/(sqrt((1/2)*(x1*(1-x1)+x2*(1-x2)))),
                asmd_c1_c2 = round(abs(smd1), digits = 3))

b1 <- b1 %>% 
  dplyr::select(cdm_name, variable, variable_level, period, asmd_c1_c2)
b2 <- b2 %>% 
  dplyr::select(cdm_name, variable, variable_level, period, asmd_c1_c2)
smd_post_match_c1_c2 <- rbind(b1,b2)

exclude <- c("Fractures", "Hiv", "Hormonal contraceptives syst", "Malignant neoplastic disease", "Antineoplastic agents")
imbal_c1_c2 <- smd_post_match_c1_c2 %>%
  dplyr::filter(!(variable_level %in% exclude))%>%
  dplyr::filter(variable!="Age") %>% #already age-matched and included in PS model
  dplyr::filter(asmd_c1_c2 >0.1) %>%
  dplyr::select(variable_level)%>%
  unique()%>%
  mutate(variable_level = to_snake_case(variable_level))
if (country_setting %in% c("Spain", "UK")){
  write_csv(imbal_c1_c2, here(t1_sub_output_folder, "imbalanced_covs_12.csv"))
}

#### glm
imbal_c1_c2_nb <- imbal_c1_c2 %>% dplyr::filter(variable_level %in% non_binary_var) %>% dplyr::pull("variable_level")

if ("visit_occurrence" %in% imbal_c1_c2_nb){
  cdm_char12[["after_matching_12_cohort"]] <- cdm_char12[["after_matching_12_cohort"]] |>
    PatientProfiles::addIntersect(
      tableName = "visit_occurrence",
      value = "count",
      window = list(c(-365, 0)),
      nameStyle = "number_visits_{window_name}"
    ) %>% 
    dplyr::compute()
} 

if ("drug_era" %in% imbal_c1_c2_nb){
  cdm_char12[["after_matching_12_cohort"]] <- cdm_char12[["after_matching_12_cohort"]] |>
    PatientProfiles::addIntersect(
      tableName = "drug_era",
      value = "count",
      window = list(c(-365, 0)),
      nameStyle = "number_drug_era_{window_name}"
    ) %>% 
    dplyr::compute()
}

imbal_c1_c2_nnb <- imbal_c1_c2 %>% dplyr::filter(!variable_level %in% non_binary_var) %>% dplyr::pull("variable_level")
meds_glm_ids <- cohortSet(cdm_char12[[medications]]) %>% 
  dplyr::collect() %>% 
  filter(cohort_name %in% imbal_c1_c2_nnb) %>% 
  dplyr::select(cohort_definition_id) %>% 
  dplyr::pull(cohort_definition_id)

cdm_char12[["after_matching_12_cohort"]] <- cdm_char12[["after_matching_12_cohort"]] %>% 
  PatientProfiles::addCohortIntersectFlag(
    targetCohortTable = medications,
    targetCohortId = meds_glm_ids,
    window = list(c(-365, 0))
  ) %>% 
  dplyr::compute()

conditions_glm_ids <- cohortSet(cdm_char12[[conditions]]) %>% 
  dplyr::collect() %>% 
  filter(cohort_name %in% imbal_c1_c2_nnb) %>% 
  dplyr::select(cohort_definition_id) %>% 
  dplyr::pull(cohort_definition_id)

cdm_char12[["after_matching_12_cohort"]] <- cdm_char12[["after_matching_12_cohort"]] %>% 
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
# info(logger, "INSTANTIATE OST CONDITIONS - AFTER MATCHING 12")
# print(paste0("Instantiating ost conditions at ", Sys.time()))
# codelistConditionsOst <- codesFromConceptSet(here("1_InstantiateCohorts", "Conditions", "Osteoporosis"), cdm_char12)
# cdm_char12 <- generateConceptCohortSet(cdm = cdm_char12, name = conditions, conceptSet = codelistConditionsOst, overwrite = T)
# 
# # create table summary
# info(logger, "CREATE SUMMARY - AFTER MATCHING 12")
# print(paste0("Using PatientProfiles for OST table one at ", Sys.time()))
# result_after_matching12OST <- cdm_char12[["after_matching_12_cohort"]] %>%
#   summariseCharacteristics(
#     cohortIntersect = list(
#       "Conditions" = list(
#         targetCohortTable = conditions, value = "flag", window = list(c(-Inf, -731), c(-730, -181), c(-180, 0))
#       )
#     )
#   )
# 
# result_after_matching12_window1 <- result_after_matching12OST %>% 
#   dplyr::filter(variable_level == "80502") %>% 
#   dplyr::filter(stringr::str_detect(variable, '-180 to 0')) %>% 
#   dplyr::mutate(variable_level = "f80502_1")
# 
# result_after_matching12_window2 <- result_after_matching12OST %>% 
#   dplyr::filter(variable_level == "80502") %>% 
#   dplyr::filter(stringr::str_detect(variable, '-730 to -181')) %>% 
#   dplyr::mutate(variable_level = "f80502_2")
# 
# result_after_matching12_window3 <- result_after_matching12OST %>% 
#   dplyr::filter(variable_level == "80502") %>% 
#   dplyr::filter(stringr::str_detect(variable, '-731')) %>% 
#   dplyr::mutate(variable_level = "f80502_3")
# 
# result_after_matching12_v2 <- rbind(
#   result_after_matching12,
#   result_after_matching12_window1,
#   result_after_matching12_window2,
#   result_after_matching12_window3
# )
# write_csv(result_after_matching12_v2, here(t1_sub_output_folder, "result_after_matching12_v2.csv"))
# 
# #print(paste0("Nicer Table1 for before matching at ", Sys.time()))
# 
# # for (i in (1:tot_periods_target)){
# #   output<-reformat_table_one_rq3_12(result_after_matching12_v2, period = i, name1 = "comparator 1", name2 = "comparator 2") %>% 
# #     dplyr::filter(!Characteristic %in% c("Fractures, n(%)", "Malignant neoplastic disease, n(%)"))
# #   write_csv(output, here(t1_sub_output_folder, paste0("c1_c2_", i, "_after_matching.csv")))
# # }

suppressWarnings(
  rm(fullName,
     after_matching_12_cohort_set,
     after_matching_12_cohort_count)
)
