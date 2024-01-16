cdm[["visit_occurrence_hes"]] <- cdm[["visit_occurrence"]] %>% 
  dplyr::filter(visit_concept_id == 9201) %>% 
  CDMConnector::computeQuery(
    name = "visit_occurrence_hes", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

# cdm[["visit_detail_inpatient"]] <- cdm[["visit_detail"]] %>% 
#   dplyr::filter(visit_detail_concept_id == 9201) %>% 
#   CDMConnector::computeQuery(
#     name = "visit_detail_inpatient", 
#     temporary = FALSE, 
#     schema = attr(cdm, "write_schema"), 
#     overwrite = TRUE
#   )
# 
# cdm[["visit_detail_intensive_care"]] <- cdm[["visit_detail"]] %>% 
#   dplyr::filter(visit_detail_concept_id == 32037) %>% 
#   CDMConnector::computeQuery(
#     name = "visit_detail_intensive_care", 
#     temporary = FALSE, 
#     schema = attr(cdm, "write_schema"), 
#     overwrite = TRUE
#   )

cdm[["condition_occurrence_hes"]] <- cdm[["condition_occurrence"]] %>% 
  dplyr::filter(condition_type_concept_id == 32829) %>% 
  CDMConnector::computeQuery(
    name = "condition_occurrence_hes", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

cdm[["procedure_occurrence_hes"]] <- cdm[["procedure_occurrence"]] %>% 
  dplyr::filter(procedure_type_concept_id == 32829) %>% 
  CDMConnector::computeQuery(
    name = "procedure_occurrence_hes", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

load(here(sub_output_folder, "tempData", "targetCohort.RData"))
load(here(sub_output_folder, "tempData", "compCohort1.RData"))
load(here(sub_output_folder, "tempData", "compCohort2.RData"))

## matched procedure
target_procedure <- procedure_frequency_table(cohort_freq = target_matched)
c1_comp1_procedure <- procedure_frequency_table(cohort_freq = cohort1_matched_to)
c1_comp2_procedure <- procedure_frequency_table(cohort_freq = cohort1_matched_from)
c2_procedure <- procedure_frequency_table(cohort_freq = cohort2_matched)
write.xlsx(target_procedure, file = here(sub_output_folder, "target_procedure.xlsx"))
write.xlsx(c1_comp1_procedure, file = here(sub_output_folder, "c1_comp1_procedure.xlsx"))
write.xlsx(c1_comp2_procedure, file = here(sub_output_folder, "c1_comp2_procedure.xlsx"))
write.xlsx(c2_procedure, file = here(sub_output_folder, "c2_procedure.xlsx"))

## matched condition
target_condition <- condition_frequency_table(cohort_freq = target_matched)
c1_comp1_condition <- condition_frequency_table(cohort_freq = cohort1_matched_to)
c1_comp2_condition <- condition_frequency_table(cohort_freq = cohort1_matched_from)
c2_condition <- condition_frequency_table(cohort_freq = cohort2_matched)
write.xlsx(target_condition, file = here(sub_output_folder, "target_condition.xlsx"))
write.xlsx(c1_comp1_condition, file = here(sub_output_folder, "c1_comp1_condition.xlsx"))
write.xlsx(c1_comp2_condition, file = here(sub_output_folder, "c1_comp2_condition.xlsx"))
write.xlsx(c2_condition, file = here(sub_output_folder, "c2_condition.xlsx"))

## matched visit_occurrence
target_visit_occurrence <- visit_occurrence_summary(cohort_freq = target_matched)
c1_comp1_visit_occurrence <- visit_occurrence_summary(cohort_freq = cohort1_matched_to)
c1_comp2_visit_occurrence <- visit_occurrence_summary(cohort_freq = cohort1_matched_from)
c2_visit_occurrence <- visit_occurrence_summary(cohort_freq = cohort2_matched)
write.xlsx(target_visit_occurrence, file = here(sub_output_folder, "target_visit_occurrence.xlsx"))
write.xlsx(c1_comp1_visit_occurrence, file = here(sub_output_folder, "c1_comp1_visit_occurrence.xlsx"))
write.xlsx(c1_comp2_visit_occurrence, file = here(sub_output_folder, "c1_comp2_visit_occurrence.xlsx"))
write.xlsx(c2_visit_occurrence, file = here(sub_output_folder, "c2_visit_occurrence.xlsx"))
