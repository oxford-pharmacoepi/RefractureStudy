if (country_setting == "UK"){
  secondary_output <- here(output_folder, washout_period[[k]], "secondary")
  if (!dir.exists(secondary_output)) {
    dir.create(secondary_output)
  }
  
  cdm[["visit_occurrence_hes"]] <- cdm[["visit_occurrence"]] %>% 
    dplyr::filter(visit_concept_id == 9201) %>% 
    CDMConnector::computeQuery(
      name = "visit_occurrence_hes", 
      temporary = FALSE, 
      schema = attr(cdm, "write_schema"), 
      overwrite = TRUE
    )
  
  cdm[["visit_detail_inpatient"]] <- cdm[["visit_detail"]] %>%
    dplyr::filter(visit_detail_concept_id == 9201) %>%
    CDMConnector::computeQuery(
      name = "visit_detail_inpatient",
      temporary = FALSE,
      schema = attr(cdm, "write_schema"),
      overwrite = TRUE
    )
  
  cdm[["visit_detail_intensive_care"]] <- cdm[["visit_detail"]] %>%
    dplyr::filter(visit_detail_concept_id == 32037) %>%
    CDMConnector::computeQuery(
      name = "visit_detail_intensive_care",
      temporary = FALSE,
      schema = attr(cdm, "write_schema"),
      overwrite = TRUE
    )
  
  cdm[["visit_detail_hes"]] <- cdm[["visit_detail"]] %>%
    dplyr::filter(visit_detail_concept_id == 32037|visit_detail_concept_id == 9201) %>%
    CDMConnector::computeQuery(
      name = "visit_detail_hes",
      temporary = FALSE,
      schema = attr(cdm, "write_schema"),
      overwrite = TRUE
    )
  
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
  write.xlsx(target_procedure, file = here(secondary_output, "target_procedure.xlsx"))
  write.xlsx(c1_comp1_procedure, file = here(secondary_output, "c1_comp1_procedure.xlsx"))
  write.xlsx(c1_comp2_procedure, file = here(secondary_output, "c1_comp2_procedure.xlsx"))
  write.xlsx(c2_procedure, file = here(secondary_output, "c2_procedure.xlsx"))
  
  target_procedure_primary <- procedure_frequency_table(cohort_freq = target_matched, primary = T)
  c1_comp1_procedure_primary <- procedure_frequency_table(cohort_freq = cohort1_matched_to, primary = T)
  c1_comp2_procedure_primary <- procedure_frequency_table(cohort_freq = cohort1_matched_from, primary = T)
  c2_procedure_primary <- procedure_frequency_table(cohort_freq = cohort2_matched, primary = T)
  write.xlsx(target_procedure_primary, file = here(secondary_output, "target_procedure_primary.xlsx"))
  write.xlsx(c1_comp1_procedure_primary, file = here(secondary_output, "c1_comp1_procedure_primary.xlsx"))
  write.xlsx(c1_comp2_procedure_primary, file = here(secondary_output, "c1_comp2_procedure_primary.xlsx"))
  write.xlsx(c2_procedure_primary, file = here(secondary_output, "c2_procedure_primary.xlsx"))
  
  ## matched condition
  target_condition <- condition_frequency_table(cohort_freq = target_matched)
  c1_comp1_condition <- condition_frequency_table(cohort_freq = cohort1_matched_to)
  c1_comp2_condition <- condition_frequency_table(cohort_freq = cohort1_matched_from)
  c2_condition <- condition_frequency_table(cohort_freq = cohort2_matched)
  write.xlsx(target_condition, file = here(secondary_output, "target_condition.xlsx"))
  write.xlsx(c1_comp1_condition, file = here(secondary_output, "c1_comp1_condition.xlsx"))
  write.xlsx(c1_comp2_condition, file = here(secondary_output, "c1_comp2_condition.xlsx"))
  write.xlsx(c2_condition, file = here(secondary_output, "c2_condition.xlsx"))
  
  target_condition_primary <- condition_frequency_table(cohort_freq = target_matched, primary = T)
  c1_comp1_condition_primary <- condition_frequency_table(cohort_freq = cohort1_matched_to, primary = T)
  c1_comp2_condition_primary <- condition_frequency_table(cohort_freq = cohort1_matched_from, primary = T)
  c2_condition_primary <- condition_frequency_table(cohort_freq = cohort2_matched, primary = T)
  write.xlsx(target_condition_primary, file = here(secondary_output, "target_condition_primary.xlsx"))
  write.xlsx(c1_comp1_condition_primary, file = here(secondary_output, "c1_comp1_condition_primary.xlsx"))
  write.xlsx(c1_comp2_condition_primary, file = here(secondary_output, "c1_comp2_condition_primary.xlsx"))
  write.xlsx(c2_condition_primary, file = here(secondary_output, "c2_condition_primary.xlsx"))
  
  ## matched visit_occurrence
  target_visit <- visit_summary(cohort_freq = target_matched)
  c1_comp1_visit <- visit_summary(cohort_freq = cohort1_matched_to)
  c1_comp2_visit <- visit_summary(cohort_freq = cohort1_matched_from)
  c2_visit <- visit_summary(cohort_freq = cohort2_matched)
  write.xlsx(target_visit, file = here(secondary_output, "target_visit.xlsx"))
  write.xlsx(c1_comp1_visit, file = here(secondary_output, "c1_comp1_visit.xlsx"))
  write.xlsx(c1_comp2_visit, file = here(secondary_output, "c1_comp2_visit.xlsx"))
  write.xlsx(c2_visit, file = here(secondary_output, "c2_visit.xlsx"))
  
} else if(country_setting == "Spain"){
  
  secondary_output <- here(output_folder, washout_period[[k]], "secondary")
  
  if (!dir.exists(secondary_output)) {
    dir.create(secondary_output)
  }
  
  cdm[["visit_detail_hes"]] <- cdm[["visit_detail"]] %>%
    dplyr::filter(visit_detail_concept_id == 32037|visit_detail_concept_id == 9201) %>%
    CDMConnector::computeQuery(
      name = "visit_detail_hes",
      temporary = FALSE,
      schema = attr(cdm, "write_schema"),
      overwrite = TRUE
    )
  
  cdm[["visit_detail_inpatient"]] <- cdm[["visit_detail"]] %>%
    dplyr::filter(visit_detail_concept_id == 9201) %>%
    CDMConnector::computeQuery(
      name = "visit_detail_inpatient",
      temporary = FALSE,
      schema = attr(cdm, "write_schema"),
      overwrite = TRUE
    )
  
  cdm[["visit_detail_intensive_care"]] <- cdm[["visit_detail"]] %>%
    dplyr::filter(visit_detail_concept_id == 32037) %>%
    CDMConnector::computeQuery(
      name = "visit_detail_intensive_care",
      temporary = FALSE,
      schema = attr(cdm, "write_schema"),
      overwrite = TRUE
    )
}