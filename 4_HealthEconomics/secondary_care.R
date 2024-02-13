if (country_setting == "UK"){
  secondary_output <- here(output_folder, washout_period[[k]], "secondary")
  secondary_output_ic <- here(output_folder, washout_period[[k]], "secondary_ic")
  secondary_output_inpatient <- here(output_folder, washout_period[[k]], "secondary_inpatient")
  
  if (!dir.exists(secondary_output)) {
    dir.create(secondary_output)
  }
  
  if (!dir.exists(secondary_output_ic)) {
    dir.create(secondary_output_ic)
  }
  
  if (!dir.exists(secondary_output_inpatient)) {
    dir.create(secondary_output_inpatient)
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
  
  cdm[["visit_detail_ic"]] <- cdm[["visit_detail"]] %>%
    dplyr::filter(visit_detail_concept_id == 32037) %>%
    CDMConnector::computeQuery(
      name = "visit_detail_ic",
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
  
  ## matched condition - ALL
  target_condition <- condition_frequency_table(cohort_freq = target_matched, table_name = "visit_detail_hes")
  c1_comp1_condition <- condition_frequency_table(cohort_freq = cohort1_matched_to, table_name = "visit_detail_hes")
  c1_comp2_condition <- condition_frequency_table(cohort_freq = cohort1_matched_from, table_name = "visit_detail_hes")
  c2_condition <- condition_frequency_table(cohort_freq = cohort2_matched, table_name = "visit_detail_hes")
  write.xlsx(target_condition, file = here(secondary_output, "target_condition_all.xlsx"))
  write.xlsx(c1_comp1_condition, file = here(secondary_output, "c1_comp1_condition_all.xlsx"))
  write.xlsx(c1_comp2_condition, file = here(secondary_output, "c1_comp2_condition_all.xlsx"))
  write.xlsx(c2_condition, file = here(secondary_output, "c2_condition_all.xlsx"))
  
  target_condition_primary <- condition_frequency_table(cohort_freq = target_matched, table_name = "visit_detail_hes", primary = T)
  c1_comp1_condition_primary <- condition_frequency_table(cohort_freq = cohort1_matched_to, table_name = "visit_detail_hes", primary = T)
  c1_comp2_condition_primary <- condition_frequency_table(cohort_freq = cohort1_matched_from, table_name = "visit_detail_hes", primary = T)
  c2_condition_primary <- condition_frequency_table(cohort_freq = cohort2_matched, table_name = "visit_detail_hes", primary = T)
  write.xlsx(target_condition_primary, file = here(secondary_output, "target_condition_primary_all.xlsx"))
  write.xlsx(c1_comp1_condition_primary, file = here(secondary_output, "c1_comp1_condition_primary_all.xlsx"))
  write.xlsx(c1_comp2_condition_primary, file = here(secondary_output, "c1_comp2_condition_primary_all.xlsx"))
  write.xlsx(c2_condition_primary, file = here(secondary_output, "c2_condition_primary_all.xlsx"))
  
  ## matched condition - IC
  target_condition_ic <- condition_frequency_table(cohort_freq = target_matched, table_name = "visit_detail_ic")
  c1_comp1_condition_ic <- condition_frequency_table(cohort_freq = cohort1_matched_to, table_name = "visit_detail_ic")
  c1_comp2_condition_ic <- condition_frequency_table(cohort_freq = cohort1_matched_from, table_name = "visit_detail_ic")
  c2_condition_ic <- condition_frequency_table(cohort_freq = cohort2_matched, table_name = "visit_detail_ic")
  write.xlsx(target_condition_ic, file = here(secondary_output_ic, "target_condition_ic.xlsx"))
  write.xlsx(c1_comp1_condition_ic, file = here(secondary_output_ic, "c1_comp1_condition_ic.xlsx"))
  write.xlsx(c1_comp2_condition_ic, file = here(secondary_output_ic, "c1_comp2_condition_ic.xlsx"))
  write.xlsx(c2_condition_ic, file = here(secondary_output_ic, "c2_condition_ic.xlsx"))
  
  target_condition_ic_primary <- condition_frequency_table(cohort_freq = target_matched, table_name = "visit_detail_ic", primary = T)
  c1_comp1_condition_ic_primary <- condition_frequency_table(cohort_freq = cohort1_matched_to, table_name = "visit_detail_ic", primary = T)
  c1_comp2_condition_ic_primary <- condition_frequency_table(cohort_freq = cohort1_matched_from, table_name = "visit_detail_ic", primary = T)
  c2_condition_ic_primary <- condition_frequency_table(cohort_freq = cohort2_matched, table_name = "visit_detail_ic", primary = T)
  write.xlsx(target_condition_ic_primary, file = here(secondary_output_ic, "target_condition_primary_ic.xlsx"))
  write.xlsx(c1_comp1_condition_ic_primary, file = here(secondary_output_ic, "c1_comp1_condition_primary_ic.xlsx"))
  write.xlsx(c1_comp2_condition_ic_primary, file = here(secondary_output_ic, "c1_comp2_condition_primary_ic.xlsx"))
  write.xlsx(c2_condition_ic_primary, file = here(secondary_output_ic, "c2_condition_primary_ic.xlsx"))
  
  ## matched condition - Inpatient
  target_condition_inpatient <- condition_frequency_table(cohort_freq = target_matched, table_name = "visit_detail_inpatient")
  c1_comp1_condition_inpatient <- condition_frequency_table(cohort_freq = cohort1_matched_to, table_name = "visit_detail_inpatient")
  c1_comp2_condition_inpatient <- condition_frequency_table(cohort_freq = cohort1_matched_from, table_name = "visit_detail_inpatient")
  c2_condition_inpatient <- condition_frequency_table(cohort_freq = cohort2_matched, table_name = "visit_detail_inpatient")
  write.xlsx(target_condition_inpatient, file = here(secondary_output_inpatient, "target_condition_inpatient.xlsx"))
  write.xlsx(c1_comp1_condition_inpatient, file = here(secondary_output_inpatient, "c1_comp1_condition_inpatient.xlsx"))
  write.xlsx(c1_comp2_condition_inpatient, file = here(secondary_output_inpatient, "c1_comp2_condition_inpatient.xlsx"))
  write.xlsx(c2_condition_inpatient, file = here(secondary_output_inpatient, "c2_condition_inpatient.xlsx"))
  
  target_condition_inpatient_primary <- condition_frequency_table(cohort_freq = target_matched, table_name = "visit_detail_inpatient", primary = T)
  c1_comp1_condition_inpatient_primary <- condition_frequency_table(cohort_freq = cohort1_matched_to, table_name = "visit_detail_inpatient", primary = T)
  c1_comp2_condition_inpatient_primary <- condition_frequency_table(cohort_freq = cohort1_matched_from, table_name = "visit_detail_inpatient", primary = T)
  c2_condition_inpatient_primary <- condition_frequency_table(cohort_freq = cohort2_matched, table_name = "visit_detail_inpatient", primary = T)
  write.xlsx(target_condition_inpatient_primary, file = here(secondary_output_inpatient, "target_condition_primary_inpatient.xlsx"))
  write.xlsx(c1_comp1_condition_inpatient_primary, file = here(secondary_output_inpatient, "c1_comp1_condition_primary_inpatient.xlsx"))
  write.xlsx(c1_comp2_condition_inpatient_primary, file = here(secondary_output_inpatient, "c1_comp2_condition_primary_inpatient.xlsx"))
  write.xlsx(c2_condition_inpatient_primary, file = here(secondary_output_inpatient, "c2_condition_primary_inpatient.xlsx"))
  
  ## matched procedure - ALL
  target_procedure <- procedure_frequency_table(cohort_freq = target_matched, table_name = "visit_detail_hes")
  c1_comp1_procedure <- procedure_frequency_table(cohort_freq = cohort1_matched_to, table_name = "visit_detail_hes")
  c1_comp2_procedure <- procedure_frequency_table(cohort_freq = cohort1_matched_from, table_name = "visit_detail_hes")
  c2_procedure <- procedure_frequency_table(cohort_freq = cohort2_matched, table_name = "visit_detail_hes")
  write.xlsx(target_procedure, file = here(secondary_output, "target_procedure_all.xlsx"))
  write.xlsx(c1_comp1_procedure, file = here(secondary_output, "c1_comp1_procedure_all.xlsx"))
  write.xlsx(c1_comp2_procedure, file = here(secondary_output, "c1_comp2_procedure_all.xlsx"))
  write.xlsx(c2_procedure, file = here(secondary_output, "c2_procedure_all.xlsx"))
  
  target_procedure_primary <- procedure_frequency_table(cohort_freq = target_matched, table_name = "visit_detail_hes", primary = T)
  c1_comp1_procedure_primary <- procedure_frequency_table(cohort_freq = cohort1_matched_to, table_name = "visit_detail_hes", primary = T)
  c1_comp2_procedure_primary <- procedure_frequency_table(cohort_freq = cohort1_matched_from, table_name = "visit_detail_hes", primary = T)
  c2_procedure_primary <- procedure_frequency_table(cohort_freq = cohort2_matched, table_name = "visit_detail_hes", primary = T)
  write.xlsx(target_procedure_primary, file = here(secondary_output, "target_procedure_primary_all.xlsx"))
  write.xlsx(c1_comp1_procedure_primary, file = here(secondary_output, "c1_comp1_procedure_primary_all.xlsx"))
  write.xlsx(c1_comp2_procedure_primary, file = here(secondary_output, "c1_comp2_procedure_primary_all.xlsx"))
  write.xlsx(c2_procedure_primary, file = here(secondary_output, "c2_procedure_primary_all.xlsx"))
  
  ## matched procedure - IC
  target_procedure_ic <- procedure_frequency_table(cohort_freq = target_matched, table_name = "visit_detail_ic")
  c1_comp1_procedure_ic <- procedure_frequency_table(cohort_freq = cohort1_matched_to, table_name = "visit_detail_ic")
  c1_comp2_procedure_ic <- procedure_frequency_table(cohort_freq = cohort1_matched_from, table_name = "visit_detail_ic")
  c2_procedure_ic <- procedure_frequency_table(cohort_freq = cohort2_matched, table_name = "visit_detail_ic")
  write.xlsx(target_procedure_ic, file = here(secondary_output_ic, "target_procedure_ic.xlsx"))
  write.xlsx(c1_comp1_procedure_ic, file = here(secondary_output_ic, "c1_comp1_procedure_ic.xlsx"))
  write.xlsx(c1_comp2_procedure_ic, file = here(secondary_output_ic, "c1_comp2_procedure_ic.xlsx"))
  write.xlsx(c2_procedure_ic, file = here(secondary_output_ic, "c2_procedure_ic.xlsx"))
  
  target_procedure_ic_primary <- procedure_frequency_table(cohort_freq = target_matched, table_name = "visit_detail_ic", primary = T)
  c1_comp1_procedure_ic_primary <- procedure_frequency_table(cohort_freq = cohort1_matched_to, table_name = "visit_detail_ic", primary = T)
  c1_comp2_procedure_ic_primary <- procedure_frequency_table(cohort_freq = cohort1_matched_from, table_name = "visit_detail_ic", primary = T)
  c2_procedure_ic_primary <- procedure_frequency_table(cohort_freq = cohort2_matched, table_name = "visit_detail_ic", primary = T)
  write.xlsx(target_procedure_ic_primary, file = here(secondary_output_ic, "target_procedure_primary_ic.xlsx"))
  write.xlsx(c1_comp1_procedure_ic_primary, file = here(secondary_output_ic, "c1_comp1_procedure_primary_ic.xlsx"))
  write.xlsx(c1_comp2_procedure_ic_primary, file = here(secondary_output_ic, "c1_comp2_procedure_primary_ic.xlsx"))
  write.xlsx(c2_procedure_ic_primary, file = here(secondary_output_ic, "c2_procedure_primary_ic.xlsx"))
  
  ## matched procedure - Inpatient
  target_procedure_inpatient <- procedure_frequency_table(cohort_freq = target_matched, table_name = "visit_detail_inpatient")
  c1_comp1_procedure_inpatient <- procedure_frequency_table(cohort_freq = cohort1_matched_to, table_name = "visit_detail_inpatient")
  c1_comp2_procedure_inpatient <- procedure_frequency_table(cohort_freq = cohort1_matched_from, table_name = "visit_detail_inpatient")
  c2_procedure_inpatient <- procedure_frequency_table(cohort_freq = cohort2_matched, table_name = "visit_detail_inpatient")
  write.xlsx(target_procedure_inpatient, file = here(secondary_output_inpatient, "target_procedure_inpatient.xlsx"))
  write.xlsx(c1_comp1_procedure_inpatient, file = here(secondary_output_inpatient, "c1_comp1_procedure_inpatient.xlsx"))
  write.xlsx(c1_comp2_procedure_inpatient, file = here(secondary_output_inpatient, "c1_comp2_procedure_inpatient.xlsx"))
  write.xlsx(c2_procedure_inpatient, file = here(secondary_output_inpatient, "c2_procedure_inpatient.xlsx"))
  
  target_procedure_inpatient_primary <- procedure_frequency_table(cohort_freq = target_matched, table_name = "visit_detail_inpatient", primary = T)
  c1_comp1_procedure_inpatient_primary <- procedure_frequency_table(cohort_freq = cohort1_matched_to, table_name = "visit_detail_inpatient", primary = T)
  c1_comp2_procedure_inpatient_primary <- procedure_frequency_table(cohort_freq = cohort1_matched_from, table_name = "visit_detail_inpatient", primary = T)
  c2_procedure_inpatient_primary <- procedure_frequency_table(cohort_freq = cohort2_matched, table_name = "visit_detail_inpatient", primary = T)
  write.xlsx(target_procedure_inpatient_primary, file = here(secondary_output_inpatient, "target_procedure_primary_inpatient.xlsx"))
  write.xlsx(c1_comp1_procedure_inpatient_primary, file = here(secondary_output_inpatient, "c1_comp1_procedure_primary_inpatient.xlsx"))
  write.xlsx(c1_comp2_procedure_inpatient_primary, file = here(secondary_output_inpatient, "c1_comp2_procedure_primary_inpatient.xlsx"))
  write.xlsx(c2_procedure_inpatient_primary, file = here(secondary_output_inpatient, "c2_procedure_primary_inpatient.xlsx"))
  
  ## matched visit - ALL
  target_visit <- visit_summary(cohort_freq = target_matched, table_name = "visit_detail_hes")
  c1_comp1_visit <- visit_summary(cohort_freq = cohort1_matched_to, table_name = "visit_detail_hes")
  c1_comp2_visit <- visit_summary(cohort_freq = cohort1_matched_from, table_name = "visit_detail_hes")
  c2_visit <- visit_summary(cohort_freq = cohort2_matched, table_name = "visit_detail_hes")
  write.xlsx(target_visit, file = here(secondary_output, "target_visit_all.xlsx"))
  write.xlsx(c1_comp1_visit, file = here(secondary_output, "c1_comp1_visit_all.xlsx"))
  write.xlsx(c1_comp2_visit, file = here(secondary_output, "c1_comp2_visit_all.xlsx"))
  write.xlsx(c2_visit, file = here(secondary_output, "c2_visit_all.xlsx"))
  
  ## matched visit - IC
  target_visit_ic <- visit_summary(cohort_freq = target_matched, table_name = "visit_detail_ic")
  c1_comp1_visit_ic <- visit_summary(cohort_freq = cohort1_matched_to, table_name = "visit_detail_ic")
  c1_comp2_visit_ic <- visit_summary(cohort_freq = cohort1_matched_from, table_name = "visit_detail_ic")
  c2_visit_ic <- visit_summary(cohort_freq = cohort2_matched, table_name = "visit_detail_ic")
  write.xlsx(target_visit_ic, file = here(secondary_output_ic, "target_visit_ic.xlsx"))
  write.xlsx(c1_comp1_visit_ic, file = here(secondary_output_ic, "c1_comp1_visit_ic.xlsx"))
  write.xlsx(c1_comp2_visit_ic, file = here(secondary_output_ic, "c1_comp2_visit_ic.xlsx"))
  write.xlsx(c2_visit_ic, file = here(secondary_output_ic, "c2_visit_ic.xlsx"))
  
  ## matched visit - Inpatient
  target_visit_inpatient <- visit_summary(cohort_freq = target_matched, table_name = "visit_detail_inpatient")
  c1_comp1_visit_inpatient <- visit_summary(cohort_freq = cohort1_matched_to, table_name = "visit_detail_inpatient")
  c1_comp2_visit_inpatient <- visit_summary(cohort_freq = cohort1_matched_from, table_name = "visit_detail_inpatient")
  c2_visit_inpatient <- visit_summary(cohort_freq = cohort2_matched, table_name = "visit_detail_inpatient")
  write.xlsx(target_visit_inpatient, file = here(secondary_output_inpatient, "target_visit_inpatient.xlsx"))
  write.xlsx(c1_comp1_visit_inpatient, file = here(secondary_output_inpatient, "c1_comp1_visit_inpatient.xlsx"))
  write.xlsx(c1_comp2_visit_inpatient, file = here(secondary_output_inpatient, "c1_comp2_visit_inpatient.xlsx"))
  write.xlsx(c2_visit_inpatient, file = here(secondary_output_inpatient, "c2_visit_inpatient.xlsx"))
  
  
} else if(country_setting == "Spain"){
  
  secondary_output <- here(output_folder, washout_period[[k]], "secondary")
  secondary_output_ic <- here(output_folder, washout_period[[k]], "secondary_ic")
  secondary_output_inpatient <- here(output_folder, washout_period[[k]], "secondary_inpatient")
  
  if (!dir.exists(secondary_output)) {
    dir.create(secondary_output)
  }
  
  if (!dir.exists(secondary_output_ic)) {
    dir.create(secondary_output_ic)
  }
  
  if (!dir.exists(secondary_output_inpatient)) {
    dir.create(secondary_output_inpatient)
  }
  
  cdm[["visit_occurrence_hes"]] <- cdm[["visit_occurrence"]] %>%
    dplyr::filter(visit_concept_id == 32037|visit_concept_id == 9201) %>%
    CDMConnector::computeQuery(
      name = "visit_occurrence_hes",
      temporary = FALSE,
      schema = attr(cdm, "write_schema"),
      overwrite = TRUE
    )
  
  cdm[["visit_occurrence_inpatient"]] <- cdm[["visit_occurrence"]] %>%
    dplyr::filter(visit_concept_id == 9201) %>%
    CDMConnector::computeQuery(
      name = "visit_occurrence_inpatient",
      temporary = FALSE,
      schema = attr(cdm, "write_schema"),
      overwrite = TRUE
    )
  
  cdm[["visit_occurrence_ic"]] <- cdm[["visit_occurrence"]] %>%
    dplyr::filter(visit_concept_id == 32037) %>%
    CDMConnector::computeQuery(
      name = "visit_occurrence_ic",
      temporary = FALSE,
      schema = attr(cdm, "write_schema"),
      overwrite = TRUE
    )
  
  ## matched condition - ALL
  target_condition <- condition_frequency_table_sidiap(cohort_freq = target_matched, table_name = "visit_occurrence_hes")
  c1_comp1_condition <- condition_frequency_table_sidiap(cohort_freq = cohort1_matched_to, table_name = "visit_occurrence_hes")
  c1_comp2_condition <- condition_frequency_table_sidiap(cohort_freq = cohort1_matched_from, table_name = "visit_occurrence_hes")
  c2_condition <- condition_frequency_table_sidiap(cohort_freq = cohort2_matched, table_name = "visit_occurrence_hes")
  write.xlsx(target_condition, file = here(secondary_output, "target_condition_all.xlsx"))
  write.xlsx(c1_comp1_condition, file = here(secondary_output, "c1_comp1_condition_all.xlsx"))
  write.xlsx(c1_comp2_condition, file = here(secondary_output, "c1_comp2_condition_all.xlsx"))
  write.xlsx(c2_condition, file = here(secondary_output, "c2_condition_all.xlsx"))
  
  # target_condition_primary <- condition_frequency_table_sidiap(cohort_freq = target_matched, table_name = "visit_occurrence_hes", primary = T)
  # c1_comp1_condition_primary <- condition_frequency_table_sidiap(cohort_freq = cohort1_matched_to, table_name = "visit_occurrence_hes", primary = T)
  # c1_comp2_condition_primary <- condition_frequency_table_sidiap(cohort_freq = cohort1_matched_from, table_name = "visit_occurrence_hes", primary = T)
  # c2_condition_primary <- condition_frequency_table_sidiap(cohort_freq = cohort2_matched, table_name = "visit_occurrence_hes", primary = T)
  # write.xlsx(target_condition_primary, file = here(secondary_output, "target_condition_primary_all.xlsx"))
  # write.xlsx(c1_comp1_condition_primary, file = here(secondary_output, "c1_comp1_condition_primary_all.xlsx"))
  # write.xlsx(c1_comp2_condition_primary, file = here(secondary_output, "c1_comp2_condition_primary_all.xlsx"))
  # write.xlsx(c2_condition_primary, file = here(secondary_output, "c2_condition_primary_all.xlsx"))
  # 
  ## matched condition - IC
  target_condition_ic <- condition_frequency_table_sidiap(cohort_freq = target_matched, table_name = "visit_occurrence_ic")
  c1_comp1_condition_ic <- condition_frequency_table_sidiap(cohort_freq = cohort1_matched_to, table_name = "visit_occurrence_ic")
  c1_comp2_condition_ic <- condition_frequency_table_sidiap(cohort_freq = cohort1_matched_from, table_name = "visit_occurrence_ic")
  c2_condition_ic <- condition_frequency_table_sidiap(cohort_freq = cohort2_matched, table_name = "visit_occurrence_ic")
  write.xlsx(target_condition_ic, file = here(secondary_output_ic, "target_condition_ic.xlsx"))
  write.xlsx(c1_comp1_condition_ic, file = here(secondary_output_ic, "c1_comp1_condition_ic.xlsx"))
  write.xlsx(c1_comp2_condition_ic, file = here(secondary_output_ic, "c1_comp2_condition_ic.xlsx"))
  write.xlsx(c2_condition_ic, file = here(secondary_output_ic, "c2_condition_ic.xlsx"))
  
  # target_condition_ic_primary <- condition_frequency_table_sidiap(cohort_freq = target_matched, table_name = "visit_occurrence_ic", primary = T)
  # c1_comp1_condition_ic_primary <- condition_frequency_table_sidiap(cohort_freq = cohort1_matched_to, table_name = "visit_occurrence_ic", primary = T)
  # c1_comp2_condition_ic_primary <- condition_frequency_table_sidiap(cohort_freq = cohort1_matched_from, table_name = "visit_occurrence_ic", primary = T)
  # c2_condition_ic_primary <- condition_frequency_table_sidiap(cohort_freq = cohort2_matched, table_name = "visit_occurrence_ic", primary = T)
  # write.xlsx(target_condition_ic_primary, file = here(secondary_output_ic, "target_condition_primary_ic.xlsx"))
  # write.xlsx(c1_comp1_condition_ic_primary, file = here(secondary_output_ic, "c1_comp1_condition_primary_ic.xlsx"))
  # write.xlsx(c1_comp2_condition_ic_primary, file = here(secondary_output_ic, "c1_comp2_condition_primary_ic.xlsx"))
  # write.xlsx(c2_condition_ic_primary, file = here(secondary_output_ic, "c2_condition_primary_ic.xlsx"))
  # 
  ## matched condition - Inpatient
  target_condition_inpatient <- condition_frequency_table_sidiap(cohort_freq = target_matched, table_name = "visit_occurrence_inpatient")
  c1_comp1_condition_inpatient <- condition_frequency_table_sidiap(cohort_freq = cohort1_matched_to, table_name = "visit_occurrence_inpatient")
  c1_comp2_condition_inpatient <- condition_frequency_table_sidiap(cohort_freq = cohort1_matched_from, table_name = "visit_occurrence_inpatient")
  c2_condition_inpatient <- condition_frequency_table_sidiap(cohort_freq = cohort2_matched, table_name = "visit_occurrence_inpatient")
  write.xlsx(target_condition_inpatient, file = here(secondary_output_inpatient, "target_condition_inpatient.xlsx"))
  write.xlsx(c1_comp1_condition_inpatient, file = here(secondary_output_inpatient, "c1_comp1_condition_inpatient.xlsx"))
  write.xlsx(c1_comp2_condition_inpatient, file = here(secondary_output_inpatient, "c1_comp2_condition_inpatient.xlsx"))
  write.xlsx(c2_condition_inpatient, file = here(secondary_output_inpatient, "c2_condition_inpatient.xlsx"))
  
  # target_condition_inpatient_primary <- condition_frequency_table_sidiap(cohort_freq = target_matched, table_name = "visit_occurrence_inpatient", primary = T)
  # c1_comp1_condition_inpatient_primary <- condition_frequency_table_sidiap(cohort_freq = cohort1_matched_to, table_name = "visit_occurrence_inpatient", primary = T)
  # c1_comp2_condition_inpatient_primary <- condition_frequency_table_sidiap(cohort_freq = cohort1_matched_from, table_name = "visit_occurrence_inpatient", primary = T)
  # c2_condition_inpatient_primary <- condition_frequency_table_sidiap(cohort_freq = cohort2_matched, table_name = "visit_occurrence_inpatient", primary = T)
  # write.xlsx(target_condition_inpatient_primary, file = here(secondary_output_inpatient, "target_condition_primary_inpatient.xlsx"))
  # write.xlsx(c1_comp1_condition_inpatient_primary, file = here(secondary_output_inpatient, "c1_comp1_condition_primary_inpatient.xlsx"))
  # write.xlsx(c1_comp2_condition_inpatient_primary, file = here(secondary_output_inpatient, "c1_comp2_condition_primary_inpatient.xlsx"))
  # write.xlsx(c2_condition_inpatient_primary, file = here(secondary_output_inpatient, "c2_condition_primary_inpatient.xlsx"))
  # 
  ## matched procedure - ALL
  target_procedure <- procedure_frequency_table_sidiap(cohort_freq = target_matched, table_name = "visit_occurrence_hes")
  c1_comp1_procedure <- procedure_frequency_table_sidiap(cohort_freq = cohort1_matched_to, table_name = "visit_occurrence_hes")
  c1_comp2_procedure <- procedure_frequency_table_sidiap(cohort_freq = cohort1_matched_from, table_name = "visit_occurrence_hes")
  c2_procedure <- procedure_frequency_table_sidiap(cohort_freq = cohort2_matched, table_name = "visit_occurrence_hes")
  write.xlsx(target_procedure, file = here(secondary_output, "target_procedure_all.xlsx"))
  write.xlsx(c1_comp1_procedure, file = here(secondary_output, "c1_comp1_procedure_all.xlsx"))
  write.xlsx(c1_comp2_procedure, file = here(secondary_output, "c1_comp2_procedure_all.xlsx"))
  write.xlsx(c2_procedure, file = here(secondary_output, "c2_procedure_all.xlsx"))
  
  # target_procedure_primary <- procedure_frequency_table_sidiap(cohort_freq = target_matched, table_name = "visit_occurrence_hes", primary = T)
  # c1_comp1_procedure_primary <- procedure_frequency_table_sidiap(cohort_freq = cohort1_matched_to, table_name = "visit_occurrence_hes", primary = T)
  # c1_comp2_procedure_primary <- procedure_frequency_table_sidiap(cohort_freq = cohort1_matched_from, table_name = "visit_occurrence_hes", primary = T)
  # c2_procedure_primary <- procedure_frequency_table_sidiap(cohort_freq = cohort2_matched, table_name = "visit_occurrence_hes", primary = T)
  # write.xlsx(target_procedure_primary, file = here(secondary_output, "target_procedure_primary_all.xlsx"))
  # write.xlsx(c1_comp1_procedure_primary, file = here(secondary_output, "c1_comp1_procedure_primary_all.xlsx"))
  # write.xlsx(c1_comp2_procedure_primary, file = here(secondary_output, "c1_comp2_procedure_primary_all.xlsx"))
  # write.xlsx(c2_procedure_primary, file = here(secondary_output, "c2_procedure_primary_all.xlsx"))
  # 
  ## matched procedure - IC
  target_procedure_ic <- procedure_frequency_table_sidiap(cohort_freq = target_matched, table_name = "visit_occurrence_ic")
  c1_comp1_procedure_ic <- procedure_frequency_table_sidiap(cohort_freq = cohort1_matched_to, table_name = "visit_occurrence_ic")
  c1_comp2_procedure_ic <- procedure_frequency_table_sidiap(cohort_freq = cohort1_matched_from, table_name = "visit_occurrence_ic")
  c2_procedure_ic <- procedure_frequency_table_sidiap(cohort_freq = cohort2_matched, table_name = "visit_occurrence_ic")
  write.xlsx(target_procedure_ic, file = here(secondary_output_ic, "target_procedure_ic.xlsx"))
  write.xlsx(c1_comp1_procedure_ic, file = here(secondary_output_ic, "c1_comp1_procedure_ic.xlsx"))
  write.xlsx(c1_comp2_procedure_ic, file = here(secondary_output_ic, "c1_comp2_procedure_ic.xlsx"))
  write.xlsx(c2_procedure_ic, file = here(secondary_output_ic, "c2_procedure_ic.xlsx"))
  
  # target_procedure_ic_primary <- procedure_frequency_table_sidiap(cohort_freq = target_matched, table_name = "visit_occurrence_ic", primary = T)
  # c1_comp1_procedure_ic_primary <- procedure_frequency_table_sidiap(cohort_freq = cohort1_matched_to, table_name = "visit_occurrence_ic", primary = T)
  # c1_comp2_procedure_ic_primary <- procedure_frequency_table_sidiap(cohort_freq = cohort1_matched_from, table_name = "visit_occurrence_ic", primary = T)
  # c2_procedure_ic_primary <- procedure_frequency_table_sidiap(cohort_freq = cohort2_matched, table_name = "visit_occurrence_ic", primary = T)
  # write.xlsx(target_procedure_ic_primary, file = here(secondary_output_ic, "target_procedure_primary_ic.xlsx"))
  # write.xlsx(c1_comp1_procedure_ic_primary, file = here(secondary_output_ic, "c1_comp1_procedure_primary_ic.xlsx"))
  # write.xlsx(c1_comp2_procedure_ic_primary, file = here(secondary_output_ic, "c1_comp2_procedure_primary_ic.xlsx"))
  # write.xlsx(c2_procedure_ic_primary, file = here(secondary_output_ic, "c2_procedure_primary_ic.xlsx"))
  # 
  ## matched procedure - Inpatient
  target_procedure_inpatient <- procedure_frequency_table_sidiap(cohort_freq = target_matched, table_name = "visit_occurrence_inpatient")
  c1_comp1_procedure_inpatient <- procedure_frequency_table_sidiap(cohort_freq = cohort1_matched_to, table_name = "visit_occurrence_inpatient")
  c1_comp2_procedure_inpatient <- procedure_frequency_table_sidiap(cohort_freq = cohort1_matched_from, table_name = "visit_occurrence_inpatient")
  c2_procedure_inpatient <- procedure_frequency_table_sidiap(cohort_freq = cohort2_matched, table_name = "visit_occurrence_inpatient")
  write.xlsx(target_procedure_inpatient, file = here(secondary_output_inpatient, "target_procedure_inpatient.xlsx"))
  write.xlsx(c1_comp1_procedure_inpatient, file = here(secondary_output_inpatient, "c1_comp1_procedure_inpatient.xlsx"))
  write.xlsx(c1_comp2_procedure_inpatient, file = here(secondary_output_inpatient, "c1_comp2_procedure_inpatient.xlsx"))
  write.xlsx(c2_procedure_inpatient, file = here(secondary_output_inpatient, "c2_procedure_inpatient.xlsx"))
  
  # target_procedure_inpatient_primary <- procedure_frequency_table_sidiap(cohort_freq = target_matched, table_name = "visit_occurrence_inpatient", primary = T)
  # c1_comp1_procedure_inpatient_primary <- procedure_frequency_table_sidiap(cohort_freq = cohort1_matched_to, table_name = "visit_occurrence_inpatient", primary = T)
  # c1_comp2_procedure_inpatient_primary <- procedure_frequency_table_sidiap(cohort_freq = cohort1_matched_from, table_name = "visit_occurrence_inpatient", primary = T)
  # c2_procedure_inpatient_primary <- procedure_frequency_table_sidiap(cohort_freq = cohort2_matched, table_name = "visit_occurrence_inpatient", primary = T)
  # write.xlsx(target_procedure_inpatient_primary, file = here(secondary_output_inpatient, "target_procedure_primary_inpatient.xlsx"))
  # write.xlsx(c1_comp1_procedure_inpatient_primary, file = here(secondary_output_inpatient, "c1_comp1_procedure_primary_inpatient.xlsx"))
  # write.xlsx(c1_comp2_procedure_inpatient_primary, file = here(secondary_output_inpatient, "c1_comp2_procedure_primary_inpatient.xlsx"))
  # write.xlsx(c2_procedure_inpatient_primary, file = here(secondary_output_inpatient, "c2_procedure_primary_inpatient.xlsx"))
  # 
  ## matched visit - ALL
  target_visit <- visit_summary_sidiap(cohort_freq = target_matched, table_name = "visit_occurrence_hes")
  c1_comp1_visit <- visit_summary_sidiap(cohort_freq = cohort1_matched_to, table_name = "visit_occurrence_hes")
  c1_comp2_visit <- visit_summary_sidiap(cohort_freq = cohort1_matched_from, table_name = "visit_occurrence_hes")
  c2_visit <- visit_summary_sidiap(cohort_freq = cohort2_matched, table_name = "visit_occurrence_hes")
  write.xlsx(target_visit, file = here(secondary_output, "target_visit_all.xlsx"))
  write.xlsx(c1_comp1_visit, file = here(secondary_output, "c1_comp1_visit_all.xlsx"))
  write.xlsx(c1_comp2_visit, file = here(secondary_output, "c1_comp2_visit_all.xlsx"))
  write.xlsx(c2_visit, file = here(secondary_output, "c2_visit_all.xlsx"))
  
  ## matched visit - IC
  target_visit_ic <- visit_summary_sidiap(cohort_freq = target_matched, table_name = "visit_occurrence_ic")
  c1_comp1_visit_ic <- visit_summary_sidiap(cohort_freq = cohort1_matched_to, table_name = "visit_occurrence_ic")
  c1_comp2_visit_ic <- visit_summary_sidiap(cohort_freq = cohort1_matched_from, table_name = "visit_occurrence_ic")
  c2_visit_ic <- visit_summary_sidiap(cohort_freq = cohort2_matched, table_name = "visit_occurrence_ic")
  write.xlsx(target_visit_ic, file = here(secondary_output_ic, "target_visit_ic.xlsx"))
  write.xlsx(c1_comp1_visit_ic, file = here(secondary_output_ic, "c1_comp1_visit_ic.xlsx"))
  write.xlsx(c1_comp2_visit_ic, file = here(secondary_output_ic, "c1_comp2_visit_ic.xlsx"))
  write.xlsx(c2_visit_ic, file = here(secondary_output_ic, "c2_visit_ic.xlsx"))
  
  ## matched visit - Inpatient
  target_visit_inpatient <- visit_summary_sidiap(cohort_freq = target_matched, table_name = "visit_occurrence_inpatient")
  c1_comp1_visit_inpatient <- visit_summary_sidiap(cohort_freq = cohort1_matched_to, table_name = "visit_occurrence_inpatient")
  c1_comp2_visit_inpatient <- visit_summary_sidiap(cohort_freq = cohort1_matched_from, table_name = "visit_occurrence_inpatient")
  c2_visit_inpatient <- visit_summary_sidiap(cohort_freq = cohort2_matched, table_name = "visit_occurrence_inpatient")
  write.xlsx(target_visit_inpatient, file = here(secondary_output_inpatient, "target_visit_inpatient.xlsx"))
  write.xlsx(c1_comp1_visit_inpatient, file = here(secondary_output_inpatient, "c1_comp1_visit_inpatient.xlsx"))
  write.xlsx(c1_comp2_visit_inpatient, file = here(secondary_output_inpatient, "c1_comp2_visit_inpatient.xlsx"))
  write.xlsx(c2_visit_inpatient, file = here(secondary_output_inpatient, "c2_visit_inpatient.xlsx"))
}
