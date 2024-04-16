if(country_setting == "Spain"){
  file_path <- here("4_HealthEconomics", "Inputs", "icd_ccs_cost_secondary_care_Spain.xlsx")
  ccs_spain_cost_inputs <- read_excel(file_path)
  ###TBD ccs_spain_cost_inputs_fx
  
  secondary_output <- here(output_folder, washout_period[[k]], "secondary_cost")
  secondary_output_ic <- here(output_folder, washout_period[[k]], "secondary_ic_cost")
  secondary_output_inpatient <- here(output_folder, washout_period[[k]], "secondary_inpatient_cost")
  
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
  
  ## secondary cost 
  
  target_secondary_cost_all <- secondary_cost_sidiap(cohort_freq = target_matched, table_name = "visit_occurrence_hes", cost_type = "all")
  c1_comp1_secondary_cost_all <- secondary_cost_sidiap(cohort_freq = cohort1_matched_to, table_name = "visit_occurrence_hes", cost_type = "all")
  c1_comp2_secondary_cost_all <- secondary_cost_sidiap(cohort_freq = cohort1_matched_from, table_name = "visit_occurrence_hes", cost_type = "all")
  c2_secondary_cost_all <- secondary_cost_sidiap(cohort_freq = cohort2_matched, table_name = "visit_occurrence_hes", cost_type = "all")
  write.xlsx(target_secondary_cost_all, file = here(secondary_output, "target_secondary_cost_all.xlsx"))
  write.xlsx(c1_comp1_secondary_cost_all, file = here(secondary_output, "c1_comp1_secondary_cost_all.xlsx"))
  write.xlsx(c1_comp2_secondary_cost_all, file = here(secondary_output, "c1_comp2_secondary_cost_all.xlsx"))
  write.xlsx(c2_secondary_cost_all, file = here(secondary_output, "c2_secondary_cost_all.xlsx"))
  
  ## secondary cost fracture related 
  
  # target_secondary_cost_fx_related <- secondary_cost_sidiap(cohort_freq = target_matched, table_name = "visit_occurrence_hes", cost_type = "fx_related")
  # c1_comp1_secondary_cost_fx_related <- secondary_cost_sidiap(cohort_freq = cohort1_matched_to, table_name = "visit_occurrence_hes", cost_type = "fx_related")
  # c1_comp2_secondary_cost_fx_related <- secondary_cost_sidiap(cohort_freq = cohort1_matched_from, table_name = "visit_occurrence_hes", cost_type = "fx_related")
  # c2_secondary_cost_fx_related <- secondary_cost_sidiap(cohort_freq = cohort2_matched, table_name = "visit_occurrence_hes", cost_type = "fx_related")
  # write.xlsx(target_secondary_cost_fx_related, file = here(secondary_output, "target_secondary_cost_fx_related.xlsx"))
  # write.xlsx(c1_comp1_secondary_cost_fx_related, file = here(secondary_output, "c1_comp1_secondary_cost_fx_related.xlsx"))
  # write.xlsx(c1_comp2_secondary_cost_fx_related, file = here(secondary_output, "c1_comp2_secondary_cost_fx_related.xlsx"))
  # write.xlsx(c2_secondary_cost_fx_related, file = here(secondary_output, "c2_secondary_cost_fx_related.xlsx"))
  
  ## secondary cost - Intensive Care 
  
  target_secondary_cost_all_ic <- secondary_cost_sidiap(cohort_freq = target_matched, table_name = "visit_occurrence_ic", cost_type = "all")
  c1_comp1_secondary_cost_all_ic  <- secondary_cost_sidiap(cohort_freq = cohort1_matched_to, table_name = "visit_occurrence_ic", cost_type = "all")
  c1_comp2_secondary_cost_all_ic  <- secondary_cost_sidiap(cohort_freq = cohort1_matched_from, table_name = "visit_occurrence_ic", cost_type = "all")
  c2_secondary_cost_all_ic  <- secondary_cost_sidiap(cohort_freq = cohort2_matched, table_name = "visit_occurrence_ic", cost_type = "all")
  write.xlsx(target_secondary_cost_all_ic , file = here(secondary_output_ic, "target_secondary_cost_all_ic .xlsx"))
  write.xlsx(c1_comp1_secondary_cost_all_ic , file = here(secondary_output_ic, "c1_comp1_secondary_cost_all_ic .xlsx"))
  write.xlsx(c1_comp2_secondary_cost_all_ic , file = here(secondary_output_ic, "c1_comp2_secondary_cost_all_ic .xlsx"))
  write.xlsx(c2_secondary_cost_all_ic , file = here(secondary_output_ic, "c2_secondary_cost_all_ic .xlsx"))
  
  ## secondary cost fracture related - Intensive Care
  
  # target_secondary_cost_fx_related_ic  <- secondary_cost_sidiap(cohort_freq = target_matched, table_name = "visit_occurrence_ic", cost_type = "fx_related")
  # c1_comp1_secondary_cost_fx_related_ic  <- secondary_cost_sidiap(cohort_freq = cohort1_matched_to, table_name = "visit_occurrence_ic", cost_type = "fx_related")
  # c1_comp2_secondary_cost_fx_related_ic  <- secondary_cost_sidiap(cohort_freq = cohort1_matched_from, table_name = "visit_occurrence_ic", cost_type = "fx_related")
  # c2_secondary_cost_fx_related_ic  <- secondary_cost_sidiap(cohort_freq = cohort2_matched, table_name = "visit_occurrence_ic", cost_type = "fx_related")
  # write.xlsx(target_secondary_cost_fx_related_ic , file = here(secondary_output_ic, "target_secondary_cost_fx_related_ic .xlsx"))
  # write.xlsx(c1_comp1_secondary_cost_fx_related_ic , file = here(secondary_output_ic, "c1_comp1_secondary_cost_fx_related_ic .xlsx"))
  # write.xlsx(c1_comp2_secondary_cost_fx_related_ic , file = here(secondary_output_ic, "c1_comp2_secondary_cost_fx_related_ic .xlsx"))
  # write.xlsx(c2_secondary_cost_fx_related_ic , file = here(secondary_output_ic, "c2_secondary_cost_fx_related_ic .xlsx"))
  
  ## secondary cost - Inpatient
  
  target_secondary_cost_all_inpatient <- secondary_cost_sidiap(cohort_freq = target_matched, table_name = "visit_occurrence_inpatient", cost_type = "all")
  c1_comp1_secondary_cost_all_inpatient  <- secondary_cost_sidiap(cohort_freq = cohort1_matched_to, table_name = "visit_occurrence_inpatient", cost_type = "all")
  c1_comp2_secondary_cost_all_inpatient  <- secondary_cost_sidiap(cohort_freq = cohort1_matched_from, table_name = "visit_occurrence_inpatient", cost_type = "all")
  c2_secondary_cost_all_inpatient  <- secondary_cost_sidiap(cohort_freq = cohort2_matched, table_name = "visit_occurrence_inpatient", cost_type = "all")
  write.xlsx(target_secondary_cost_all_inpatient , file = here(secondary_output_inpatient, "target_secondary_cost_all_inpatient .xlsx"))
  write.xlsx(c1_comp1_secondary_cost_all_inpatient , file = here(secondary_output_inpatient, "c1_comp1_secondary_cost_all_inpatient .xlsx"))
  write.xlsx(c1_comp2_secondary_cost_all_inpatient , file = here(secondary_output_inpatient, "c1_comp2_secondary_cost_all_inpatient .xlsx"))
  write.xlsx(c2_secondary_cost_all_inpatient , file = here(secondary_output_inpatient, "c2_secondary_cost_all_inpatient .xlsx"))
  
  ## secondary cost fracture related - Inpatient
  
  # target_secondary_cost_fx_related_inpatient  <- secondary_cost_sidiap(cohort_freq = target_matched, table_name = "visit_occurrence_inpatient", cost_type = "fx_related")
  # c1_comp1_secondary_cost_fx_related_inpatient  <- secondary_cost_sidiap(cohort_freq = cohort1_matched_to, table_name = "visit_occurrence_inpatient", cost_type = "fx_related")
  # c1_comp2_secondary_cost_fx_related_inpatient  <- secondary_cost_sidiap(cohort_freq = cohort1_matched_from, table_name = "visit_occurrence_inpatient", cost_type = "fx_related")
  # c2_secondary_cost_fx_related_inpatient  <- secondary_cost_sidiap(cohort_freq = cohort2_matched, table_name = "visit_occurrence_inpatient", cost_type = "fx_related")
  # write.xlsx(target_secondary_cost_fx_related_inpatient , file = here(secondary_output_inpatient, "target_secondary_cost_fx_related_inpatient .xlsx"))
  # write.xlsx(c1_comp1_secondary_cost_fx_related_inpatient , file = here(secondary_output_inpatient, "c1_comp1_secondary_cost_fx_related_inpatient .xlsx"))
  # write.xlsx(c1_comp2_secondary_cost_fx_related_inpatient , file = here(secondary_output_inpatient, "c1_comp2_secondary_cost_fx_related_inpatient .xlsx"))
  # write.xlsx(c2_secondary_cost_fx_related_inpatient , file = here(secondary_output_inpatient, "c2_secondary_cost_fx_related_inpatient .xlsx"))
  
} else if (country_setting == "UK"){
  file_path_4 <- here("4_HealthEconomics", "Inputs", "simplified_nhs_hrg_grouper_v2_gf.xlsx")
  grouper_uk_Diag_incl <- read_excel(file_path_4, sheet = "Diag_incl")
  grouper_uk_Diag_HRG <- read_excel(file_path_4, sheet = "Diag_HRG")
  grouper_uk_Proc_HRG_any <- read_excel(file_path_4, sheet = "Proc_HRG_any")
  grouper_uk_Proc_HRG_STM <- read_excel(file_path_4, sheet = "Proc_HRG_STM")
  grouper_uk_unit_cost <- read_excel(file_path_4, sheet = "Unit costs")
  
  secondary_output <- here(output_folder, washout_period[[k]], "secondary_cost")
  secondary_output_ic <- here(output_folder, washout_period[[k]], "secondary_ic_cost")
  secondary_output_inpatient <- here(output_folder, washout_period[[k]], "secondary_inpatient_cost")
  
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
  
  
  target_secondary_cost_all <- secondary_cost_cprd(cohort_freq = target_matched, table_name = "visit_occurrence_hes")
  c1_comp1_secondary_cost_all <- secondary_cost_cprd(cohort_freq = cohort1_matched_to, table_name = "visit_occurrence_hes")
  c1_comp2_secondary_cost_all <- secondary_cost_cprd(cohort_freq = cohort1_matched_from, table_name = "visit_occurrence_hes")
  c2_secondary_cost_all <- secondary_cost_cprd(cohort_freq = cohort2_matched, table_name = "visit_occurrence_hes")
  write.xlsx(target_secondary_cost_all, file = here(secondary_output, "target_secondary_cost_all.xlsx"))
  write.xlsx(c1_comp1_secondary_cost_all, file = here(secondary_output, "c1_comp1_secondary_cost_all.xlsx"))
  write.xlsx(c1_comp2_secondary_cost_all, file = here(secondary_output, "c1_comp2_secondary_cost_all.xlsx"))
  write.xlsx(c2_secondary_cost_all, file = here(secondary_output, "c2_secondary_cost_all.xlsx"))
  
  target_secondary_cost_all_ic <- secondary_cost_cprd(cohort_freq = target_matched, table_name = "visit_occurrence_ic")
  c1_comp1_secondary_cost_all_ic  <- secondary_cost_cprd(cohort_freq = cohort1_matched_to, table_name = "visit_occurrence_ic")
  c1_comp2_secondary_cost_all_ic  <- secondary_cost_cprd(cohort_freq = cohort1_matched_from, table_name = "visit_occurrence_ic")
  c2_secondary_cost_all_ic  <- secondary_cost_cprd(cohort_freq = cohort2_matched, table_name = "visit_occurrence_ic")
  write.xlsx(target_secondary_cost_all_ic , file = here(secondary_output_ic, "target_secondary_cost_all_ic .xlsx"))
  write.xlsx(c1_comp1_secondary_cost_all_ic , file = here(secondary_output_ic, "c1_comp1_secondary_cost_all_ic .xlsx"))
  write.xlsx(c1_comp2_secondary_cost_all_ic , file = here(secondary_output_ic, "c1_comp2_secondary_cost_all_ic .xlsx"))
  write.xlsx(c2_secondary_cost_all_ic , file = here(secondary_output_ic, "c2_secondary_cost_all_ic .xlsx"))
  
  target_secondary_cost_all_inpatient <- secondary_cost_cprd(cohort_freq = target_matched, table_name = "visit_occurrence_inpatient")
  c1_comp1_secondary_cost_all_inpatient  <- secondary_cost_cprd(cohort_freq = cohort1_matched_to, table_name = "visit_occurrence_inpatient")
  c1_comp2_secondary_cost_all_inpatient  <- secondary_cost_cprd(cohort_freq = cohort1_matched_from, table_name = "visit_occurrence_inpatient")
  c2_secondary_cost_all_inpatient  <- secondary_cost_cprd(cohort_freq = cohort2_matched, table_name = "visit_occurrence_inpatient")
  write.xlsx(target_secondary_cost_all_inpatient , file = here(secondary_output_inpatient, "target_secondary_cost_all_inpatient .xlsx"))
  write.xlsx(c1_comp1_secondary_cost_all_inpatient , file = here(secondary_output_inpatient, "c1_comp1_secondary_cost_all_inpatient .xlsx"))
  write.xlsx(c1_comp2_secondary_cost_all_inpatient , file = here(secondary_output_inpatient, "c1_comp2_secondary_cost_all_inpatient .xlsx"))
  write.xlsx(c2_secondary_cost_all_inpatient , file = here(secondary_output_inpatient, "c2_secondary_cost_all_inpatient .xlsx"))
  
}