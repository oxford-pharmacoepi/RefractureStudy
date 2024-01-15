cdm[["visit_occurrence_hes"]] <- cdm[["visit_occurrence"]] %>% 
  dplyr::filter(viist_concept_id == 9201) %>% 
  CDMConnector::computeQuery(
    name = "visit_occurrence_hes", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )
