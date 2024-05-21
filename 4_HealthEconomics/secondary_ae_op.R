cdm[["visit_occurrence_hes_ae"]] <- cdm[["visit_occurrence"]] %>% 
  dplyr::filter(visit_concept_id == 9203) %>% 
  CDMConnector::computeQuery(
    name = "visit_occurrence_hes_ae", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

cdm[["visit_occurrence_hes_op"]] <- cdm[["visit_occurrence"]] %>% 
  dplyr::filter(visit_concept_id == 9202) %>% 
  CDMConnector::computeQuery(
    name = "visit_occurrence_hes_op", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

secondary_output_ae <- here(output_folder, washout_period[[k]], "secondary_ae")
secondary_output_op <- here(output_folder, washout_period[[k]], "secondary_op")

if (!dir.exists(secondary_output_ae)) {
  dir.create(secondary_output_ae)
}

if (!dir.exists(secondary_output_op)) {
  dir.create(secondary_output_op)
}

target_ae <- visit_summary_ae(cohort_freq = target_matched)
c1_comp1_ae <- visit_summary_ae(cohort_freq = cohort1_matched_to)
c1_comp2_ae <- visit_summary_ae(cohort_freq = cohort1_matched_from)
c2_ae <- visit_summary_ae(cohort_freq = cohort2_matched)
write.xlsx(target_ae, file = here(secondary_output_ae, "target_ae.xlsx"))
write.xlsx(c1_comp1_ae, file = here(secondary_output_ae, "c1_comp1_ae.xlsx"))
write.xlsx(c1_comp2_ae, file = here(secondary_output_ae, "c1_comp2_ae.xlsx"))
write.xlsx(c2_ae, file = here(secondary_output_ae, "c2_ae.xlsx"))

target_op <- visit_summary_op(cohort_freq = target_matched)
c1_comp1_op <- visit_summary_op(cohort_freq = cohort1_matched_to)
c1_comp2_op <- visit_summary_op(cohort_freq = cohort1_matched_from)
c2_op <- visit_summary_op(cohort_freq = cohort2_matched)

target_op[[names(target_op)[4]]] <-target_op[[names(target_op)[4]]] %>% 
  dplyr::mutate(mean_visits_per_year = ifelse((tot_visits < minimum_counts & tot_visits > 0), paste0("<", minimum_counts), as.numeric(.data$mean_visits_per_year)),
                sd_visits_per_year = ifelse((tot_visits < minimum_counts & tot_visits > 0), paste0("<", minimum_counts), as.numeric(.data$sd_visits_per_year)),
                min_visits_per_year  = ifelse((tot_visits < minimum_counts & tot_visits > 0), paste0("<", minimum_counts), as.numeric(.data$min_visits_per_year )),
                max_visits_per_year = ifelse((tot_visits < minimum_counts & tot_visits > 0), paste0("<", minimum_counts), as.numeric(.data$max_visits_per_year)),
                num_subjects_visited = ifelse((tot_visits < minimum_counts & tot_visits > 0), paste0("<", minimum_counts), as.numeric(.data$num_subjects_visited)),
                tot_visits = ifelse((tot_visits < minimum_counts & tot_visits>0), paste0("<", minimum_counts), as.numeric(.data$tot_visits)))

target_op[[names(target_op)[5]]] <-target_op[[names(target_op)[5]]] %>% 
  dplyr::mutate(mean_visits_per_year = ifelse((tot_visits < minimum_counts & tot_visits > 0), paste0("<", minimum_counts), as.numeric(.data$mean_visits_per_year)),
                sd_visits_per_year = ifelse((tot_visits < minimum_counts & tot_visits > 0), paste0("<", minimum_counts), as.numeric(.data$sd_visits_per_year)),
                min_visits_per_year  = ifelse((tot_visits < minimum_counts & tot_visits > 0), paste0("<", minimum_counts), as.numeric(.data$min_visits_per_year )),
                max_visits_per_year = ifelse((tot_visits < minimum_counts & tot_visits > 0), paste0("<", minimum_counts), as.numeric(.data$max_visits_per_year)),
                num_subjects_visited = ifelse((tot_visits < minimum_counts & tot_visits > 0), paste0("<", minimum_counts), as.numeric(.data$num_subjects_visited)),
                tot_visits = ifelse((tot_visits < minimum_counts & tot_visits>0), paste0("<", minimum_counts), as.numeric(.data$tot_visits)))

c1_comp1_op[[names(c1_comp1_op)[4]]] <-c1_comp1_op[[names(c1_comp1_op)[4]]] %>% 
  dplyr::mutate(mean_visits_per_year = ifelse((tot_visits < minimum_counts & tot_visits > 0), paste0("<", minimum_counts), as.numeric(.data$mean_visits_per_year)),
                sd_visits_per_year = ifelse((tot_visits < minimum_counts & tot_visits > 0), paste0("<", minimum_counts), as.numeric(.data$sd_visits_per_year)),
                min_visits_per_year  = ifelse((tot_visits < minimum_counts & tot_visits > 0), paste0("<", minimum_counts), as.numeric(.data$min_visits_per_year )),
                max_visits_per_year = ifelse((tot_visits < minimum_counts & tot_visits > 0), paste0("<", minimum_counts), as.numeric(.data$max_visits_per_year)),
                num_subjects_visited = ifelse((tot_visits < minimum_counts & tot_visits > 0), paste0("<", minimum_counts), as.numeric(.data$num_subjects_visited)),
                tot_visits = ifelse((tot_visits < minimum_counts & tot_visits>0), paste0("<", minimum_counts), as.numeric(.data$tot_visits)))

c1_comp1_op[[names(c1_comp1_op)[5]]] <-c1_comp1_op[[names(c1_comp1_op)[5]]] %>% 
  dplyr::mutate(mean_visits_per_year = ifelse((tot_visits < minimum_counts & tot_visits > 0), paste0("<", minimum_counts), as.numeric(.data$mean_visits_per_year)),
                sd_visits_per_year = ifelse((tot_visits < minimum_counts & tot_visits > 0), paste0("<", minimum_counts), as.numeric(.data$sd_visits_per_year)),
                min_visits_per_year  = ifelse((tot_visits < minimum_counts & tot_visits > 0), paste0("<", minimum_counts), as.numeric(.data$min_visits_per_year )),
                max_visits_per_year = ifelse((tot_visits < minimum_counts & tot_visits > 0), paste0("<", minimum_counts), as.numeric(.data$max_visits_per_year)),
                num_subjects_visited = ifelse((tot_visits < minimum_counts & tot_visits > 0), paste0("<", minimum_counts), as.numeric(.data$num_subjects_visited)),
                tot_visits = ifelse((tot_visits < minimum_counts & tot_visits>0), paste0("<", minimum_counts), as.numeric(.data$tot_visits)))

c1_comp2_op[[names(c1_comp2_op)[4]]] <-c1_comp2_op[[names(c1_comp2_op)[4]]] %>% 
  dplyr::mutate(mean_visits_per_year = ifelse((tot_visits < minimum_counts & tot_visits > 0), paste0("<", minimum_counts), as.numeric(.data$mean_visits_per_year)),
                sd_visits_per_year = ifelse((tot_visits < minimum_counts & tot_visits > 0), paste0("<", minimum_counts), as.numeric(.data$sd_visits_per_year)),
                min_visits_per_year  = ifelse((tot_visits < minimum_counts & tot_visits > 0), paste0("<", minimum_counts), as.numeric(.data$min_visits_per_year )),
                max_visits_per_year = ifelse((tot_visits < minimum_counts & tot_visits > 0), paste0("<", minimum_counts), as.numeric(.data$max_visits_per_year)),
                num_subjects_visited = ifelse((tot_visits < minimum_counts & tot_visits > 0), paste0("<", minimum_counts), as.numeric(.data$num_subjects_visited)),
                tot_visits = ifelse((tot_visits < minimum_counts & tot_visits>0), paste0("<", minimum_counts), as.numeric(.data$tot_visits)))

c1_comp2_op[[names(c1_comp2_op)[5]]] <-c1_comp2_op[[names(c1_comp2_op)[5]]] %>% 
  dplyr::mutate(mean_visits_per_year = ifelse((tot_visits < minimum_counts & tot_visits > 0), paste0("<", minimum_counts), as.numeric(.data$mean_visits_per_year)),
                sd_visits_per_year = ifelse((tot_visits < minimum_counts & tot_visits > 0), paste0("<", minimum_counts), as.numeric(.data$sd_visits_per_year)),
                min_visits_per_year  = ifelse((tot_visits < minimum_counts & tot_visits > 0), paste0("<", minimum_counts), as.numeric(.data$min_visits_per_year )),
                max_visits_per_year = ifelse((tot_visits < minimum_counts & tot_visits > 0), paste0("<", minimum_counts), as.numeric(.data$max_visits_per_year)),
                num_subjects_visited = ifelse((tot_visits < minimum_counts & tot_visits > 0), paste0("<", minimum_counts), as.numeric(.data$num_subjects_visited)),
                tot_visits = ifelse((tot_visits < minimum_counts & tot_visits>0), paste0("<", minimum_counts), as.numeric(.data$tot_visits)))

c2_op[[names(c2_op)[4]]] <-c2_op[[names(c2_op)[4]]] %>% 
  dplyr::mutate(mean_visits_per_year = ifelse((tot_visits < minimum_counts & tot_visits > 0), paste0("<", minimum_counts), as.numeric(.data$mean_visits_per_year)),
                sd_visits_per_year = ifelse((tot_visits < minimum_counts & tot_visits > 0), paste0("<", minimum_counts), as.numeric(.data$sd_visits_per_year)),
                min_visits_per_year  = ifelse((tot_visits < minimum_counts & tot_visits > 0), paste0("<", minimum_counts), as.numeric(.data$min_visits_per_year )),
                max_visits_per_year = ifelse((tot_visits < minimum_counts & tot_visits > 0), paste0("<", minimum_counts), as.numeric(.data$max_visits_per_year)),
                num_subjects_visited = ifelse((tot_visits < minimum_counts & tot_visits > 0), paste0("<", minimum_counts), as.numeric(.data$num_subjects_visited)),
                tot_visits = ifelse((tot_visits < minimum_counts & tot_visits>0), paste0("<", minimum_counts), as.numeric(.data$tot_visits)))

c2_op[[names(c2_op)[5]]] <-c2_op[[names(c2_op)[5]]] %>% 
  dplyr::mutate(mean_visits_per_year = ifelse((tot_visits < minimum_counts & tot_visits > 0), paste0("<", minimum_counts), as.numeric(.data$mean_visits_per_year)),
                sd_visits_per_year = ifelse((tot_visits < minimum_counts & tot_visits > 0), paste0("<", minimum_counts), as.numeric(.data$sd_visits_per_year)),
                min_visits_per_year  = ifelse((tot_visits < minimum_counts & tot_visits > 0), paste0("<", minimum_counts), as.numeric(.data$min_visits_per_year )),
                max_visits_per_year = ifelse((tot_visits < minimum_counts & tot_visits > 0), paste0("<", minimum_counts), as.numeric(.data$max_visits_per_year)),
                num_subjects_visited = ifelse((tot_visits < minimum_counts & tot_visits > 0), paste0("<", minimum_counts), as.numeric(.data$num_subjects_visited)),
                tot_visits = ifelse((tot_visits < minimum_counts & tot_visits>0), paste0("<", minimum_counts), as.numeric(.data$tot_visits)))

write.xlsx(target_op, file = here(secondary_output_op, "target_op.xlsx"))
write.xlsx(c1_comp1_op, file = here(secondary_output_op, "c1_comp1_op.xlsx"))
write.xlsx(c1_comp2_op, file = here(secondary_output_op, "c1_comp2_op.xlsx"))
write.xlsx(c2_op, file = here(secondary_output_op, "c2_op.xlsx"))
