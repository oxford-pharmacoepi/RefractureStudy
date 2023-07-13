rq3_table <- cdm$denominator %>% collect()
rq3_table <- rq3_table %>% left_join(fracture_table, by = "subject_id")
rq3_table <- rq3_table %>% select(-cohort_definition_id)
