# Creating follow up time
info(logger, "CREATING FOLLOW UP TIME: FOLLOWUPEND")
fracture_table_follow_up <- fracture_table

# 730 days after the index date
fracture_table_follow_up <- fracture_table_follow_up %>% mutate(after_index = index_date + 730)

# End of data collection (assuming each person has only one observation period)
fracture_table_follow_up <- fracture_table_follow_up %>% 
  left_join(cdm[["observation_period"]], by = c("subject_id" = "person_id"), copy = T) %>%
  select(subject_id:after_index, observation_period_end_date)

# Adding first cancer date after the index date
fracture_table_follow_up <- fracture_table_follow_up %>% left_join(fracture_table_follow_up %>% 
                                         inner_join(cdm[[exclusionCohortTableName]] %>% 
                                                      filter(cohort_definition_id == cancerId), 
                                                    by = "subject_id", 
                                                    copy = T, 
                                                    relationship = "many-to-many") %>%
                                         filter(index_date < cohort_start_date) %>%   
                                         group_by(subject_id, condition_concept_id, condition_start_date, fracture_site, index_date, observation_period_end_date) %>%
                                         arrange(cohort_start_date) %>%
                                         filter(row_number()==1) %>%
                                         ungroup(), by = c("subject_id", "condition_concept_id", "condition_start_date", "fracture_site", "index_date", "after_index", "observation_period_end_date")) %>%
  select(-cohort_definition_id, - cohort_end_date) %>%
  rename(cancer_date_after_index = cohort_start_date)

# Adding first bone disease date after the index date
fracture_table_follow_up <- fracture_table_follow_up %>% left_join(fracture_table_follow_up %>% 
                                                                     inner_join(cdm[[exclusionCohortTableName]] %>% 
                                                                                  filter(cohort_definition_id == BoneDiseaseId), 
                                                                                by = "subject_id", 
                                                                                copy = T, 
                                                                                relationship = "many-to-many") %>%
                                                                     filter(index_date < cohort_start_date) %>%   
                                                                     group_by(subject_id, condition_concept_id, condition_start_date, fracture_site, index_date, observation_period_end_date) %>%
                                                                     arrange(cohort_start_date) %>%
                                                                     filter(row_number()==1) %>%
                                                                     ungroup(), by = c("subject_id", "condition_concept_id", "condition_start_date", "fracture_site", "index_date", "after_index", "observation_period_end_date", "cancer_date_after_index")) %>%
  select(-cohort_definition_id, - cohort_end_date) %>%
  rename(bone_disease_date_after_index = cohort_start_date)
  
# Add in first fracture date after the index dates
fracture_table_follow_up <- fracture_table_follow_up %>% left_join(fracture_table %>% group_by(subject_id) %>% filter(condition_start_date> index_date) %>% summarise(fracture_after_index = min(condition_start_date, na.rm =  T)),
                                       by = "subject_id")

# Add in death date after the index date 
fracture_table_follow_up <- fracture_table_follow_up %>% left_join(fracture_table_follow_up %>% 
                                         left_join(cdm[["death"]], by = c("subject_id" = "person_id"), copy = T) %>%
                                         select(subject_id:death_date) %>%
                                         filter(index_date < death_date), by = c("subject_id", "condition_concept_id", "condition_start_date", "fracture_site", "index_date", "after_index", "observation_period_end_date", "cancer_date_after_index", "bone_disease_date_after_index", "fracture_after_index"))

# Add in FOLLOWUPEND
fracture_table_follow_up <- fracture_table_follow_up %>% 
  mutate(follow_up_end = pmin(after_index, observation_period_end_date, cancer_date_after_index, bone_disease_date_after_index, fracture_after_index, death_date, na.rm = T)) %>%
  mutate(follow_up_time = follow_up_end-index_date) %>%
  filter(follow_up_time > 0)

AttritionReportFrac<- AttritionReportFrac %>% 
  union_all(  
    tibble(
      cohort_definition_id = as.integer(1),
      number_records = fracture_table_follow_up %>% tally() %>% pull(),
      number_subjects = fracture_table_follow_up %>% distinct(subject_id) %>% tally() %>% pull(),
      reason = "Excluding people with 0 day follow up due to their observational period"
    )
  )

### Finalise attrition
AttritionReportFrac <- AttritionReportFrac %>% 
  mutate(subjects_excluded = -(number_subjects-lag(number_subjects)), records_excluded = -(number_records - lag(number_records)))

### Relevant counts
fracture_table_follow_up
