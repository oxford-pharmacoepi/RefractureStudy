################################ Adding index date ###################################
info(logger, "DEFINING INDEX DATE FOR EACH INDIVIDUAL")
fracture_table_rq2 <- addIndex(fracture_table) 

AttritionReportRQ2<- AttritionReportFrac[,1:4] %>% 
  union_all(  
    tibble(
      cohort_definition_id = as.integer(1),
      number_records = fracture_table_rq2 %>% tally() %>% pull(),
      number_subjects = fracture_table_rq2 %>% distinct(subject_id) %>% tally() %>% pull(),
      reason = "Excluding individuals who do not have a fracture within the study period"
    )
  ) 

### Exclusion criteria
# At least 730 days prior obs
info(logger, "EXCLUDING INDIVIDUALS WHO DO NOT HAVE SUFFICIENT PRIOR OBSERVATION")
fracture_table_rq2 <-fracture_table_rq2 %>% 
  left_join(cdm[["observation_period"]], by = c("subject_id" = "person_id"), copy = T) %>% 
  select(subject_id:index_date, observation_period_start_date, observation_period_end_date) %>%
  mutate(days_prior_obs = index_date - observation_period_start_date, days_after_obs = observation_period_end_date - index_date) %>%
  filter(days_prior_obs >= prior_observation, days_after_obs >= 0) %>%
  select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date, fracture_site, index_date)

AttritionReportRQ2<- AttritionReportRQ2 %>% 
  union_all(  
    tibble(
      cohort_definition_id = as.integer(1),
      number_records = fracture_table_rq2 %>% tally() %>% pull(),
      number_subjects = fracture_table_rq2 %>% distinct(subject_id) %>% tally() %>% pull(),
      reason = "Excluding individuals who do not have sufficient prior observation"
    )
  ) 

# No records of death on the index date
info(logger, "EXCLUDING INDIVIDUALS WHO HAS A RECORD OF DEATH ON THE SAME DAY AS THE INDEX DATE")

fracture_table_rq2 <- noDeathOnIndex(fracture_table_rq2)

AttritionReportRQ2<- AttritionReportRQ2 %>% 
  union_all(  
    tibble(
      cohort_definition_id = as.integer(1),
      number_records = fracture_table_rq2 %>% tally() %>% pull(),
      number_subjects = fracture_table_rq2 %>% distinct(subject_id) %>% tally() %>% pull(),
      reason = "Excluding individuals who has a record of death on the same day as the index date"
    )
  ) 

# No records of cancer before or on the index date
info(logger, "EXCLUDING INDIVIDUALS WHO HAS A RECORD OF CANCER OF INTEREST BEFORE THE INDEX DATE")

fracture_table_rq2 <- noCancerPriorOrOnIndex(fracture_table_rq2)

AttritionReportRQ2<- AttritionReportRQ2 %>% 
  union_all(  
    tibble(
      cohort_definition_id = as.integer(1),
      number_records = fracture_table_rq2 %>% tally() %>% pull(),
      number_subjects = fracture_table_rq2 %>% distinct(subject_id) %>% tally() %>% pull(),
      reason = "Excluding individuals who has a record of cancer of interest before the index date"
    )
  ) 

# No records of metabolic bone disease
info(logger, "EXCLUDING INDIVIDUALS WHO HAS A RECORD OF METABOLIC BONE DISEASE OF INTEREST BEFORE THE INDEX DATE")

fracture_table_rq2 <- noBoneDiseasePriorOrOnIndex(fracture_table_rq2)

AttritionReportRQ2<- AttritionReportRQ2 %>% 
  union_all(  
    tibble(
      cohort_definition_id = as.integer(1),
      number_records = fracture_table_rq2 %>% tally() %>% pull(),
      number_subjects = fracture_table_rq2 %>% distinct(subject_id) %>% tally() %>% pull(),
      reason = "Excluding individuals who has a record of metabolic bone diseases of interest before the index date"
    )
  ) 

# Excluding individuals who had a fracture within 2 years before the index date
info(logger, "EXCLUDING INDIVIDUALS WHO HAD A FRACTURE THAT HAPPENED WITHIN 2 YEARS BEFORE THE INDEX DATE")

fracture_table_rq2 <- fracture_table_rq2 %>%
  anti_join(fracture_table_rq2 %>% filter(condition_start_date < index_date & condition_start_date>=index_date-730), by = "subject_id") 

AttritionReportRQ2<- AttritionReportRQ2 %>% 
  union_all(  
    tibble(
      cohort_definition_id = as.integer(1),
      number_records = fracture_table_rq2 %>% tally() %>% pull(),
      number_subjects = fracture_table_rq2 %>% distinct(subject_id) %>% tally() %>% pull(),
      reason = "Excluding individuals who had a record of fractures within 2 years before the index date"
    )
  )

# Excluding individuals who has index date same as obs period end date
info(logger, "EXCLUDING INDIVIDUALS WHO HAS INDEX DATE ON THE SAME DATE AS THE OBSERVATION PERIOD END DATE")

fracture_table_rq2 <- fracture_table_rq2 %>% 
  anti_join(cdm[["observation_period"]], by = c("subject_id" = "person_id", "index_date" = "observation_period_end_date"), copy = T)

AttritionReportRQ2<- AttritionReportRQ2 %>% 
  union_all(  
    tibble(
      cohort_definition_id = as.integer(1),
      number_records = fracture_table_rq2 %>% tally() %>% pull(),
      number_subjects = fracture_table_rq2 %>% distinct(subject_id) %>% tally() %>% pull(),
      reason = "Excluding individuals who has index date same as observation period end date"
    )
  )

fracture_table_rq1 <- fracture_table_rq2

### Finalise attrition
AttritionReportRQ2 <- AttritionReportRQ2 %>% 
  mutate(subjects_excluded = -(number_subjects-lag(number_subjects)), records_excluded = -(number_records - lag(number_records)))

fracture_table <- fracture_table %>% 
  group_by(subject_id) %>% 
  arrange(condition_start_date, .by_group = T) %>%
  ungroup() %>%
  arrange(subject_id)

write.xlsx(AttritionReportRQ2, file = here::here(output_folder, "AttritionReportRQ2.xlsx"))

