fracture_table_rq2 <- fracture_table

### Exclusion criteria
# no fractures 730 prior
info(logger, "EXCLUDING FRACTURE RECORDS THAT HAS ANOTHER FRACTURE WHICH HAPPENED WITHIN 2 YEARS BEFORE THE INDEX DATE")
fracture_table_rq2 <- fracture_table_rq2 %>%
  dplyr::group_by(subject_id) %>%
  dplyr::arrange(condition_start_date, .by_group = T) %>%
  dplyr::mutate(gap = condition_start_date - lag(condition_start_date)) %>%
  dplyr::filter(gap>730|is.na(gap)) %>%
  dplyr::ungroup() %>%
  dplyr::select(-gap)

AttritionReportRQ2<- AttritionReportFrac[,1:4] %>% 
  union_all(  
    tibble(
      cohort_definition_id = as.integer(1),
      number_records = fracture_table_rq2 %>% tally() %>% pull(),
      number_subjects = fracture_table_rq2 %>% distinct(subject_id) %>% tally() %>% pull(),
      reason = "Excluding records that has at least one fracture 730 days prior"
    )
  ) 

# at least 50
fracture_table_rq2 <- fracture_table_rq2 %>% 
  dplyr::right_join(cdm[["person"]], by = c("subject_id" = "person_id"), copy = T) %>%
  dplyr::mutate(age_fracture = lubridate::year(condition_start_date) - year_of_birth) %>%
  dplyr::filter(age_fracture >= 50) %>%
  dplyr::select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date, fracture_site)

AttritionReportRQ2<- AttritionReportRQ2 %>% 
  union_all(  
    tibble(
      cohort_definition_id = as.integer(1),
      number_records = fracture_table_rq2 %>% tally() %>% pull(),
      number_subjects = fracture_table_rq2 %>% distinct(subject_id) %>% tally() %>% pull(),
      reason = "Excluding records before the subject turning 50"
    )
  )

# At least 730 days prior obs
info(logger, "EXCLUDING INDIVIDUALS WHO DO NOT HAVE SUFFICIENT PRIOR OBSERVATION")
fracture_table_rq2 <-fracture_table_rq2 %>% 
  left_join(cdm[["observation_period"]], by = c("subject_id" = "person_id"), copy = T) %>% 
  select(subject_id:fracture_site, observation_period_start_date, observation_period_end_date) %>%
  mutate(days_prior_obs = condition_start_date - observation_period_start_date, days_after_obs = observation_period_end_date - condition_start_date) %>%
  filter(days_prior_obs >= prior_observation, days_after_obs >= 0) %>%
  select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date, fracture_site)

AttritionReportRQ2<- AttritionReportRQ2 %>% 
  union_all(  
    tibble(
      cohort_definition_id = as.integer(1),
      number_records = fracture_table_rq2 %>% tally() %>% pull(),
      number_subjects = fracture_table_rq2 %>% distinct(subject_id) %>% tally() %>% pull(),
      reason = "Excluding records with insufficient prior observation"
    )
  ) 

# No records of death on the index date
info(logger, "EXCLUDING INDIVIDUALS WHO HAS A RECORD OF DEATH ON THE SAME DAY AS THE INDEX DATE")

fracture_table_rq2 <- fracture_table_rq2 %>% 
  dplyr::anti_join(cdm[["death"]], by = c("subject_id" = "person_id", "condition_start_date" = "death_date"), copy = T)

AttritionReportRQ2<- AttritionReportRQ2 %>% 
  union_all(  
    tibble(
      cohort_definition_id = as.integer(1),
      number_records = fracture_table_rq2 %>% tally() %>% pull(),
      number_subjects = fracture_table_rq2 %>% distinct(subject_id) %>% tally() %>% pull(),
      reason = "Excluding records on the same day as death"
    )
  ) 

# No records of cancer before or on the index date
info(logger, "EXCLUDING INDIVIDUALS WHO HAS A RECORD OF CANCER OF INTEREST BEFORE THE INDEX DATE")

fracture_table_rq2 <- 
  fracture_table_rq2 %>% anti_join(fracture_table_rq2 %>% 
                                     dplyr::inner_join(cdm[["cancer"]] %>% dplyr::select(subject_id, cancer_date), by = "subject_id", copy = T, relationship = "many-to-many") %>%
                                     dplyr::filter(cancer_date<=condition_start_date) %>%
                                     dplyr::distinct() %>%
                                     dplyr::compute(), by = colnames(fracture_table_rq2))

AttritionReportRQ2<- AttritionReportRQ2 %>% 
  union_all(  
    tibble(
      cohort_definition_id = as.integer(1),
      number_records = fracture_table_rq2 %>% tally() %>% pull(),
      number_subjects = fracture_table_rq2 %>% distinct(subject_id) %>% tally() %>% pull(),
      reason = "Excluding records happened after cancer"
    )
  ) 

# No records of metabolic bone disease
info(logger, "EXCLUDING INDIVIDUALS WHO HAS A RECORD OF METABOLIC BONE DISEASE OF INTEREST BEFORE THE INDEX DATE")

fracture_table_rq2 <- 
  fracture_table_rq2 %>% anti_join(fracture_table_rq2 %>% 
                                     dplyr::inner_join(cdm[["mbd"]] %>% dplyr::select(subject_id, mbd_date), by = "subject_id", copy = T, relationship = "many-to-many") %>%
                                     dplyr::filter(mbd_date<=condition_start_date) %>%
                                     dplyr::distinct() %>%
                                     dplyr::compute(), by = colnames(fracture_table_rq2))

AttritionReportRQ2<- AttritionReportRQ2 %>% 
  union_all(  
    tibble(
      cohort_definition_id = as.integer(1),
      number_records = fracture_table_rq2 %>% tally() %>% pull(),
      number_subjects = fracture_table_rq2 %>% distinct(subject_id) %>% tally() %>% pull(),
      reason = "Excluding records happened after cancer"
    )
  )  

# Excluding individuals who has index date same as obs period end date
info(logger, "EXCLUDING INDIVIDUALS WHO HAS INDEX DATE ON THE SAME DATE AS THE OBSERVATION PERIOD END DATE")

fracture_table_rq2 <- fracture_table_rq2 %>%
  dplyr::anti_join(cdm[["observation_period"]], by = c("subject_id" = "person_id", "condition_start_date" = "observation_period_end_date"), copy = T)

AttritionReportRQ2<- AttritionReportRQ2 %>% 
  union_all(  
    tibble(
      cohort_definition_id = as.integer(1),
      number_records = fracture_table_rq2 %>% tally() %>% pull(),
      number_subjects = fracture_table_rq2 %>% distinct(subject_id) %>% tally() %>% pull(),
      reason = "Excluding records that happen on the last day of observation period"
    )
  ) 

# restrict the fractures to the study period
info(logger, "EXCLUDING RECORDS THAT HAPPENED OUTSIDE OF STUDY PERIOD")

fracture_table_rq2 <- fracture_table_rq2 %>% 
  dplyr::filter(condition_start_date<=cohort_end_date, condition_start_date>=cohort_start_date)

AttritionReportRQ2<- AttritionReportRQ2 %>% 
  union_all(  
    tibble(
      cohort_definition_id = as.integer(1),
      number_records = fracture_table_rq2 %>% tally() %>% pull(),
      number_subjects = fracture_table_rq2 %>% distinct(subject_id) %>% tally() %>% pull(),
      reason = "Excluding records that happen outside of study period"
    )
  ) 

fracture_table_rq1 <- fracture_table_rq2

### Finalise attrition
AttritionReportRQ2 <- AttritionReportRQ2 %>% 
  dplyr::mutate(subjects_excluded = -(number_subjects-lag(number_subjects)), records_excluded = -(number_records - lag(number_records)))

AttritionReportRQ2 <- AttritionReportRQ2 %>%
  dplyr::mutate(masked_records = ifelse((records_excluded<5 & records_excluded>0), "<5", as.integer(.data$records_excluded)),
                masked_subjects = ifelse((subjects_excluded<5 & subjects_excluded>0), "<5", as.integer(.data$subjects_excluded))) %>%
  dplyr::select(-c("records_excluded", "subjects_excluded"))

fracture_table <- fracture_table %>% 
  group_by(subject_id) %>% 
  arrange(condition_start_date, .by_group = T) %>%
  ungroup() %>%
  arrange(subject_id)

# write.xlsx(AttritionReportRQ2, file = here::here(output_folder, "AttritionReportRQ2.xlsx"))

AttritionReport <- rbind(AttritionReportDenom %>% dplyr::select(number_subjects, reason),
              AttritionReportRQ2 %>% dplyr::select(number_subjects, reason)) %>% 
  dplyr::mutate(subjects_excluded = -(number_subjects-lag(number_subjects))) %>%
  dplyr::mutate(masked_subjects_excluded = ifelse((subjects_excluded<5 & subjects_excluded>0), "<5", as.integer(.data$subjects_excluded))) %>%
  dplyr::select(-"subjects_excluded")
  
write.xlsx(AttritionReport, file = here::here(output_folder, "AttritionReport.xlsx"))
