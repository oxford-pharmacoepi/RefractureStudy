### Creating periods
info(logger, "CREATING 6-MONTH PERIODS")
periodStart <- c()
periodEnd <- c()
numberPeriods <- lubridate::interval(study_start_date, study_end_date) %/% months(6)
periodStart[[1]] <- study_start_date
for (i in (1:numberPeriods)){
  periodEnd[[i]] <- ymd(periodStart[[i]]) %m+% months(6)-days(1)
  periodStart[[i+1]] <- periodEnd[[i]]+1
}
periodEnd[[numberPeriods+1]] <- study_end_date

### Creating relevant cohorts
info(logger, "CREATING UNDERLYING DENOMINATORS")
denom <- cdm[["denominator"]] %>% 
  dplyr::collect()

denom <- denom %>%
  dplyr::mutate(cohort_start_date = as.Date(as.character(cohort_start_date))) %>%
  dplyr::mutate(cohort_end_date = as.Date(as.character(cohort_end_date))) %>%
  dplyr::mutate(cohort_interval = interval(cohort_start_date, cohort_end_date)) %>%
  select(-cohort_definition_id) 

denom_by_periods <- list()
for (i in (1:(numberPeriods+1))){
  denom_by_periods[[i]] <- denom %>% 
    mutate(intersect = lubridate::intersect(cohort_interval, interval(periodStart[[i]], periodEnd[[i]]))) %>%
    filter(!is.na(intersect)) %>%
    select(-intersect) %>%
    mutate(period_start = periodStart[[i]], period_end = periodEnd[[i]])
}

rm(denom)
compCohort2<-list()
compCohort1<-list()
targetCohort <- list()

info(logger, "CREATING COMPARATOR COHORT 1")
for (i in (1:(numberPeriods+1))){
  compCohort1[[i]] <- denom_by_periods[[i]] %>% 
    dplyr::inner_join(fracture_table_rq2_index %>%
                        dplyr::select(-cohort_start_date, -cohort_end_date, -class) %>%
                        dplyr::filter(condition_start_date<=periodEnd[[i]] & condition_start_date >= periodStart[[i]]), by = "subject_id") %>%
    dplyr::filter(condition_start_date <= cohort_end_date & condition_start_date >= cohort_start_date)
} 
AttritionReportRQ3C1 <- AttritionReport

fracture_table_rq3_index <- fracture_table_rq2_index 

info(logger, "CREATING TARGET COHORT")
fracture_table_rq3_imminent <- fracture_table %>% 
  dplyr::group_by(subject_id) %>%
  dplyr::mutate(gap = condition_start_date - lag(condition_start_date)) %>%
  dplyr::filter(gap<=730) %>%
  dplyr::ungroup() %>%
  select(-gap)

AttritionReportRQ3T <- tibble(
  number_records = fracture_table_rq3_imminent %>% tally() %>% pull(),
  number_subjects = fracture_table_rq3_imminent %>% distinct(subject_id) %>% tally() %>% pull(),
  reason = "Starting Population - Anyone With Imminent Fracture(s)"
)

# at least 50
fracture_table_rq3_imminent <- fracture_table_rq3_imminent %>% 
  dplyr::right_join(cdm[["person"]], by = c("subject_id" = "person_id"), copy = T) %>%
  dplyr::mutate(age_fracture = lubridate::year(condition_start_date) - year_of_birth) %>%
  dplyr::filter(age_fracture >= 50) %>%
  dplyr::select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date, fracture_site)

AttritionReportRQ3T <- AttritionReportRQ3T %>% 
  union_all(
    tibble(
      number_records = fracture_table_rq3_imminent %>% tally() %>% pull(),
      number_subjects = fracture_table_rq3_imminent %>% distinct(subject_id) %>% tally() %>% pull(),
      reason = "Excluding records before the subject turning 50"
    )
  ) 

# At least 730 days prior obs
fracture_table_rq3_imminent <-fracture_table_rq3_imminent %>% 
  left_join(cdm[["observation_period"]], by = c("subject_id" = "person_id"), copy = T) %>% 
  select(subject_id:fracture_site, observation_period_start_date, observation_period_end_date) %>%
  mutate(days_prior_obs = condition_start_date - observation_period_start_date, days_after_obs = observation_period_end_date - condition_start_date) %>%
  filter(days_prior_obs >= prior_observation, days_after_obs >= 0) %>%
  select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date, fracture_site)

AttritionReportRQ3T<- AttritionReportRQ3T %>% 
  union_all(  
    tibble(
      number_records = fracture_table_rq3_imminent %>% tally() %>% pull(),
      number_subjects = fracture_table_rq3_imminent %>% distinct(subject_id) %>% tally() %>% pull(),
      reason = "Excluding records with insufficient prior observation and/or outside of observation period"
    )
  ) 

# No records of death on the index date
fracture_table_rq3_imminent <- fracture_table_rq3_imminent %>% 
  dplyr::anti_join(cdm[["death"]], by = c("subject_id" = "person_id", "condition_start_date" = "death_date"), copy = T)

AttritionReportRQ3T<- AttritionReportRQ3T %>% 
  union_all(  
    tibble(
      number_records = fracture_table_rq3_imminent %>% tally() %>% pull(),
      number_subjects = fracture_table_rq3_imminent %>% distinct(subject_id) %>% tally() %>% pull(),
      reason = "Excluding records on the same day as death"
    )
  ) 

# No records of cancer before or on the index date
fracture_table_rq3_imminent <- 
  fracture_table_rq3_imminent %>% anti_join(fracture_table_rq3_imminent %>% 
                                              dplyr::inner_join(cdm[["cancer"]] %>% dplyr::select(subject_id, cancer_date), by = "subject_id", copy = T, relationship = "many-to-many") %>%
                                              dplyr::filter(cancer_date<=condition_start_date) %>%
                                              dplyr::distinct() %>%
                                              dplyr::compute(), by = colnames(fracture_table_rq3_imminent))

AttritionReportRQ3T<- AttritionReportRQ3T %>% 
  union_all(  
    tibble(
      number_records = fracture_table_rq3_imminent %>% tally() %>% pull(),
      number_subjects = fracture_table_rq3_imminent %>% distinct(subject_id) %>% tally() %>% pull(),
      reason = "Excluding records happened after cancer"
    )
  ) 

# No records of metabolic bone disease
fracture_table_rq3_imminent <- 
  fracture_table_rq3_imminent %>% anti_join(fracture_table_rq3_imminent %>% 
                                              dplyr::inner_join(cdm[["mbd"]] %>% dplyr::select(subject_id, mbd_date), by = "subject_id", copy = T, relationship = "many-to-many") %>%
                                              dplyr::filter(mbd_date<=condition_start_date) %>%
                                              dplyr::distinct() %>%
                                              dplyr::compute(), by = colnames(fracture_table_rq3_imminent))

AttritionReportRQ3T<- AttritionReportRQ3T %>% 
  union_all(  
    tibble(
      number_records = fracture_table_rq3_imminent %>% tally() %>% pull(),
      number_subjects = fracture_table_rq3_imminent %>% distinct(subject_id) %>% tally() %>% pull(),
      reason = "Excluding records happened after metabolic bone diseases"
    )
  )  

# Excluding individuals who has index date same as obs period end date
fracture_table_rq3_imminent <- fracture_table_rq3_imminent %>%
  dplyr::anti_join(cdm[["observation_period"]], by = c("subject_id" = "person_id", "condition_start_date" = "observation_period_end_date"), copy = T)

AttritionReportRQ3T<- AttritionReportRQ3T %>% 
  union_all(  
    tibble(
      number_records = fracture_table_rq3_imminent %>% tally() %>% pull(),
      number_subjects = fracture_table_rq3_imminent %>% distinct(subject_id) %>% tally() %>% pull(),
      reason = "Excluding records that happen on the last day of observation period"
    )
  ) 

# restrict the fractures to the study period
fracture_table_rq3_imminent <- fracture_table_rq3_imminent %>% 
  dplyr::filter(condition_start_date<=cohort_end_date, condition_start_date>=cohort_start_date)

AttritionReportRQ3T<- AttritionReportRQ3T %>% 
  union_all(  
    tibble(
      number_records = fracture_table_rq3_imminent %>% tally() %>% pull(),
      number_subjects = fracture_table_rq3_imminent %>% distinct(subject_id) %>% tally() %>% pull(),
      reason = "Excluding records that happen outside of study period"
    )
  ) 

AttritionReportRQ3T<-
  rbind(AttritionReport[1:14,] %>% dplyr::select(-masked_subjects_excluded),
        AttritionReportRQ3T %>% dplyr::select(number_subjects, reason)) %>%
  dplyr::mutate(subjects_excluded = -(number_subjects-lag(number_subjects))) %>%
  dplyr::mutate(masked_subjects_excluded = ifelse((subjects_excluded<minimum_counts & subjects_excluded>0), paste0("<", minimum_counts), as.integer(.data$subjects_excluded))) %>%
  dplyr::select(-"subjects_excluded")

write.xlsx(AttritionReportRQ3T, file = here::here(output_folder, "AttritionReportRQ3Target.xlsx"))
write.xlsx(AttritionReportRQ3C1, file = here::here(output_folder, "AttritionReportRQ3CompCohort1.xlsx"))

for (i in (1:(numberPeriods+1))){
  targetCohort[[i]] <- denom_by_periods[[i]] %>% 
    dplyr::inner_join(fracture_table_rq3_imminent %>%
                        dplyr::select(-cohort_start_date, -cohort_end_date) %>%
                        dplyr::filter(condition_start_date<=periodEnd[[i]] & condition_start_date >= periodStart[[i]]), by = "subject_id") %>%
    dplyr::filter(condition_start_date <= cohort_end_date & condition_start_date >= cohort_start_date)
} 

info(logger, "CREATING COMPARATOR COHORT 2")
for (i in (1:(numberPeriods+1))){
  set.seed(12345)
  compCohort2[[i]] <- denom_by_periods[[i]] %>% 
    dplyr::anti_join(fracture_table %>% filter(condition_start_date <= periodStart[[i]]), by = "subject_id") %>%
    dplyr::mutate(index_date = sample(seq(periodStart[[i]], periodEnd[[i]], by="day"), n(), replace = T))
}

collated_c2 <- data.frame()
for (i in (1:length(compCohort2))){
  collated_c2 <- rbind(collated_c2, compCohort2[[i]])
}

AttritionReportRQ3C2 <- 
    tibble(
    number_records = collated_c2 %>% tally() %>% pull(),
    number_subjects = collated_c2 %>% distinct(subject_id) %>% tally() %>% pull(),
    reason = "Starting Population - Anyone without history of fracture prior the start of the period"
  )

### exclusion based on fractures
for (i in (1:length(compCohort2))){
  compCohort2[[i]] <- compCohort2[[i]] %>%
    dplyr::anti_join(compCohort2[[i]] %>% 
                       dplyr::left_join(fracture_table %>% dplyr::select(subject_id, condition_start_date), by = "subject_id") %>%
                       dplyr::filter(index_date >= condition_start_date), by = "subject_id")
}

collated_c2 <- data.frame()
for (i in (1:length(compCohort2))){
  collated_c2 <- rbind(collated_c2, compCohort2[[i]])
}

AttritionReportRQ3C2 <- AttritionReportRQ3C2 %>%
  union_all(  
    tibble(
      number_records = collated_c2 %>% tally() %>% pull(),
      number_subjects = collated_c2 %>% distinct(subject_id) %>% tally() %>% pull(),
      reason = "Exclusion based on fractures"
    )
  )

### excluding based on death
for (i in (1:length(compCohort2))){
  compCohort2[[i]] <- compCohort2[[i]] %>% 
    dplyr::left_join(cdm[["death"]], by = c("subject_id"= "person_id"), copy = T) %>% 
    dplyr::filter(death_date >= index_date |is.na(death_date)) %>%
    dplyr::select(colnames(compCohort2[[i]]))
}

collated_c2 <- data.frame()
for (i in (1:length(compCohort2))){
  collated_c2 <- rbind(collated_c2, compCohort2[[i]])
}

AttritionReportRQ3C2 <- AttritionReportRQ3C2 %>%
  union_all(  
    tibble(
      number_records = collated_c2 %>% tally() %>% pull(),
      number_subjects = collated_c2 %>% distinct(subject_id) %>% tally() %>% pull(),
      reason = "Exclusion based on deaths"
    )
  )

### excluding based on prior cancer
for (i in (1:length(compCohort2))){
  compCohort2[[i]] <- compCohort2[[i]] %>% 
    dplyr::anti_join(compCohort2[[i]] %>% 
                       dplyr::select(-cohort_start_date, -cohort_end_date) %>%
                       dplyr::inner_join(cdm[["cancer"]], by = "subject_id", copy = T, relationship = "many-to-many") %>%
                       dplyr::filter(cancer_date<=index_date), by = "subject_id")
}

collated_c2 <- data.frame()
for (i in (1:length(compCohort2))){
  collated_c2 <- rbind(collated_c2, compCohort2[[i]])
}

AttritionReportRQ3C2 <- AttritionReportRQ3C2 %>%
  union_all(  
    tibble(
      number_records = collated_c2 %>% tally() %>% pull(),
      number_subjects = collated_c2 %>% distinct(subject_id) %>% tally() %>% pull(),
      reason = "Exclusion based on prior cancer"
    )
  )

### Exclusion based on prior bone disease
for (i in (1:length(compCohort2))){
  compCohort2[[i]] <- compCohort2[[i]] %>% 
    dplyr::anti_join(compCohort2[[i]] %>% 
                       dplyr::select(-cohort_start_date, -cohort_end_date) %>%
                       dplyr::inner_join(cdm[["mbd"]], by = "subject_id", copy = T, relationship = "many-to-many") %>%
                       dplyr::filter(cohort_start_date<=index_date), by = "subject_id")
}

collated_c2 <- data.frame()
for (i in (1:length(compCohort2))){
  collated_c2 <- rbind(collated_c2, compCohort2[[i]])
}

AttritionReportRQ3C2 <- AttritionReportRQ3C2 %>%
  union_all(  
    tibble(
      number_records = collated_c2 %>% tally() %>% pull(),
      number_subjects = collated_c2 %>% distinct(subject_id) %>% tally() %>% pull(),
      reason = "Exclusion based on prior mbd"
    )
  )

### Exclusion based on prior obs
for (i in (1:length(compCohort2))){
  compCohort2[[i]] <-compCohort2[[i]] %>% 
    dplyr::left_join(cdm[["observation_period"]], by = c("subject_id" = "person_id"), copy = T) %>% 
    dplyr::select(subject_id:index_date, observation_period_start_date, observation_period_end_date) %>%
    dplyr::mutate(days_prior_obs = index_date - observation_period_start_date, days_after_obs = observation_period_end_date - index_date) %>%
    dplyr::filter(days_prior_obs >= prior_observation, days_after_obs >= 0) %>%
    dplyr::select(subject_id, cohort_start_date, cohort_end_date, cohort_interval, period_start, period_end, index_date)
}

collated_c2 <- data.frame()
for (i in (1:length(compCohort2))){
  collated_c2 <- rbind(collated_c2, compCohort2[[i]])
}

AttritionReportRQ3C2 <- AttritionReportRQ3C2 %>%
  union_all(  
    tibble(
      number_records = collated_c2 %>% tally() %>% pull(),
      number_subjects = collated_c2 %>% distinct(subject_id) %>% tally() %>% pull(),
      reason = "Exclusion based on prior observation length"
    )
  )

### Exclusion based on index date not being on the last day of obs period
for (i in (1:length(compCohort2))){
  compCohort2[[i]] <- compCohort2[[i]] %>%
      dplyr::anti_join(cdm[["observation_period"]], by = c("subject_id" = "person_id", "index_date" = "observation_period_end_date"), copy = T)
}

collated_c2 <- data.frame()
for (i in (1:length(compCohort2))){
  collated_c2 <- rbind(collated_c2, compCohort2[[i]])
}

AttritionReportRQ3C2 <- AttritionReportRQ3C2 %>%
  union_all(  
    tibble(
      number_records = collated_c2 %>% tally() %>% pull(),
      number_subjects = collated_c2 %>% distinct(subject_id) %>% tally() %>% pull(),
      reason = "Exclusion based on index date not being on the last day of obs period"
    )
  )

### Restrict index dates to study period
for (i in (1:length(compCohort2))){
  compCohort2[[i]] <- compCohort2[[i]] %>%
    dplyr::filter(index_date<=cohort_end_date & index_date >=cohort_start_date)
}

collated_c2 <- data.frame()
for (i in (1:length(compCohort2))){
  collated_c2 <- rbind(collated_c2, compCohort2[[i]])
}

AttritionReportRQ3C2 <- AttritionReportRQ3C2 %>%
  union_all(  
    tibble(
      number_records = collated_c2 %>% tally() %>% pull(),
      number_subjects = collated_c2 %>% distinct(subject_id) %>% tally() %>% pull(),
      reason = "Restrict index dates to study period"
    )
  )

AttritionReportRQ3C2<-
  rbind(AttritionReport[1:9,] %>% dplyr::select(-masked_subjects_excluded),
        AttritionReportRQ3C2 %>% dplyr::select(number_subjects, reason)) %>%
  dplyr::mutate(subjects_excluded = -(number_subjects-lag(number_subjects))) %>%
  dplyr::mutate(masked_subjects_excluded = ifelse((subjects_excluded<minimum_counts & subjects_excluded>0), paste0("<", minimum_counts), as.integer(.data$subjects_excluded))) %>%
  dplyr::select(-"subjects_excluded")

write.xlsx(AttritionReportRQ3C2, file = here::here(output_folder, "AttritionReportRQ3C2.xlsx"))

### Create Follow Up Time
info(logger, "CREATING FOLLOW UP TIME FOR TARGET COHORT")

# 2 years after
for (i in (1:length(targetCohort))){
  targetCohort[[i]] <- targetCohort[[i]] %>%
    dplyr::mutate(after_index = condition_start_date + 730) 
}

# end of obs period
for (i in (1:length(targetCohort))){
  targetCohort[[i]] <- targetCohort[[i]] %>%
    dplyr::left_join(cdm[["observation_period"]],  by = c("subject_id" = "person_id"), copy = T) %>%
    dplyr::select(subject_id:after_index, observation_period_end_date)
}
  
# next cancer date
for (i in (1:length(targetCohort))){
  targetCohort[[i]] <- targetCohort[[i]] %>%
    dplyr::left_join(cdm[["cancer"]] %>% dplyr::select(subject_id, cancer_date), copy = T, by = "subject_id", relationship = "many-to-many") %>%
    dplyr::group_by(subject_id, condition_concept_id, condition_start_date) %>%
    dplyr::arrange(cancer_date, .by_group = T) %>%
    dplyr::filter(row_number()==1) %>%
    dplyr::ungroup()
}

# next mbd date
for (i in (1:length(targetCohort))){
  targetCohort[[i]] <- targetCohort[[i]] %>%
    dplyr::left_join(cdm[["mbd"]] %>% dplyr::select(subject_id, mbd_date), copy = T, by = "subject_id", relationship = "many-to-many") %>%
    dplyr::group_by(subject_id, condition_concept_id, condition_start_date) %>%
    dplyr::arrange(mbd_date, .by_group = T) %>%
    dplyr::filter(row_number()==1) %>%
    dplyr::ungroup()
}

# next imminent fracture
fracture_table_rq3_imminent <- fracture_table_rq3_imminent %>% 
  dplyr::rename(imminent_date = condition_start_date) %>%
  dplyr::group_by(subject_id) %>%
  dplyr::arrange(imminent_date, .by_group = T) %>%
  dplyr::mutate(time_to_next_imminent = lead(imminent_date) - imminent_date) %>%
  dplyr::mutate(next_imminent = imminent_date + time_to_next_imminent) %>%
  dplyr::ungroup()

for (i in (1:length(targetCohort))){
  targetCohort[[i]] <- targetCohort[[i]] %>%
    dplyr::left_join(fracture_table_rq3_imminent %>% dplyr::select(subject_id, imminent_date, next_imminent), by = c("subject_id", "condition_start_date" = "imminent_date"), relationship = "many-to-many")
}

# death date
for (i in (1:length(targetCohort))){
  targetCohort[[i]] <- targetCohort[[i]] %>% 
    dplyr::left_join(cdm[["death"]], by = c("subject_id" = "person_id"), copy = T) %>%
    dplyr::filter(death_date > condition_start_date | is.na(death_date)) %>%
    dplyr::select(subject_id, cohort_start_date, cohort_end_date, cohort_interval, period_start, period_end, condition_concept_id, condition_start_date, fracture_site, after_index, observation_period_end_date, cancer_date, mbd_date, next_imminent, death_date)
}

for (i in (1:length(targetCohort))){
  targetCohort[[i]] <- targetCohort[[i]] %>%
    dplyr::mutate(follow_up_end = pmin(after_index, observation_period_end_date, cancer_date, mbd_date, next_imminent, death_date, na.rm = T)) %>%
    dplyr::rename(index_date = condition_start_date)
}

#
info(logger, "CREATING FOLLOW UP TIME FOR COMPARATOR COHORT 1")

# 2 years after
for (i in (1:length(compCohort1))){
  compCohort1[[i]] <- compCohort1[[i]] %>%
    dplyr::mutate(after_index = condition_start_date + 730) 
}

# end of obs period
for (i in (1:length(compCohort1))){
  compCohort1[[i]] <- compCohort1[[i]] %>%
    dplyr::left_join(cdm[["observation_period"]],  by = c("subject_id" = "person_id"), copy = T) %>%
    dplyr::select(subject_id:after_index, observation_period_end_date)
}

# next cancer date
for (i in (1:length(compCohort1))){
  compCohort1[[i]] <- compCohort1[[i]] %>%
    dplyr::left_join(cdm[["cancer"]] %>% dplyr::select(subject_id, cancer_date), copy = T, by = "subject_id", relationship = "many-to-many") %>%
    dplyr::group_by(subject_id, condition_concept_id, condition_start_date) %>%
    dplyr::arrange(cancer_date, .by_group = T) %>%
    dplyr::filter(row_number()==1) %>%
    dplyr::ungroup()
}

# next mbd date
for (i in (1:length(compCohort1))){
  compCohort1[[i]] <- compCohort1[[i]] %>%
    dplyr::left_join(cdm[["mbd"]] %>% dplyr::select(subject_id, mbd_date), copy = T, by = "subject_id", relationship = "many-to-many") %>%
    dplyr::group_by(subject_id, condition_concept_id, condition_start_date) %>%
    dplyr::arrange(mbd_date, .by_group = T) %>%
    dplyr::filter(row_number()==1) %>%
    dplyr::ungroup()
}

# next fracture
for (i in (1:length(compCohort1))){
  compCohort1[[i]] <- compCohort1[[i]] %>%
    dplyr::rename(index_date = condition_start_date) %>%
    dplyr::left_join(fracture_table %>% select(subject_id, condition_start_date), by = "subject_id") %>%
    dplyr::filter(condition_start_date >= index_date) %>%
    dplyr::group_by(subject_id) %>%
    dplyr::mutate(gap_to_next_fracture = lead(condition_start_date)- condition_start_date) %>%
    dplyr::mutate(ensuing_fracture = condition_start_date + gap_to_next_fracture) %>%
    dplyr::filter(condition_start_date == index_date) %>%
    dplyr::select(-gap_to_next_fracture, -condition_start_date) %>%
    dplyr::ungroup()
}

# death date and follow up end
for (i in (1:length(compCohort1))){
  compCohort1[[i]] <- compCohort1[[i]] %>% 
    dplyr::left_join(cdm[["death"]], by = c("subject_id" = "person_id"), copy = T) %>%
    dplyr::filter(death_date > index_date | is.na(death_date)) %>%
    dplyr::select(subject_id, cohort_start_date, cohort_end_date, cohort_interval, period_start, period_end, condition_concept_id, index_date, fracture_site, after_index, observation_period_end_date, cancer_date, mbd_date, ensuing_fracture, death_date) %>%
    dplyr::mutate(follow_up_end = pmin(after_index, observation_period_end_date, cancer_date, mbd_date, ensuing_fracture, death_date, na.rm = T))
}

#
info(logger, "CREATING FOLLOW UP TIME FOR COMPARATOR COHORT 2")

# 2 years after
for (i in (1:length(compCohort2))){
  compCohort2[[i]] <- compCohort2[[i]] %>%
    dplyr::mutate(after_index = index_date + 730) 
}

# end of obs period
for (i in (1:length(compCohort2))){
  compCohort2[[i]] <- compCohort2[[i]] %>%
    dplyr::left_join(cdm[["observation_period"]],  by = c("subject_id" = "person_id"), copy = T) %>%
    dplyr::select(subject_id:after_index, observation_period_end_date)
}

# next cancer date
for (i in (1:length(compCohort2))){
  compCohort2[[i]] <- compCohort2[[i]] %>%
    dplyr::left_join(cdm[["cancer"]] %>% dplyr::select(subject_id, cancer_date), copy = T, by = "subject_id", relationship = "many-to-many") %>%
    dplyr::group_by(subject_id, index_date) %>%
    dplyr::arrange(cancer_date, .by_group = T) %>%
    dplyr::filter(row_number()==1) %>%
    dplyr::ungroup()
}

# next mbd date
for (i in (1:length(compCohort2))){
  compCohort2[[i]] <- compCohort2[[i]] %>%
    dplyr::left_join(cdm[["mbd"]] %>% dplyr::select(subject_id, mbd_date), copy = T, by = "subject_id", relationship = "many-to-many") %>%
    dplyr::group_by(subject_id, index_date) %>%
    dplyr::arrange(mbd_date, .by_group = T) %>%
    dplyr::filter(row_number()==1) %>%
    dplyr::ungroup()
}

# next fracture
for (i in (1:length(compCohort2))){
  compCohort2[[i]] <- compCohort2[[i]] %>%
    dplyr::left_join(fracture_table %>% dplyr::select(subject_id, condition_start_date), by = "subject_id") %>%
    dplyr::group_by(subject_id, index_date) %>%
    dplyr::arrange(condition_start_date, .by_group = T) %>%
    dplyr::filter(row_number()==1) %>%
    dplyr::ungroup()
}

# death date and follow up end
for (i in (1:length(compCohort2))){
  compCohort2[[i]] <- compCohort2[[i]] %>% 
    dplyr::left_join(cdm[["death"]], by = c("subject_id" = "person_id"), copy = T) %>%
    dplyr::filter(death_date > index_date | is.na(death_date)) %>%
    dplyr::select(subject_id, cohort_start_date, cohort_end_date, cohort_interval, period_start, period_end, index_date, after_index, observation_period_end_date, cancer_date, mbd_date, condition_start_date, death_date) %>%
    dplyr::mutate(follow_up_end = pmin(after_index, observation_period_end_date, cancer_date, mbd_date, condition_start_date, death_date, na.rm = T)) %>%
    dplyr::rename(fracture_date = condition_start_date)
}
