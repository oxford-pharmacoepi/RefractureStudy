### Creating periods
info(logger, "CREATING 6-MONTH PERIODS")
periodStart <- c()
periodEnd <- c()
numberPeriods <- as.integer(-time_length(difftime(study_start_date, study_end_date), "years")*2)
periodStart[[1]] <- study_start_date
for (i in (1:numberPeriods)){
  periodEnd[[i]] <- ymd(periodStart[[i]]) %m+% months(6)-days(1)
  periodStart[[i+1]] <- periodEnd[[i]]+1
}
periodEnd[[numberPeriods+1]] <- study_end_date

### Creating relevant cohorts
denom <- cdm[["denominator"]] %>% 
  mutate(cohort_start_date = as.Date(as.character(cohort_start_date))) %>%
  mutate(cohort_end_date = as.Date(as.character(cohort_end_date))) %>%
  collect()

denom <- 
  denom %>% 
  mutate(cohort_interval = interval(cohort_start_date, cohort_end_date)) %>%
  select(-cohort_definition_id)

denom_by_periods <- list()

for (i in (1:(numberPeriods+1))){
  denom_by_periods[[i]] <- denom %>% 
    mutate(intersect = lubridate::intersect(cohort_interval, interval(periodStart[[i]], periodEnd[[i]]))) %>%
    filter(!is.na(intersect)) %>%
    select(-intersect) %>%
    mutate(period_start = periodStart[[i]], period_end = periodEnd[[i]])
}

compCohort2<-list()
compCohort1<-list()
targetCohort <- list()

for (i in (1:(numberPeriods+1))){
  compCohort2[[i]] <- denom_by_periods[[i]] %>% 
    anti_join(fracture_table %>% filter(condition_start_date <= periodEnd[[i]]), by = "subject_id")
  compCohort2[[i]] <- compCohort2[[i]] %>% 
    mutate(index_date = sample(seq(periodStart[[i]], periodEnd[[i]], by="day"), n(), replace = T))
}

fractureCohorts <- list()
for (i in (1:(numberPeriods+1))){
  fractureCohorts[[i]] <- denom_by_periods[[i]] %>% 
    left_join(fracture_table %>% select(-cohort_start_date, -cohort_end_date) %>% filter(condition_start_date <= periodEnd[[i]]) %>% filter(condition_start_date >= periodStart[[i]]), by = "subject_id") %>%
    filter(!is.na(condition_start_date)) %>%
    mutate(two_years_before = condition_start_date-730)
}

imminentFractures <- list()
for (i in (1:(numberPeriods+1))){
  imminentFractures[[i]] <- fractureCohorts[[i]] %>% 
    left_join(fracture_table, by = c("subject_id", "cohort_start_date", "cohort_end_date"), relationship = "many-to-many") %>%
    filter(condition_start_date.y >= two_years_before) %>%
    filter(condition_start_date.y < two_years_before +730) %>%
    rename(condition_start_date = condition_start_date.x,
           condition_concept_id = condition_concept_id.x,
           fracture_site = fracture_site.x) %>%
    select(-two_years_before, -condition_start_date.y, -condition_concept_id.y, -fracture_site.y) %>%
    distinct()
}

for (i in (1:length(imminentFractures))){
  targetCohort[[i]] <- imminentFractures[[i]] %>% 
    rename(index_date = condition_start_date)
}

for (i in (1:length(compCohort2))){
  compCohort1[[i]]<-fractureCohorts[[i]] %>%
    anti_join(fractureCohorts[[i]] %>% 
                left_join(fracture_table, by = c("subject_id", "cohort_start_date", "cohort_end_date"), relationship = "many-to-many") %>%
                filter(condition_start_date.y >= two_years_before) %>%
                filter(condition_start_date.y < two_years_before +730), 
              by = "subject_id") %>%
    select(-two_years_before) %>%
    rename (index_date = condition_start_date)
}

accumulativeTargetCohort <- list()
accumulativeTargetCohort[[1]]<-targetCohort[[1]]
for (i in (2:length(targetCohort))){
  accumulativeTargetCohort[[i]] <- rbind(accumulativeTargetCohort[[i-1]], targetCohort[[i]])
}

for (i in (2:length(compCohort1))){
  compCohort1[[i]] <- compCohort1[[i]] %>% 
    anti_join(accumulativeTargetCohort[[i-1]], by = "subject_id")
}

AttritionReportRQ3<-tibble(
  reason_id = 1,
  cohort_definition = "Comparator Cohort 2",
  number_records = compCohort2[[1]] %>% tally() %>% pull(),
  number_subjects = compCohort2[[1]] %>% distinct(subject_id) %>% tally() %>% pull(),
  reason = "Starting Population",
  period = 1
)

for (i in (2:length(compCohort2))){
  AttritionReportRQ3 <- AttritionReportRQ3 %>%
    union_all(
      tibble(
        reason_id = 1,
        cohort_definition = "Comparator Cohort 2",
        number_records = compCohort2[[i]] %>% tally() %>% pull(),
        number_subjects = compCohort2[[i]] %>% distinct(subject_id) %>% tally() %>% pull(),
        reason = "Starting Population",
        period = i
  )
)
}

for (i in (1:length(compCohort1))){
  AttritionReportRQ3 <- AttritionReportRQ3 %>%
    union_all(
      tibble(
        reason_id = 1,
        cohort_definition = "Comparator Cohort 1",
        number_records = compCohort1[[i]] %>% tally() %>% pull(),
        number_subjects = compCohort1[[i]] %>% distinct(subject_id) %>% tally() %>% pull(),
        reason = "Starting Population",
        period = i
      )
    )
}

for (i in (1:length(targetCohort))){
  AttritionReportRQ3 <- AttritionReportRQ3 %>%
    union_all(
      tibble(
        reason_id = 1,
        cohort_definition = "Target cohort",
        number_records = targetCohort[[i]] %>% tally() %>% pull(),
        number_subjects = targetCohort[[i]] %>% distinct(subject_id) %>% tally() %>% pull(),
        reason = "Starting Population",
        period = i
      )
    )
}

### Exclusion based on death 
for (i in (1:length(targetCohort))){
  targetCohort[[i]] <- noDeathOnOrAfterIndex(targetCohort[[i]])
}

for (i in (1:length(compCohort1))){
  compCohort1[[i]] <- noDeathOnOrAfterIndex(compCohort1[[i]])
}

for (i in (1:length(compCohort2))){
  compCohort2[[i]] <- noDeathOnOrAfterIndex(compCohort2[[i]])
}

for (i in (1:length(targetCohort))){
  AttritionReportRQ3 <- AttritionReportRQ3 %>%
    union_all(
      tibble(
        reason_id = 2,
        cohort_definition = "Target cohort",
        number_records = targetCohort[[i]] %>% tally() %>% pull(),
        number_subjects = targetCohort[[i]] %>% distinct(subject_id) %>% tally() %>% pull(),
        reason = "Excluding based on death",
        period = i
      )
    )
}

for (i in (1:length(compCohort1))){
  AttritionReportRQ3 <- AttritionReportRQ3 %>%
    union_all(
      tibble(
        reason_id = 2,
        cohort_definition = "Comparator Cohort 1",
        number_records = compCohort1[[i]] %>% tally() %>% pull(),
        number_subjects = compCohort1[[i]] %>% distinct(subject_id) %>% tally() %>% pull(),
        reason = "Excluding based on death",
        period = i
      )
    )
}

for (i in (1:length(compCohort2))){
  AttritionReportRQ3 <- AttritionReportRQ3 %>%
    union_all(
      tibble(
        reason_id = 2,
        cohort_definition = "Comparator Cohort 2",
        number_records = compCohort2[[i]] %>% tally() %>% pull(),
        number_subjects = compCohort2[[i]] %>% distinct(subject_id) %>% tally() %>% pull(),
        reason = "Excluding based on death",
        period = i
      )
    )
}

### Exclusion based on index date must lie within the cohort start and cohort end date 
for (i in (1:length(targetCohort))){
  targetCohort[[i]] <- targetCohort[[i]] %>% filter(index_date>=cohort_start_date & index_date <=cohort_end_date)
}

for (i in (1:length(compCohort1))){
  compCohort1[[i]] <- compCohort1[[i]] %>% filter(index_date>=cohort_start_date & index_date <=cohort_end_date)
}

for (i in (1:length(compCohort2))){
  compCohort2[[i]] <- compCohort2[[i]] %>% filter(index_date>=cohort_start_date & index_date <=cohort_end_date)
}

for (i in (1:length(targetCohort))){
  AttritionReportRQ3 <- AttritionReportRQ3 %>%
    union_all(
      tibble(
        reason_id = 3,
        cohort_definition = "Target cohort",
        number_records = targetCohort[[i]] %>% tally() %>% pull(),
        number_subjects = targetCohort[[i]] %>% distinct(subject_id) %>% tally() %>% pull(),
        reason = "Excluding based on index must lie within cohort start and cohort end date",
        period = i
      )
    )
}

for (i in (1:length(compCohort1))){
  AttritionReportRQ3 <- AttritionReportRQ3 %>%
    union_all(
      tibble(
        reason_id = 3,
        cohort_definition = "Comparator Cohort 1",
        number_records = compCohort1[[i]] %>% tally() %>% pull(),
        number_subjects = compCohort1[[i]] %>% distinct(subject_id) %>% tally() %>% pull(),
        reason = "Excluding based on index must lie within cohort start and cohort end date",
        period = i
      )
    )
}

for (i in (1:length(compCohort2))){
  AttritionReportRQ3 <- AttritionReportRQ3 %>%
    union_all(
      tibble(
        reason_id = 3,
        cohort_definition = "Comparator Cohort 2",
        number_records = compCohort2[[i]] %>% tally() %>% pull(),
        number_subjects = compCohort2[[i]] %>% distinct(subject_id) %>% tally() %>% pull(),
        reason = "Excluding based on index must lie within cohort start and cohort end date",
        period = i
      )
    )
}

### Exclusion based on prior cancer 
for (i in (1:length(targetCohort))){
  targetCohort[[i]] <- noCancerPriorOrOnIndex(targetCohort[[i]])
}

for (i in (1:length(compCohort1))){
  compCohort1[[i]] <- noCancerPriorOrOnIndex(compCohort1[[i]])
}

for (i in (1:length(compCohort2))){
  compCohort2[[i]] <- noCancerPriorOrOnIndex(compCohort2[[i]])
}

for (i in (1:length(targetCohort))){
  AttritionReportRQ3 <- AttritionReportRQ3 %>%
    union_all(
      tibble(
        reason_id = 4 ,
        cohort_definition = "Target cohort",
        number_records = targetCohort[[i]] %>% tally() %>% pull(),
        number_subjects = targetCohort[[i]] %>% distinct(subject_id) %>% tally() %>% pull(),
        reason = "Excluding based on cancer dates before index date",
        period = i
      )
    )
}

for (i in (1:length(compCohort1))){
  AttritionReportRQ3 <- AttritionReportRQ3 %>%
    union_all(
      tibble(
        reason_id = 4,
        cohort_definition = "Comparator Cohort 1",
        number_records = compCohort1[[i]] %>% tally() %>% pull(),
        number_subjects = compCohort1[[i]] %>% distinct(subject_id) %>% tally() %>% pull(),
        reason = "Excluding based on cancer dates before index date",
        period = i
      )
    )
}

for (i in (1:length(compCohort2))){
  AttritionReportRQ3 <- AttritionReportRQ3 %>%
    union_all(
      tibble(
        reason_id = 4,
        cohort_definition = "Comparator Cohort 2",
        number_records = compCohort2[[i]] %>% tally() %>% pull(),
        number_subjects = compCohort2[[i]] %>% distinct(subject_id) %>% tally() %>% pull(),
        reason = "Excluding based on cancer dates before index date",
        period = i
      )
    )
}

### Exclusion based on prior bone disease
for (i in (1:length(targetCohort))){
  targetCohort[[i]] <- noBoneDiseasePriorOrOnIndex(targetCohort[[i]])
}

for (i in (1:length(compCohort1))){
  compCohort1[[i]] <- noBoneDiseasePriorOrOnIndex(compCohort1[[i]])
}

for (i in (1:length(compCohort2))){
  compCohort2[[i]] <- noBoneDiseasePriorOrOnIndex(compCohort2[[i]])
}

for (i in (1:length(targetCohort))){
  AttritionReportRQ3 <- AttritionReportRQ3 %>%
    union_all(
      tibble(
        reason_id = 5,
        cohort_definition = "Target cohort",
        number_records = targetCohort[[i]] %>% tally() %>% pull(),
        number_subjects = targetCohort[[i]] %>% distinct(subject_id) %>% tally() %>% pull(),
        reason = "Excluding based on bone disease dates before index date",
        period = i
      )
    )
}

for (i in (1:length(compCohort1))){
  AttritionReportRQ3 <- AttritionReportRQ3 %>%
    union_all(
      tibble(
        reason_id = 5,
        cohort_definition = "Comparator Cohort 1",
        number_records = compCohort1[[i]] %>% tally() %>% pull(),
        number_subjects = compCohort1[[i]] %>% distinct(subject_id) %>% tally() %>% pull(),
        reason = "Excluding based on bone disease dates before index date",
        period = i
      )
    )
}

for (i in (1:length(compCohort2))){
  AttritionReportRQ3 <- AttritionReportRQ3 %>%
    union_all(
      tibble(
        reason_id = 5,
        cohort_definition = "Comparator Cohort 2",
        number_records = compCohort2[[i]] %>% tally() %>% pull(),
        number_subjects = compCohort2[[i]] %>% distinct(subject_id) %>% tally() %>% pull(),
        reason = "Excluding based on bone disease dates before index date",
        period = i
      )
    )
}

### Exclusion based on prior obs
for (i in (1:length(targetCohort))){
  targetCohort[[i]] <-targetCohort[[i]] %>% 
    left_join(cdm[["observation_period"]], by = c("subject_id" = "person_id"), copy = T) %>% 
    select(subject_id:fracture_site, observation_period_start_date, observation_period_end_date) %>%
    mutate(days_prior_obs = index_date - observation_period_start_date, days_after_obs = observation_period_end_date - index_date) %>%
    filter(days_prior_obs >= prior_observation, days_after_obs >= 0) %>%
    select(subject_id, cohort_start_date, cohort_end_date, cohort_interval, period_start, period_end, condition_concept_id, index_date, fracture_site)
  
}

for (i in (1:length(compCohort1))){
  compCohort1[[i]] <-compCohort1[[i]] %>% 
    left_join(cdm[["observation_period"]], by = c("subject_id" = "person_id"), copy = T) %>% 
    select(subject_id:fracture_site, observation_period_start_date, observation_period_end_date) %>%
    mutate(days_prior_obs = index_date - observation_period_start_date, days_after_obs = observation_period_end_date - index_date) %>%
    filter(days_prior_obs >= prior_observation, days_after_obs >= 0) %>%
    select(subject_id, cohort_start_date, cohort_end_date, cohort_interval, period_start, period_end, condition_concept_id, index_date, fracture_site)
}

for (i in (1:length(compCohort2))){
  compCohort2[[i]] <-compCohort2[[i]] %>% 
    left_join(cdm[["observation_period"]], by = c("subject_id" = "person_id"), copy = T) %>% 
    select(subject_id:index_date, observation_period_start_date, observation_period_end_date) %>%
    mutate(days_prior_obs = index_date - observation_period_start_date, days_after_obs = observation_period_end_date - index_date) %>%
    filter(days_prior_obs >= prior_observation, days_after_obs >= 0) %>%
    select(subject_id, cohort_start_date, cohort_end_date, cohort_interval, period_start, period_end, index_date)
}

for (i in (1:length(targetCohort))){
  AttritionReportRQ3 <- AttritionReportRQ3 %>%
    union_all(
      tibble(
        reason_id = 6,
        cohort_definition = "Target cohort",
        number_records = targetCohort[[i]] %>% tally() %>% pull(),
        number_subjects = targetCohort[[i]] %>% distinct(subject_id) %>% tally() %>% pull(),
        reason = "Excluding based prior observation before index date",
        period = i
      )
    )
}

for (i in (1:length(compCohort1))){
  AttritionReportRQ3 <- AttritionReportRQ3 %>%
    union_all(
      tibble(
        reason_id = 6,
        cohort_definition = "Comparator Cohort 1",
        number_records = compCohort1[[i]] %>% tally() %>% pull(),
        number_subjects = compCohort1[[i]] %>% distinct(subject_id) %>% tally() %>% pull(),
        reason = "Excluding based prior observation before index date",
        period = i
      )
    )
}

for (i in (1:length(compCohort2))){
  AttritionReportRQ3 <- AttritionReportRQ3 %>%
    union_all(
      tibble(
        reason_id = 6,
        cohort_definition = "Comparator Cohort 2",
        number_records = compCohort2[[i]] %>% tally() %>% pull(),
        number_subjects = compCohort2[[i]] %>% distinct(subject_id) %>% tally() %>% pull(),
        reason = "Excluding based prior observation before index date",
        period = i
      )
    )
}

write.xlsx(AttritionReportRQ3, file = here::here(output_folder, "AttritionReportRQ3.xlsx"))
