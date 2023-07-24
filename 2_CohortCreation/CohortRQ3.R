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

### Create a cohort of women only, age above 50 between 2010/04/01 and 2018/03/31
info(logger, "CREATING DENOMINATOR - WOMEN WHO ARE ABOVE 50 WITHIN THE STUDY PERIOD")
cdm <- generateDenominatorCohortSet(
  cdm,
  name = "denominator",
  cohortDateRange = c(study_start_date, study_end_date),
  ageGroup = list(c(50, 150)))

### cohort with all records of fracture 
info(logger, "COLLECTING ALL RECORDS OF FRACTURES FROM DENOMINATORS")
cdm[["fracture"]] <- cdm[["denominator"]] %>% 
  left_join(cdm[["condition_occurrence"]], by = c("subject_id" = "person_id")) %>%
  filter(condition_concept_id %in% any_fracture_id) %>%
  select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date) %>%
  mutate(cohort_start_date = as.Date(as.character(cohort_start_date))) %>%
  mutate(cohort_end_date = as.Date(as.character(cohort_end_date))) %>%
  mutate(fracture_site = if (condition_concept_id %in% hip_fracture_id) {
    "Hip"
  }
  else if (condition_concept_id %in% femur_fracture_id) {
    "Femur"
  }
  else if (condition_concept_id %in% foot_fracture_id) {
    "Foot"
  }
  else if (condition_concept_id %in% tib_fracture_id) {
    "Tibia and Fibula"
  }
  else if (condition_concept_id %in% rib_fracture_id) {
    "Rib"
  }
  else if (condition_concept_id %in% forearm_fracture_id) {
    "Forearm"
  }
  else if (condition_concept_id %in% vert_fracture_id) {
    "Vertebra"
  }
  else if (condition_concept_id %in% pelvic_fracture_id) {
    "Pelvic"
  }  
  else if (condition_concept_id %in% humerus_fracture_id) {
    "Humerus"
  } 
  else if (condition_concept_id %in% nonspecific_fracture_id) {
    "Nonspecific"
  }
  )

fracture_table <- cdm[["fracture"]] %>% collect()

AttritionReportRQ3<-tibble(
  cohort_definition_id = as.integer(1),
  number_records = fracture_table %>% tally() %>% pull(),
  number_subjects = fracture_table %>% distinct(subject_id) %>% tally() %>% pull(),
  reason = "Starting Population"
)

### Removing fractures before the birth year
info(logger, "REMOVING FRACTURES BEFORE THE BIRTH YEAR")
fracture_table <- fracture_table %>%
  left_join(cdm[["person"]], by = c("subject_id" = "person_id"), copy = T) %>%
  mutate(fracture_year = as.numeric(format(condition_start_date, "%Y"))) %>%
  filter(fracture_year >= year_of_birth) %>%
  select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date, fracture_site)

AttritionReportRQ3<- AttritionReportRQ3 %>% 
  union_all(  
    tibble(
      cohort_definition_id = as.integer(1),
      number_records = fracture_table %>% tally() %>% pull(),
      number_subjects = fracture_table %>% distinct(subject_id) %>% tally() %>% pull(),
      reason = "fracture happening before the birth year"
    )
  ) 

### Removing the fractures that happen on the same day as a trauma and applying hierarchy 
info(logger, "REMOVING FRACTURES THAT HAPPEN ON THE SAME DAY AS A TRAUMA")
fracture_table <- fracture_table %>%
  anti_join(cdm[["condition_occurrence"]] %>% filter(condition_concept_id %in% trauma_condition), by = c("subject_id" = "person_id", "condition_start_date"), copy = T)

fracture_table <- fracture_table %>% 
  anti_join(cdm[["observation"]] %>% filter(observation_concept_id %in% trauma_observation), by = c("subject_id" = "person_id", "condition_start_date" = "observation_date"), copy = T) 

fracture_table<-fracture_table %>%
  anti_join(
    fracture_table %>% filter(!fracture_site=="Nonspecific") %>% group_by(subject_id, condition_start_date) %>% summarise(number_site = n_distinct(fracture_site)) %>% filter(number_site>=3),
    by = c("subject_id", "condition_start_date")
  )

AttritionReportRQ3<- AttritionReportRQ3 %>% 
  union_all(  
    tibble(
      cohort_definition_id = as.integer(1),
      number_records = fracture_table %>% tally() %>% pull(),
      number_subjects = fracture_table %>% distinct(subject_id) %>% tally() %>% pull(),
      reason = "fracture happening on the same day as a trauma code"
    )
  ) 

### Washout and removing re-recordings
info(logger, "APPLYING WASHOUT PERIOD")
sites <- c("Hip", "Femur", "Pelvic", "Vertebra", "Humerus", "Forearm", "Tibia and Fibula", "Rib", "Foot", "Nonspecific")

fracture_table_back_up <- fracture_table
index_fractures <- tibble()

while(nrow(fracture_table_back_up)>0){
  fracture_correction <- list()
  for (i in (1:10)){
    fracture_correction[[sites[i]]] <- fracture_table_back_up %>% 
      group_by(subject_id) %>% 
      filter(fracture_site == sites[[i]] | fracture_site == sites[[10]]) %>% 
      summarise(min_date = min(condition_start_date, na.rm =T)) %>% 
      mutate(site = sites[[i]])
  }
  
  fracture_correction_nonspecific <- list()
  
  for (i in (1:10)){
    fracture_correction_nonspecific <- rbind(fracture_correction_nonspecific, fracture_correction[[i]])
  }
  
  fracture_table_back_up <- fracture_table_back_up %>% 
    left_join(fracture_correction_nonspecific, by = c("subject_id", "fracture_site" = "site")) %>% # compute min date
    mutate(gap_to_min_date = condition_start_date - min_date) %>%
    filter(gap_to_min_date == 0 | gap_to_min_date > washout_period) %>%
    group_by(subject_id, condition_start_date, fracture_site) %>%
    arrange(condition_concept_id, .by_group = TRUE) %>% 
    filter(row_number()==1) %>%
    ungroup()
  
  fracture_table_back_up <- fracture_table_back_up %>%
    left_join(fracture_table_back_up %>% group_by(subject_id) %>% filter (gap_to_min_date == 0) %>% count(), by = "subject_id") %>%
    filter (!((n>1) & (gap_to_min_date==0) & (fracture_site=="Nonspecific")))
  
  index_fractures <- rbind(index_fractures, fracture_table_back_up %>% filter(gap_to_min_date==0) %>% select(-min_date, -gap_to_min_date, -n))
  
  fracture_table_back_up <- fracture_table_back_up %>% 
    filter(!gap_to_min_date==0) %>%
    select(-min_date, -gap_to_min_date, -n)
}

fracture_table <- index_fractures

AttritionReportRQ3<- AttritionReportRQ3 %>% 
  union_all(  
    tibble(
      cohort_definition_id = as.integer(1),
      number_records = fracture_table %>% tally() %>% pull(),
      number_subjects = fracture_table %>% distinct(subject_id) %>% tally() %>% pull(),
      reason = "Clean out using washout period"
    )
  ) 

# Applying Hierarchy to multiple records on the same day
info(logger, "APPLYING HIERARCHY TO PREVENT MORE THAN ONE RECORD ON THE SAME DAY FOR THE SAME PERSON")

fracture_table$fracture_site<-factor(fracture_table$fracture_site, levels = sites)

fracture_table <- fracture_table %>%
  group_by(subject_id, condition_start_date) %>%
  arrange(fracture_site, .by_group = TRUE) %>% 
  filter(row_number()==1) %>%
  ungroup()

AttritionReportRQ3<- AttritionReportRQ3 %>% 
  union_all(  
    tibble(
      cohort_definition_id = as.integer(1),
      number_records = fracture_table %>% tally() %>% pull(),
      number_subjects = fracture_table %>% distinct(subject_id) %>% tally() %>% pull(),
      reason = "Excluding records on the same day for the same person according to hierarchy"
    )
  )

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

