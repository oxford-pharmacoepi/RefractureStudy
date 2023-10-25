fracture_table_rq2_index <- fracture_table

### Exclusion criteria
# no fractures 730 prior
info(logger, "EXCLUDING FRACTURE RECORDS THAT HAS ANOTHER FRACTURE WHICH HAPPENED WITHIN 2 YEARS BEFORE THE INDEX DATE")
fracture_table_rq2_index <- fracture_table_rq2_index %>%
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
      number_records = fracture_table_rq2_index %>% tally() %>% pull(),
      number_subjects = fracture_table_rq2_index %>% distinct(subject_id) %>% tally() %>% pull(),
      reason = "Excluding records that has at least one fracture 730 days prior"
    )
  ) 

# at least 50
fracture_table_rq2_index <- fracture_table_rq2_index %>% 
  dplyr::right_join(cdm[["person"]], by = c("subject_id" = "person_id"), copy = T) %>%
  dplyr::mutate(age_fracture = lubridate::year(condition_start_date) - year_of_birth) %>%
  dplyr::filter(age_fracture >= 50) %>%
  dplyr::select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date, fracture_site)

AttritionReportRQ2<- AttritionReportRQ2 %>% 
  union_all(  
    tibble(
      cohort_definition_id = as.integer(1),
      number_records = fracture_table_rq2_index %>% tally() %>% pull(),
      number_subjects = fracture_table_rq2_index %>% distinct(subject_id) %>% tally() %>% pull(),
      reason = "Excluding records before the subject turning 50"
    )
  )

# At least 730 days prior obs
info(logger, "EXCLUDING INDIVIDUALS WHO DO NOT HAVE SUFFICIENT PRIOR OBSERVATION")
fracture_table_rq2_index <-fracture_table_rq2_index %>% 
  left_join(cdm[["observation_period"]], by = c("subject_id" = "person_id"), copy = T) %>% 
  select(subject_id:fracture_site, observation_period_start_date, observation_period_end_date) %>%
  mutate(days_prior_obs = condition_start_date - observation_period_start_date, days_after_obs = observation_period_end_date - condition_start_date) %>%
  filter(days_prior_obs >= prior_observation, days_after_obs >= 0) %>%
  select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date, fracture_site)

AttritionReportRQ2<- AttritionReportRQ2 %>% 
  union_all(  
    tibble(
      cohort_definition_id = as.integer(1),
      number_records = fracture_table_rq2_index %>% tally() %>% pull(),
      number_subjects = fracture_table_rq2_index %>% distinct(subject_id) %>% tally() %>% pull(),
      reason = "Excluding records with insufficient prior observation"
    )
  ) 

# No records of death on the index date
info(logger, "EXCLUDING INDIVIDUALS WHO HAS A RECORD OF DEATH ON THE SAME DAY AS THE INDEX DATE")

fracture_table_rq2_index <- fracture_table_rq2_index %>% 
  dplyr::anti_join(cdm[["death"]], by = c("subject_id" = "person_id", "condition_start_date" = "death_date"), copy = T)

AttritionReportRQ2<- AttritionReportRQ2 %>% 
  union_all(  
    tibble(
      cohort_definition_id = as.integer(1),
      number_records = fracture_table_rq2_index %>% tally() %>% pull(),
      number_subjects = fracture_table_rq2_index %>% distinct(subject_id) %>% tally() %>% pull(),
      reason = "Excluding records on the same day as death"
    )
  ) 

# No records of cancer before or on the index date
info(logger, "EXCLUDING INDIVIDUALS WHO HAS A RECORD OF CANCER OF INTEREST BEFORE THE INDEX DATE")

fracture_table_rq2_index <- 
  fracture_table_rq2_index %>% anti_join(fracture_table_rq2_index %>% 
                                     dplyr::inner_join(cdm[["cancer"]] %>% dplyr::select(subject_id, cancer_date), by = "subject_id", copy = T, relationship = "many-to-many") %>%
                                     dplyr::filter(cancer_date<=condition_start_date) %>%
                                     dplyr::distinct() %>%
                                     dplyr::compute(), by = colnames(fracture_table_rq2_index))

AttritionReportRQ2<- AttritionReportRQ2 %>% 
  union_all(  
    tibble(
      cohort_definition_id = as.integer(1),
      number_records = fracture_table_rq2_index %>% tally() %>% pull(),
      number_subjects = fracture_table_rq2_index %>% distinct(subject_id) %>% tally() %>% pull(),
      reason = "Excluding records happened after cancer"
    )
  ) 

# No records of metabolic bone disease
info(logger, "EXCLUDING INDIVIDUALS WHO HAS A RECORD OF METABOLIC BONE DISEASE OF INTEREST BEFORE THE INDEX DATE")

fracture_table_rq2_index <- 
  fracture_table_rq2_index %>% anti_join(fracture_table_rq2_index %>% 
                                     dplyr::inner_join(cdm[["mbd"]] %>% dplyr::select(subject_id, mbd_date), by = "subject_id", copy = T, relationship = "many-to-many") %>%
                                     dplyr::filter(mbd_date<=condition_start_date) %>%
                                     dplyr::distinct() %>%
                                     dplyr::compute(), by = colnames(fracture_table_rq2_index))

AttritionReportRQ2<- AttritionReportRQ2 %>% 
  union_all(  
    tibble(
      cohort_definition_id = as.integer(1),
      number_records = fracture_table_rq2_index %>% tally() %>% pull(),
      number_subjects = fracture_table_rq2_index %>% distinct(subject_id) %>% tally() %>% pull(),
      reason = "Excluding records happened after cancer"
    )
  )  

# Excluding individuals who has index date same as obs period end date
info(logger, "EXCLUDING INDIVIDUALS WHO HAS INDEX DATE ON THE SAME DATE AS THE OBSERVATION PERIOD END DATE")

fracture_table_rq2_index <- fracture_table_rq2_index %>%
  dplyr::anti_join(cdm[["observation_period"]], by = c("subject_id" = "person_id", "condition_start_date" = "observation_period_end_date"), copy = T)

AttritionReportRQ2<- AttritionReportRQ2 %>% 
  union_all(  
    tibble(
      cohort_definition_id = as.integer(1),
      number_records = fracture_table_rq2_index %>% tally() %>% pull(),
      number_subjects = fracture_table_rq2_index %>% distinct(subject_id) %>% tally() %>% pull(),
      reason = "Excluding records that happen on the last day of observation period"
    )
  ) 

# restrict the fractures to the study period
info(logger, "EXCLUDING RECORDS THAT HAPPENED OUTSIDE OF STUDY PERIOD")

fracture_table_rq2_index <- fracture_table_rq2_index %>% 
  dplyr::filter(condition_start_date<=cohort_end_date, condition_start_date>=cohort_start_date) %>%
  dplyr::mutate(class = "index")

fracture_table_rq2_index_ids <- fracture_table_rq2_index %>% pull(subject_id)

AttritionReportRQ2<- AttritionReportRQ2 %>% 
  union_all(  
    tibble(
      cohort_definition_id = as.integer(1),
      number_records = fracture_table_rq2_index %>% tally() %>% pull(),
      number_subjects = fracture_table_rq2_index %>% distinct(subject_id) %>% tally() %>% pull(),
      reason = "Excluding records that happen outside of study period"
    )
  ) 
info(logger, "PULLING OUT POSSIBLE INDEX DATES IS COMPLETED")

### Finalise attrition
AttritionReportRQ2 <- AttritionReportRQ2 %>% 
  dplyr::mutate(subjects_excluded = -(number_subjects-lag(number_subjects)), records_excluded = -(number_records - lag(number_records)))

AttritionReportRQ2 <- AttritionReportRQ2 %>%
  dplyr::mutate(masked_records = ifelse((records_excluded<5 & records_excluded>0), "<5", as.integer(.data$records_excluded)),
                masked_subjects = ifelse((subjects_excluded<5 & subjects_excluded>0), "<5", as.integer(.data$subjects_excluded))) %>%
  dplyr::select(-c("records_excluded", "subjects_excluded"))

AttritionReport <- rbind(AttritionReportDenom %>% dplyr::select(number_subjects, reason),
              AttritionReportRQ2 %>% dplyr::select(number_subjects, reason)) %>% 
  dplyr::mutate(subjects_excluded = -(number_subjects-lag(number_subjects))) %>%
  dplyr::mutate(masked_subjects_excluded = ifelse((subjects_excluded<5 & subjects_excluded>0), "<5", as.integer(.data$subjects_excluded))) %>%
  dplyr::select(-"subjects_excluded")
  
write.xlsx(AttritionReport, file = here::here(output_folder, "AttritionReport.xlsx"))

###
fracture_table_rq2 <- fracture_table

fracture_table_rq2 <- fracture_table_rq2 %>%
  dplyr::left_join(fracture_table_rq2_index, by = colnames(fracture_table_rq2)) %>%
  dplyr::filter(subject_id %in% fracture_table_rq2_index_ids)

fracture_table_rq2 <- addIndex(fracture_table_rq2)

info(logger, "CREATING FOLLOWUPEND AND ENTRIES")

# 730 days after the index date
fracture_table_rq2 <- addInTwoYearsAfter(fracture_table_rq2)

# End of data collection (assuming each person has only one observation period)
fracture_table_rq2 <- addInObsEndDate(fracture_table_rq2)

# Adding first cancer date after the index date
fracture_table_rq2 <- addInCancerPostIndex(fracture_table_rq2)

# Adding first bone disease date after the index date
fracture_table_rq2 <- addInBoneDiseasePostIndex(fracture_table_rq2)

# Add in first fracture date after the index dates
fracture_table_rq2 <- addInNextFracture(fracture_table_rq2)

# Add in death date after the index date 
fracture_table_rq2 <- addInDeath(fracture_table_rq2)

# Add in FOLLOWUPEND
fracture_table_rq2 <- addInFollowUpEnd(fracture_table_rq2)

# Add in immFracture
fracture_table_rq2 <- immFracture(fracture_table_rq2)

fracture_table_rq2_back_up <- fracture_table_rq2

reverseEntryTable <- list()

while (nrow(fracture_table_rq2_back_up) > 0){
  reverseEntryTable[[nrow(fracture_table_rq2_back_up)]] <- fracture_table_rq2_back_up %>% dplyr::filter(follow_up_time > 0)
  fracture_table_rq2_back_up <- fracture_table_rq2_back_up %>% dplyr::filter(follow_up_time > 0)
  fracture_table_rq2_back_up <- nextFractureClean(fracture_table_rq2_back_up)
  fracture_table_rq2_back_up <- addIndex(fracture_table_rq2_back_up)
  fracture_table_rq2_back_up <- addInTwoYearsAfter(fracture_table_rq2_back_up)
  fracture_table_rq2_back_up <- addInObsEndDate(fracture_table_rq2_back_up)
  fracture_table_rq2_back_up <- addInCancerPostIndex(fracture_table_rq2_back_up)
  fracture_table_rq2_back_up <- addInBoneDiseasePostIndex(fracture_table_rq2_back_up)
  fracture_table_rq2_back_up <- addInNextFracture(fracture_table_rq2_back_up)
  fracture_table_rq2_back_up <- addInDeath(fracture_table_rq2_back_up)
  fracture_table_rq2_back_up <- addInFollowUpEnd(fracture_table_rq2_back_up)
  fracture_table_rq2_back_up <- fracture_table_rq2_back_up %>% ungroup()
  fracture_table_rq2_back_up <- immFracture(fracture_table_rq2_back_up)
}

reverseEntryTable <- reverseEntryTable[!sapply(reverseEntryTable, is.null)]

entryTable <- list()
for (i in (1:length(reverseEntryTable))){
  entryTable[[i]]<-reverseEntryTable[[length(reverseEntryTable)+1-i]]
}
info(logger, "CREATING FOLLOWUPEND AND ENTRIES IS COMPLETED")

stratifiedCohort <- list()
for (i in (1:(length(entryTable)-1))){
  stratifiedCohort[[i]] <- entryTable[[i]] %>% dplyr::anti_join(entryTable[[i+1]], by = "subject_id")
}

stratifiedCohort[[length(entryTable)]] <- entryTable[[length(entryTable)]]
info(logger, "CREATING FOLLOW UP TIME IS DONE")

### imminent fracture cohort
imminentFractureCohort <- list()
for (i in (1:length(stratifiedCohort))){
  imminentFractureCohort[[i]] <- stratifiedCohort[[i]] %>% 
    dplyr::filter (imminentFracture==1) %>% 
    dplyr::select(subject_id)
}

imminentFractureCohortTotal <-data.frame()
for (i in 1:length(imminentFractureCohort)){
  imminentFractureCohortTotal<-rbind(imminentFractureCohortTotal, imminentFractureCohort[[i]])
}

withoutImminentFractureCohortTotal <- entryTable[[1]] %>%
  dplyr::anti_join(imminentFractureCohortTotal, by = "subject_id") %>%
  dplyr::select(subject_id) %>%
  dplyr::distinct()
