# Creating follow up time
info(logger, "CREATING FOLLOW UP TIME: FOLLOWUPEND")
fracture_table_follow_up <- fracture_table

# 730 days after the index date
fracture_table_follow_up <- addInTwoYearsAfter(fracture_table_follow_up)

# End of data collection (assuming each person has only one observation period)
fracture_table_follow_up <- addInObsEndDate(fracture_table_follow_up)

# Adding first cancer date after the index date
fracture_table_follow_up <- addInCancerPostIndex(fracture_table_follow_up)

# Adding first bone disease date after the index date
fracture_table_follow_up <- addInBoneDiseasePostIndex(fracture_table_follow_up)
  
# Add in first fracture date after the index dates
fracture_table_follow_up <- addInNextFracture(fracture_table_follow_up)

# Add in death date after the index date 
fracture_table_follow_up <- addInDeath(fracture_table_follow_up)

# Add in FOLLOWUPEND
fracture_table_follow_up <- addInFollowUpEnd(fracture_table_follow_up)

### Relevant counts
fracture_table_follow_up_back_up <- fracture_table_follow_up

counts <- 
    tibble(
      number_records = fracture_table_follow_up_back_up %>% tally() %>% pull(),
      number_subjects = fracture_table_follow_up_back_up %>% distinct(subject_id) %>% tally() %>% pull(),
      cohort_number_records = fracture_table_follow_up_back_up %>% filter(follow_up_time < 730) %>% tally() %>% pull(),
      cohort_number_subjects = fracture_table_follow_up_back_up %>% filter(follow_up_time < 730) %>% distinct(subject_id) %>% tally() %>% pull(),
      cohort_number_fracture_records = fracture_table_follow_up_back_up %>% filter(follow_up_time < 730 & follow_up_end == fracture_after_index) %>% tally() %>% pull(),
      cohort_number_fracture_subjects = fracture_table_follow_up_back_up %>% filter(follow_up_time < 730 & follow_up_end == fracture_after_index) %>% distinct(subject_id) %>% tally() %>% pull(),
      zero_days = fracture_table_follow_up_back_up %>% filter (follow_up_time == 0) %>% distinct(subject_id) %>% tally() %>% pull()
    )
  
patientID <- list()
zeroPatientID <- list()
reEntryTable <- list()

while (nrow(fracture_table_follow_up_back_up) > 0){
  reEntryTable[[nrow(fracture_table_follow_up_back_up)]] <- fracture_table_follow_up_back_up %>% filter(follow_up_time > 0)
  patientID[[nrow(fracture_table_follow_up_back_up)]] <- fracture_table_follow_up_back_up %>% filter(follow_up_time > 0) %>% distinct(subject_id) %>% pull()
  zeroPatientID[[nrow(fracture_table_follow_up_back_up)]] <- fracture_table_follow_up_back_up %>% filter(follow_up_time == 0) %>% distinct(subject_id) %>% pull()
  fracture_table_follow_up_back_up <- fracture_table_follow_up_back_up %>% filter(follow_up_time > 0)
  fracture_table_follow_up_back_up <- nextFractureClean(fracture_table_follow_up_back_up)
  fracture_table_follow_up_back_up <- addIndex(fracture_table_follow_up_back_up)
  fracture_table_follow_up_back_up <- noDeathOnIndex(fracture_table_follow_up_back_up)
  fracture_table_follow_up_back_up <- noCancerPriorOrOnIndex(fracture_table_follow_up_back_up)
  fracture_table_follow_up_back_up <- noBoneDiseasePriorOrOnIndex(fracture_table_follow_up_back_up)
  fracture_table_follow_up_back_up <- addInTwoYearsAfter(fracture_table_follow_up_back_up)
  fracture_table_follow_up_back_up <- addInObsEndDate(fracture_table_follow_up_back_up)
  fracture_table_follow_up_back_up <- addInCancerPostIndex(fracture_table_follow_up_back_up)
  fracture_table_follow_up_back_up <- addInBoneDiseasePostIndex(fracture_table_follow_up_back_up)
  fracture_table_follow_up_back_up <- addInNextFracture(fracture_table_follow_up_back_up)
  fracture_table_follow_up_back_up <- addInDeath(fracture_table_follow_up_back_up)
  fracture_table_follow_up_back_up <- addInFollowUpEnd(fracture_table_follow_up_back_up)
  fracture_table_follow_up_back_up <- fracture_table_follow_up_back_up %>% ungroup()
  
  counts <- counts %>%
    union_all(
      tibble(
        number_records = fracture_table_follow_up_back_up %>% tally() %>% pull(),
        number_subjects = fracture_table_follow_up_back_up %>% distinct(subject_id) %>% tally() %>% pull(),
        cohort_number_records = fracture_table_follow_up_back_up %>% filter(follow_up_time < 730) %>% tally() %>% pull(),
        cohort_number_subjects = fracture_table_follow_up_back_up %>% filter(follow_up_time < 730) %>% distinct(subject_id) %>% tally() %>% pull(),
        cohort_number_fracture_records = fracture_table_follow_up_back_up %>% filter(follow_up_time < 730 & follow_up_end == fracture_after_index) %>% tally() %>% pull(),
        cohort_number_fracture_subjects = fracture_table_follow_up_back_up %>% filter(follow_up_time < 730 & follow_up_end == fracture_after_index) %>% distinct(subject_id) %>% tally() %>% pull(),
        zero_days = fracture_table_follow_up_back_up %>% filter (follow_up_time == 0) %>% distinct(subject_id) %>% tally() %>% pull()
      )
    )
}

patientID <- patientID[!sapply(patientID, is.null)]
zeroPatientID <- zeroPatientID[!sapply(zeroPatientID, is.null)]
reEntryTable <- reEntryTable[!sapply(reEntryTable, is.null)]

ObservedTime <- list()

for (i in (1:length(reEntryTable))){
  ObservedTime[[i]] <- reEntryTable[[i]] %>% 
    select(subject_id, follow_up_time) %>% 
    distinct() %>% 
    summarise(total_time = sum(follow_up_time))
}

totalObservedTime <- 0
for (i in (1:length(ObservedTime))){
  totalObservedTime <- totalObservedTime + ObservedTime[[i]]
}

totalObservedTimeYears <- as.integer(totalObservedTime)/365.25

for (i in (1:length(reEntryTable))){
  reEntryTable[[i]] <- reEntryTable[[i]] %>% mutate(fracture_in_obs = as.integer(condition_start_date==follow_up_end)*as.integer(condition_start_date>index_date))  
}

totalFracture <- 0
for (i in (1:length(reEntryTable))){
  totalFracture <- totalFracture + sum(reEntryTable[[i]]$fracture_in_obs)
}

inc_results <- tibble(fracture = totalFracture, 
                      py = totalObservedTimeYears)

confidenceInterval <- PoissonCI(x=totalFracture, n=totalObservedTimeYears, method = "exact")
confidenceInterval <- as.data.frame(confidenceInterval) %>% mutate(est = round(est * 1000,2), lower = round(lwr.ci * 1000, 2), upper = round(upr.ci*1000,2)) %>% select(c(-lwr.ci, -upr.ci))
inc_results <- cbind(inc_results, confidenceInterval, tibble(database_name=db_name)) 

