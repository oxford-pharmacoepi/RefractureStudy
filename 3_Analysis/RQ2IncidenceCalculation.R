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

# Add in immFracture
fracture_table_follow_up <- immFracture(fracture_table_follow_up)

fracture_table_follow_up_back_up <- fracture_table_follow_up

reverseEntryTable <- list()

while (nrow(fracture_table_follow_up_back_up) > 0){
  reverseEntryTable[[nrow(fracture_table_follow_up_back_up)]] <- fracture_table_follow_up_back_up %>% filter(follow_up_time > 0)
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
  fracture_table_follow_up_back_up <- immFracture(fracture_table_follow_up_back_up)
}

reverseEntryTable <- reverseEntryTable[!sapply(reverseEntryTable, is.null)]

#entryTable serves as a table recording people who entered the cohort at least once, twice etc..
entryTable <- list()
for (i in (1:length(reverseEntryTable))){
  entryTable[[i]]<-reverseEntryTable[[length(reverseEntryTable)+1-i]]
}

ObservedTime <- list()

for (i in (1:length(entryTable))){
  ObservedTime[[i]] <- entryTable[[i]] %>% 
    select(subject_id, follow_up_time) %>% 
    distinct() %>% 
    summarise(total_time = sum(follow_up_time))
}

totalObservedTime <- 0
for (i in (1:length(ObservedTime))){
  totalObservedTime <- totalObservedTime + ObservedTime[[i]]
}

totalObservedTimeYears <- as.integer(totalObservedTime)/365.25

totalFracture <- 0
for (i in (1:length(reverseEntryTable))){
  totalFracture <- totalFracture + sum(entryTable[[i]]$imminentFracture)
}

inc_results <- tibble(fracture = totalFracture, 
                      py = totalObservedTimeYears)

confidenceInterval <- PoissonCI(x=totalFracture, n=totalObservedTimeYears/1000, method = "exact")
confidenceInterval <- as.data.frame(confidenceInterval) %>% mutate(est = round(est,2), lower = round(lwr.ci, 2), upper = round(upr.ci,2)) %>% select(c(-lwr.ci, -upr.ci))
inc_results <- cbind(inc_results, tibble(n_person = entryTable[[1]] %>% distinct(subject_id) %>% tally() %>% pull()), confidenceInterval, tibble(database_name=db_name)) 

### Stratifying by the number of times someone enters the cohort
stratifiedCohort <- list()
for (i in (1:(length(entryTable)-1))){
  stratifiedCohort[[i]] <- entryTable[[i]] %>% anti_join(entryTable[[i+1]], by = "subject_id")
}

stratifiedCohort[[length(entryTable)]] <- entryTable[[length(entryTable)]]

stratifiedTime <- c()
for (i in (1: length(stratifiedCohort))){
  stratifiedTime[[i]] <- stratifiedCohort[[i]] %>% 
    select(subject_id, follow_up_time) %>% 
    distinct() %>% 
    summarise(total_time = sum(follow_up_time))
  stratifiedTime[[i]]<-as.integer(stratifiedTime[[i]] %>% pull())/365.25
}

stratifiedFractures <- c()
for (i in (1:length(stratifiedCohort))){
  stratifiedFractures[[i]] <- sum(stratifiedCohort[[i]]$imminentFracture)
}

stratifiedIncidenceResults <- list()
for (i in (1:length(stratifiedCohort))){
  stratifiedIncidenceResults[[i]] <- tibble(fracture = stratifiedFractures[[i]], py = stratifiedTime[[i]], tibble(n_person = stratifiedCohort[[i]] %>% distinct(subject_id) %>% tally() %>% pull()))
  stratifiedIncidenceResults[[i]] <- cbind(as.data.frame(PoissonCI(x=stratifiedFractures[[i]], n=stratifiedTime[[i]]/1000, method = "exact")) %>% mutate(est = round(est,2), lower = round(lwr.ci, 2), upper = round(upr.ci,2)) %>% select(c(-lwr.ci, -upr.ci)), stratifiedIncidenceResults[[i]], tibble(database_name=db_name), tibble(cohort_entering_counter = i))
}

stratifiedIncidenceResultsTable <- data.frame()
for (i in (1:length(stratifiedIncidenceResults))){
  stratifiedIncidenceResultsTable<-rbind(stratifiedIncidenceResultsTable, stratifiedIncidenceResults[[i]])
}

# obscuring according to min cell count
stratifiedIncidenceResultsTable <- stratifiedIncidenceResultsTable %>% 
  mutate(obscured = n_person < minimum_counts)

stratifiedIncidenceResultsTable <- stratifiedIncidenceResultsTable %>%
  mutate(est = ifelse(obscured == T, NA, est),
         lower = ifelse(obscured == T, NA, lower),
         upper = ifelse(obscured == T, NA, upper),
         fracture = ifelse(obscured == T, NA, fracture),
         py = ifelse(obscured == T, NA, py),
         n_person = ifelse(obscured == T, NA, n_person))

# Export Incidence Results
write.xlsx(inc_results, file = here::here(output_folder, "IncidenceResults.xlsx"))
write.xlsx(stratifiedIncidenceResultsTable, file = here::here(output_folder, "StratifiedIncidenceResults.xlsx"))

### cumulative incidence function
