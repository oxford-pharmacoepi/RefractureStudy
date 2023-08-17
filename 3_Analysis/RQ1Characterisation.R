# Creating follow up time
info(logger, "CREATING FOLLOW UP TIME: FOLLOWUPEND")
fracture_table_follow_up <- fracture_table_rq1

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

stratifiedCohort <- list()
for (i in (1:(length(entryTable)-1))){
  stratifiedCohort[[i]] <- entryTable[[i]] %>% anti_join(entryTable[[i+1]], by = "subject_id")
}

stratifiedCohort[[length(entryTable)]] <- entryTable[[length(entryTable)]]

### imminent fracture cohort
imminentFractureCohort <- list()
for (i in (1:length(stratifiedCohort))){
  imminentFractureCohort[[i]] <- stratifiedCohort[[i]] %>% filter (imminentFracture==1) %>% select(subject_id)
}

imminentFractureCohortTotal <-data.frame()
for (i in 1:length(imminentFractureCohort)){
  imminentFractureCohortTotal<-rbind(imminentFractureCohortTotal, imminentFractureCohort[[i]])
}

withoutImminentFractureCohortTotal <- entryTable[[1]] %>%
  anti_join(imminentFractureCohortTotal, by = "subject_id") %>%
  select(subject_id) %>%
  distinct()

#### characterisation 
info(logger, "CHARACTERISATION: DEMOGRAPHICS")
cdm[["noImminentFractureCohort"]] <- cdm[["denominator"]] %>%
  inner_join(withoutImminentFractureCohortTotal, by = "subject_id", copy = T) %>%
  left_join(entryTable[[1]] %>% select(subject_id, index_date), by = "subject_id", copy = T) %>%
  select(-cohort_start_date, -cohort_end_date) %>%
  rename(cohort_start_date = index_date) %>% 
  mutate(cohort_end_date = cohort_start_date) %>%
  distinct() %>%
  compute()

cdm[["imminentFractureCohort"]] <-cdm[["denominator"]] %>%
  inner_join(imminentFractureCohortTotal, by = "subject_id", copy = T) %>%
  left_join(entryTable[[1]] %>% select(subject_id, index_date), by = "subject_id", copy = T) %>%
  select(-cohort_start_date, -cohort_end_date) %>%
  rename(cohort_start_date = index_date) %>% 
  mutate(cohort_end_date = cohort_start_date) %>%
  distinct() %>%
  compute()

imminentCohortSet <- cdm[["imminentFractureCohort"]] %>% 
  select("cohort_definition_id") %>% 
  distinct() %>% 
  mutate(cohort_name = if_else(cohort_definition_id == 1, "imminent_fracture", "else")) %>%
  compute()

noImminentCohortSet <- cdm[["noImminentFractureCohort"]] %>% 
  select("cohort_definition_id") %>% 
  distinct() %>% 
  mutate(cohort_name = if_else(cohort_definition_id == 1, "no_imminent_fracture", "else")) %>%
  compute()

imminent_cohort_count <- cdm[["imminentFractureCohort"]] %>%
  group_by(cohort_definition_id) %>%
  tally() %>%
  compute() %>%
  rename(number_records = n) %>%
  mutate(number_subjects = number_records) %>%
  compute()

noimminent_cohort_count <- cdm[["noImminentFractureCohort"]] %>%
  group_by(cohort_definition_id) %>%
  tally() %>%
  compute() %>%
  rename(number_records = n) %>%
  mutate(number_subjects = number_records) %>%
  compute()

imm_demographics<-
  addDemographics(x = cdm[["imminentFractureCohort"]], cdm = cdm) %>%
  compute()

no_imm_demographics<-
  addDemographics(x = cdm[["noImminentFractureCohort"]], cdm = cdm) %>%
  compute()

#### commodity and medications
info(logger, "CHARACTERISATION: COMORBIDITIES AND MEDICAL HISTORY")
conditions <- paste0(stem_table, "_conditions")
medications <- paste0(stem_table, "_medications")

### 1. With imminent fractures
cdm_char_imm<-CDMConnector::cdm_from_con(
  con = db,
  cdm_schema = cdm_database_schema,
  write_schema = results_database_schema
)

cdm_char_imm[["imminentFractureCohort"]] <- newGeneratedCohortSet(cohortRef = cdm[["imminentFractureCohort"]],
                                                                  cohortSetRef = imminentCohortSet,
                                                                  cohortCountRef = imminent_cohort_count)

cdm_char_imm <- cdmSubsetCohort(cdm_char_imm, "imminentFractureCohort", verbose = T)

# instantiate medications
info(logger, "INSTANTIATE MEDICATIONS")
codelistMedications <- codesFromConceptSet(here("1_InstantiateCohorts", "Medications"), cdm_char_imm)
cdm_char_imm <- generateConceptCohortSet(cdm_char_imm, medications, codelistMedications)

# instantiate conditions
info(logger, "INSTANTIATE CONDITIONS")
codelistConditions <- codesFromConceptSet(here("1_InstantiateCohorts", "Conditions"), cdm_char_imm)
cdm_char_imm <- generateConceptCohortSet(cdm_char_imm, conditions, codelistConditions)

# create table summary
info(logger, "CREATE SUMMARY")

cdm_char_imm[["drug_era2"]] <- cdm_char_imm[["drug_era"]] %>% 
  inner_join(cdm_char_imm[["concept"]] %>% filter(concept_class_id == "Ingredient", standard_concept == "S"), by = c("drug_concept_id" = "concept_id")) %>%
  select(colnames(cdm_char_imm[["drug_era"]])) %>%
  dplyr::filter(lubridate::year(.data$drug_era_start_date) >= 2009) %>%
  dplyr::compute()
  
cdm_char_imm[["visit_occurence2"]] <- cdm_char_imm[["visit_occurrence"]] %>%  
  dplyr::filter(lubridate::year(.data$visit_start_date) >= 2009) %>%
  dplyr::compute()

result_imm <- cdm_char_imm[["imminentFractureCohort"]] %>%
  summariseCharacteristics(
    ageGroup = list(c(50, 54), c(55, 59), c(60, 64), c(65, 69), c(70, 74), c(75,79), c(80,84), c(85,89), c(90,94), c(95,99), c(100,150)),
    tableIntersect = list(
      "Visits" = list(
        tableName = "visit_occurrence2", value = "count", window = c(-365, 0)
      ),
      "Medications" = list(
        tableName = "drug_era2", value = "count", window = c(-365, 0)
      )
    ), 
    cohortIntersect = list(
      "Medications" = list(
        targetCohortTable = medications, value = "flag", window = c(-365, 0)
      ),
      "Conditions" = list(
        targetCohortTable = conditions, value = "flag", window = c(-Inf, 0)
      )
    )
  )

### 2. Without imminent fractures
cdm_char_no_imm<-CDMConnector::cdm_from_con(
  con = db,
  cdm_schema = cdm_database_schema,
  write_schema = results_database_schema
)

cdm_char_no_imm[["noImminentFractureCohort"]] <- newGeneratedCohortSet(cohortRef = cdm[["noImminentFractureCohort"]],
                                                                       cohortSetRef = noImminentCohortSet,
                                                                       cohortCountRef = noimminent_cohort_count)

cdm_char_no_imm <- cdmSubsetCohort(cdm_char_no_imm, "noImminentFractureCohort", verbose = T)

# instantiate medications
info(logger, "INSTANTIATE MEDICATIONS")
codelistMedications <- codesFromConceptSet(here("1_InstantiateCohorts", "Medications"), cdm_char_no_imm)
cdm_char_no_imm <- generateConceptCohortSet(cdm_char_no_imm, medications, codelistMedications)

# instantiate conditions
info(logger, "INSTANTIATE CONDITIONS")
codelistConditions <- codesFromConceptSet(here("1_InstantiateCohorts", "Conditions"), cdm_char_no_imm)
cdm_char_no_imm <- generateConceptCohortSet(cdm_char_no_imm, conditions, codelistConditions)

# create table summary
info(logger, "CREATE SUMMARY")

cdm_char_no_imm[["drug_era2"]] <- cdm_char_no_imm[["drug_era"]] %>% 
  inner_join(cdm_char_no_imm[["concept"]] %>% filter(concept_class_id == "Ingredient", standard_concept == "S"), by = c("drug_concept_id" = "concept_id")) %>%
  select(colnames(cdm_char_no_imm[["drug_era"]])) %>%
  dplyr::filter(lubridate::year(.data$drug_era_start_date) >= 2009) %>%
  dplyr::compute()

cdm_char_no_imm[["visit_occurence2"]] <- cdm_char_no_imm[["visit_occurrence"]] %>%  
  dplyr::filter(lubridate::year(.data$visit_start_date) >= 2009) %>%
  dplyr::compute()

result_no_imm <- cdm_char_no_imm[["imminentFractureCohort"]] %>%
  summariseCharacteristics(
    ageGroup = list(c(50, 54), c(55, 59), c(60, 64), c(65, 69), c(70, 74), c(75,79), c(80,84), c(85,89), c(90,94), c(95,99), c(100,150)),
    tableIntersect = list(
      "Visits" = list(
        tableName = "visit_occurrence2", value = "count", window = c(-365, 0)
      ),
      "Medications" = list(
        tableName = "drug_era2", value = "count", window = c(-365, 0)
      )
    ), 
    cohortIntersect = list(
      "Medications" = list(
        targetCohortTable = medications, value = "flag", window = c(-365, 0)
      ),
      "Conditions" = list(
        targetCohortTable = conditions, value = "flag", window = c(-Inf, 0)
      )
    )
  )


# export results
info(logger, "EXPORT RESULTS")
write_csv(result_imm, here(output_folder, "table_one_imm.csv"))
write_csv(result_no_imm, here(output_folder, "table_one_no_imm.csv"))

# create zip file
info(logger, "ZIPPING RESULTS")
output_folder <- basename(output_folder)
zip(
  zipfile = file.path(paste0(output_folder, "/Results_", db_name, ".zip")),
  files = list.files(output_folder, full.names = TRUE)
)

info(logger, "DONE")