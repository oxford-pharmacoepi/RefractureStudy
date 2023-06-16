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
fracture_table_follow_up_back_up <- fracture_table_follow_up

counts <- tibble()

counts <- counts %>%
  union_all(
    tibble(
    number_records = fracture_table_follow_up_back_up %>% tally() %>% pull(),
    number_subjects = fracture_table_follow_up_back_up %>% distinct(subject_id) %>% tally() %>% pull()
  )
  )
fracture_table_follow_up_back_up <- nextFractureClean(fracture_table_follow_up_back_up)
fracture_table_follow_up_back_up <- addIndex(fracture_table_follow_up_back_up)
fracture_table_follow_up_back_up <- noDeathOnIndex(fracture_table_follow_up_back_up)
fracture_table_follow_up_back_up <- noCancerPriorOrOnIndex(fracture_table_follow_up_back_up)
fracture_table_follow_up_back_up <- noBoneDiseasePriorOrOnIndex(fracture_table_follow_up_back_up)
fracture_table_follow_up_back_up <- addInTwoYearsAfter(fracture_table_follow_up_back_up)
fracture_table_follow_up_back_up <- addInObsEndDate(fracture_table_follow_up_back_up)
fracture_table_follow_up_back_up <- addInCancerPostIndex(fracture_table_follow_up_back_up)
fracture_table_follow_up_back_up <- addInBoneDiseasePostIndex(fracture_table_follow_up_back_up)
fracture_table_follow_up_back_up <- addInDeath(fracture_table_follow_up_back_up)
