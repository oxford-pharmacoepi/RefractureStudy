## COHORT DATA HEALTH ECONOMICS ----

### Combine list of all vectors from Cohort 1,2, and Target list into one dataframe and adding a period variable
cohort1_combined <- imap_dfr(compCohort1, ~tibble(.x, fracture_period = .y)) %>% 
  mutate(cohort="cohort1") %>% 
  mutate(exposed_yrs = as.numeric(follow_up_end - index_date)/ 365.25) %>% # exposed time in years
  select(subject_id, index_date, fracture_site, fracture_period, cohort, follow_up_end, exposed_yrs)

cohort2_combined <- imap_dfr(compCohort2, ~tibble(.x, fracture_period = .y)) %>% 
  mutate(cohort="cohort2") %>% 
  mutate(exposed_yrs = as.numeric(follow_up_end - index_date)/ 365.25) %>% # exposed time in years
  select(subject_id, index_date, fracture_period, cohort, follow_up_end, exposed_yrs) # does not have fracture site

target_combined <- imap_dfr(targetCohort, ~tibble(.x, fracture_period = .y)) %>% 
  mutate(cohort="target") %>% 
  mutate(exposed_yrs = as.numeric(follow_up_end - index_date)/ 365.25) %>% # exposed time in years
  select(subject_id, index_date, fracture_site, fracture_period, cohort, follow_up_end, exposed_yrs)

### Combine lists of all vectors from PS matching

# TEST MATCHING ----

# test_match12 <- readRDS("subclass12_1.RDS")
# 
# # Getting unique subject_ids from each cohort
# 
# unique_subject_ids_cohort1 <- cohort1_combined$subject_id %>% unique() %>% as.double()
# unique_subject_ids_cohort2 <- cohort2_combined$subject_id %>% unique() %>% as.double()
# 
# # Add a random subject_id column based on group just for testing
# test_match12 <- test_match12 %>%
#   rowwise() %>%
#   mutate(subject_id = case_when(
#     group == "comparator 1" ~ sample(unique_subject_ids_cohort1, 1),
#     group == "comparator 2" ~ sample(unique_subject_ids_cohort2, 1),
#     TRUE ~ NA_real_ 
#   ))
# 
# matched_cohort2_combined <- imap_dfr(test_match12, ~tibble(.x, study_period = .y)) %>% 
#   mutate(cohort="matched_cohort2") 
# 
# matched_cohort1_combined <- imap_dfr(subclassest1, ~tibble(.x, study_period = .y)) %>% # confirm name subclassest1
#   mutate(cohort="matched_cohort1") 

# ### create a new variable "exposure_end" assuming the value of the next event or 730 if no subsequent events 
# ### WE NEED TO ADD A CONDITION FOR OTHER CENSORING HERE (like death, cancer etc) - XIHANG WILL CREATE A BESPOKE CODE FOR THIS
# target_combined <- target_combined %>%
#   arrange(subject_id, index_date) %>%
#   group_by(subject_id) %>%
#   mutate(exposure_end = lead(index_date)) %>% # lead retrieves the subsequent index_date for each row within the same subject_id
#   mutate(exposure_end = case_when(
#     is.na(exposure_end) ~ pmin(index_date + 730, study_end_date), # Use the minimum of the two dates
#     exposure_end - index_date > 730 ~ pmin(index_date + 730, study_end_date), # Use the minimum of the two dates
#     TRUE ~ exposure_end
#   )) %>%
#   mutate(exposed_yrs = as.numeric(exposure_end - index_date)/ 365) %>% # exposed time in years - should we divide by 365.25?
#   ungroup()
#  
# cohort1_combined <- cohort1_combined %>%
#   arrange(subject_id, index_date) %>%
#   group_by(subject_id) %>%
#   mutate(exposure_end = lead(index_date)) %>% # lead retrieves the subsequent index_date for each row within the same subject_id
#   mutate(exposure_end = case_when(
#     is.na(exposure_end) ~ pmin(index_date + 730, study_end_date), # Use the minimum of the two dates
#     exposure_end - index_date > 730 ~ pmin(index_date + 730, study_end_date), # Use the minimum of the two dates
#     TRUE ~ exposure_end
#   )) %>%
#   mutate(exposed_yrs = as.numeric(exposure_end - index_date)/ 365) %>% # exposed time in years
#   ungroup()


