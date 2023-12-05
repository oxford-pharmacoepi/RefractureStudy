## MATCHED COHORTS ## -----
# here we combine all the matched vectors (per fracture period) into a single list, 
# then we select the cohort of relevance and we delete duplicated subjects


# Comparison 1: Target versus Matched cohort 1 

#Target
target_matched <- Reduce(dplyr::union_all, subclasses01) %>%
  filter(group == "target") %>% 
  mutate(cohort = "target") %>%
  select(-group) %>%
  distinct(subject_id, .keep_all = TRUE)

#Cohort1 - to
cohort1_matched_to <- Reduce(dplyr::union_all, subclasses01) %>%
  filter(group == "comparator 1") %>% 
  mutate(cohort = "cohort1") %>%
  select(-group) %>%
  distinct(subject_id, .keep_all = TRUE)


## Comparison 2: cohort 1 versus matched cohort 2

#Cohort1 - from
cohort1_matched_from <- Reduce(dplyr::union_all, subclasses12) %>%
  filter(group == "comparator 1") %>% 
  mutate(cohort = "cohort1") %>%
  select(-group) %>%
  distinct(subject_id, .keep_all = TRUE)

#Cohort2
cohort2_matched <- Reduce(dplyr::union_all, subclasses12) %>%
  filter(group == "comparator 2") %>% 
  mutate(cohort = "cohort2") %>%
  select(-group) %>%
  distinct(subject_id, .keep_all = TRUE)


## UNMATCHED COHORTS ## ------
# Combine list of all vectors from Cohort 1,2, and Target list into one dataframe and adding a period variable
# cohort1_combined_unmatched <- imap_dfr(compCohort1, ~tibble(.x, fracture_period = .y)) %>% 
#   mutate(cohort="cohort1") %>% 
#   mutate(exposed_yrs = as.numeric(follow_up_end - index_date)/ 365.25) %>% # exposed time in years
#   select(subject_id, index_date, fracture_site, fracture_period, cohort, follow_up_end, exposed_yrs)
# 
# cohort2_combined_unmatched <- imap_dfr(compCohort2, ~tibble(.x, fracture_period = .y)) %>% 
#   mutate(cohort="cohort2") %>% 
#   mutate(exposed_yrs = as.numeric(follow_up_end - index_date)/ 365.25) %>% # exposed time in years
#   select(subject_id, index_date, fracture_period, cohort, follow_up_end, exposed_yrs) # does not have fracture site
# 
# target_combined_unmatched <- imap_dfr(targetCohort, ~tibble(.x, fracture_period = .y)) %>% 
#   mutate(cohort="target") %>% 
#   mutate(exposed_yrs = as.numeric(follow_up_end - index_date)/ 365.25) %>% # exposed time in years
#   select(subject_id, index_date, fracture_site, fracture_period, cohort, follow_up_end, exposed_yrs)




