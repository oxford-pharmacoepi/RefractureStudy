## MATCHED COHORTS ## -----
# here we combine all the matched vectors (per fracture period) into a single list, 
# then we select the cohort of relevance and we delete duplicated subjects
load(here(sub_output_folder, "tempData", "subclasses01.RData"))
load(here(sub_output_folder, "tempData", "subclasses12.RData"))
# Comparison 1: Target versus Matched cohort 1 

#Target
subclasses01_back_up <- subclasses01
subclasses12_back_up <- subclasses12

for (i in (1:length(subclasses01_back_up))){
  subclasses01_back_up[[i]] <- subclasses01_back_up[[i]] %>% 
    dplyr::select(subject_id, group, index_date)
}

target_matched <- Reduce(dplyr::union_all, subclasses01_back_up) %>%
  dplyr::filter(group == "target") %>% 
  dplyr::mutate(cohort = "target") %>%
  dplyr::select(-group) %>%
  dplyr::distinct(subject_id, .keep_all = TRUE)

#Cohort1 - to
cohort1_matched_to <- Reduce(dplyr::union_all, subclasses01_back_up) %>%
  dplyr::filter(group == "comparator 1") %>% 
  dplyr::mutate(cohort = "cohort1") %>%
  dplyr::select(-group) %>%
  dplyr::distinct(subject_id, .keep_all = TRUE)

## Comparison 2: cohort 1 versus matched cohort 2

for (i in (1:length(subclasses12_back_up))){
  subclasses12_back_up[[i]] <- subclasses12_back_up[[i]] %>% 
    dplyr::select(subject_id, group, index_date)
}

#Cohort1 - from
cohort1_matched_from <- Reduce(dplyr::union_all, subclasses12_back_up) %>%
  dplyr::filter(group == "comparator 1") %>% 
  dplyr::mutate(cohort = "cohort1") %>%
  dplyr::select(-group) %>%
  dplyr::distinct(subject_id, .keep_all = TRUE)

#Cohort2
cohort2_matched <- Reduce(dplyr::union_all, subclasses12_back_up) %>%
  dplyr::filter(group == "comparator 2") %>% 
  dplyr::mutate(cohort = "cohort2") %>%
  dplyr::select(-group) %>%
  dplyr::distinct(subject_id, .keep_all = TRUE)

rm(subclasses01, subclasses01_back_up, subclasses12, subclasses12_back_up)
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
