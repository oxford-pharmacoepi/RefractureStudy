# Functions

# Analyse primary care visits

analyse_visits <- function(cohort_combined) {

### Filtering visits based on the cohort_combined
filtered_visits <- visit_data %>%
  left_join(cohort_combined, by = "subject_id", relationship = "many-to-many") %>% 
  filter(visit_detail_start_date >= index_date & visit_detail_start_date <= follow_up_end) %>% 
  group_by(subject_id, index_date, specialty) %>% # we group by index date to ensure each visit is associated with an entry
  summarise(visit_count = n()) %>%
  ungroup() 

### Pivot the data
visits_count_wide <- filtered_visits %>%
  pivot_wider(names_from = specialty, values_from = visit_count, values_fill = NA)

### Join the wide dataframe back to cohort_combined and count tot num visits
visits_count_wide <- left_join(cohort_combined, visits_count_wide, by = c("subject_id", "index_date"))
#%>%   mutate(total_visits = rowSums(select(., 8:ncol(.)), na.rm = TRUE)) # specialties start from 8th column - remember to change if needed

### summary for user only (subjects/visit= NA, not counted)
user_only_summary <- visits_count_wide %>%
  gather(specialty, visits, 8:(ncol(.))) %>% # Convert wide format to long format, gathering all columns from the 8th onward - change if you change columns or add new
  mutate(visits_per_year = visits / exposed_yrs) %>% # Calculate visits per year
  filter(visits > 0) %>% # Only consider rows where there was at least one visit (i.e. service users only)
  group_by(specialty) %>%  # Group by specialty and calculate statistics for visits per year
  summarise(
    mean_visits_per_year = round(mean(visits_per_year, na.rm = TRUE),2),
    sd_visits_per_year = round(sd(visits_per_year, na.rm = TRUE),2),
    min_visits_per_year = round(min(visits_per_year, na.rm = TRUE),2),
    max_visits_per_year = round(max(visits_per_year, na.rm = TRUE),2),
    num_subjects_visited = n_distinct(subject_id), # number of service users per specialty
    tot_visits = sum(visits)
  )

### summary for all subjects (subjects/visits = NA, treated as zero)
all_summary <- visits_count_wide %>%
  gather(specialty, visits, 8:(ncol(.))) %>% # Convert wide format to long format, gathering all columns from the 8th onward - change if you change columns or add new
  complete(subject_id, specialty, fill = list(visits = 0)) %>% # filling missing visits with 0
  mutate(visits_per_year = visits / exposed_yrs) %>%
  group_by(specialty) %>%
  summarise(
    mean_visits_per_year = round(mean(visits_per_year, na.rm = TRUE),2),
    sd_visits_per_year = round(sd(visits_per_year, na.rm = TRUE),2),
    min_visits_per_year = round(min(visits_per_year, na.rm = TRUE),2),
    max_visits_per_year = round(max(visits_per_year, na.rm = TRUE),2), # same as service users 
    num_subjects_visited = n_distinct(subject_id[visits > 0]), # same as service users 
    tot_visits = sum(visits) # same as service users 
  )

# Calculating non-service users
non_service_users <- visits_count_wide %>%
  filter(rowSums(is.na(select(., 8:ncol(.)))) == (ncol(.) - 7)) %>%
  summarise (non_service_users = n_distinct(subject_id))

return(list(user_only_summary = user_only_summary, all_summary = all_summary,non_service_users=non_service_users))
}


# Estimate costs primary care visits 

analyse_visits_cost <- function(cohort_combined) {
  
  ### Filtering visits based on the cohort_combined
  filtered_visits <- visit_data %>%
    left_join(cohort_combined, by = "subject_id", relationship = "many-to-many") %>% 
    filter(visit_detail_start_date >= index_date & visit_detail_start_date <= follow_up_end) %>% 
    group_by(subject_id, index_date, specialty) %>% # we group by index date to ensure each visit is associated with an entry
    summarise(visit_count = n()) %>%
    ungroup() 
  
  ### Import unit cost
  filtered_visits <- filtered_visits %>% 
    left_join(provider_cost_inputs %>% select (specialty, unit_cost),by= "specialty")  
  
  ### Compute costs visits
  filtered_visits <- filtered_visits %>%  
    mutate (visit_cost = visit_count * unit_cost) %>% 
    select (-unit_cost, -visit_count)
  
  ### Pivot the data
  visits_cost_wide <- filtered_visits %>%
    pivot_wider(names_from = specialty, values_from = visit_cost, values_fill = NA)
  
  ### Join the wide dataframe back to cohort_combined and count tot num visits
  visits_cost_wide <- left_join(target_combined, visits_cost_wide, by = c("subject_id", "index_date"))
  
  ### summary for user only (subjects/visit= NA, not counted)
  user_only_cost_summary <- visits_cost_wide %>%
    gather(specialty, visits, 8:(ncol(.))) %>% # Convert wide format to long format, gathering all columns from the 8th onward - change if you change columns or add new
    mutate(visits_per_year = visits / exposed_yrs) %>% # Calculate visits per year
    filter(visits > 0) %>% # Only consider rows where there was at least one visit (i.e. service users only)
    group_by(specialty) %>%  # Group by specialty and calculate statistics for visits per year
    summarise(
      mean_cost_visits_per_year = round(mean(visits_per_year, na.rm = TRUE),2),
      sd_cost_visits_per_year = round(sd(visits_per_year, na.rm = TRUE),2),
      min_cost_visits_per_year = round(min(visits_per_year, na.rm = TRUE),2),
      max_cost_visits_per_year = round(max(visits_per_year, na.rm = TRUE),2),
      num_subjects_visited = n_distinct(subject_id), # number of service users per specialty
      tot_visits = sum(visits)
    )
  
  ### summary for all subjects (subjects/visits = NA, treated as zero)
  all_cost_summary <- visits_cost_wide %>%
    gather(specialty, visits, 8:(ncol(.))) %>% # Convert wide format to long format, gathering all columns from the 8th onward - change if you change columns or add new
    complete(subject_id, specialty, fill = list(visits = 0)) %>% # filling missing visits with 0
    mutate(visits_per_year = visits / exposed_yrs) %>%
    group_by(specialty) %>%
    summarise(
      mean_cost_visits_per_year = round(mean(visits_per_year, na.rm = TRUE),2),
      sd_cost_visits_per_year = round(sd(visits_per_year, na.rm = TRUE),2),
      min_cost_visits_per_year = round(min(visits_per_year, na.rm = TRUE),2),
      max_cost_visits_per_year = round(max(visits_per_year, na.rm = TRUE),2), # same as service users 
      num_subjects_visited = n_distinct(subject_id[visits > 0]), # same as service users 
      tot_visits = sum(visits) # same as service users 
    )
  
  return(list(user_only_cost_summary = user_only_cost_summary, all_cost_summary = all_cost_summary))
}


# Cohort summary

cohort_summary <- function(data, cohort_name, non_service_users) {
  entries_per_woman <- data %>% 
    group_by(subject_id) %>% 
    summarise(entries_per_woman = n(), .groups = "drop") # Drop groups after summarising) 
  
  summary <- tibble(
    cohort = cohort_name,
    num_distinct_women = n_distinct(data$subject_id),
    num_entries = nrow(data),
    tot_exposed_yrs = sum(data$exposed_yrs),
    mean_entries_per_woman = round(mean(entries_per_woman$entries_per_woman), 2),
    sd_entries_per_woman = round(sd(entries_per_woman$entries_per_woman), 2),
    min_entries_per_woman = min(entries_per_woman$entries_per_woman),
    max_entries_per_woman = max(entries_per_woman$entries_per_woman),
    num_non_service_users = unlist(non_service_users),
    perc_non_service_users = unlist(round((non_service_users / n_distinct(data$subject_id) * 100), 2)),
    num_service_users = num_distinct_women - num_non_service_users,
    perc_service_users = 100-perc_non_service_users,
    num_women_1_entry = sum(entries_per_woman$entries_per_woman == 1),
    num_women_2_entries = sum(entries_per_woman$entries_per_woman == 2),
    num_women_3plus_entries = sum(entries_per_woman$entries_per_woman >= 3)
  )
  
  return(summary)
}
