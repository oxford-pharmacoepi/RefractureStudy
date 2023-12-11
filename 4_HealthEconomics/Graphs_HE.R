# ************************************************************************* #
# Purpose: To generate plots
# Authors: LN 
# ************************************************************************** #
# To begin with, please, add visit_count_wide as a resulting output of analyse_visit function

# Graphs ----
# Target Cohort ----

target_temp <- analyse_visits(target_matched, visit_data = visit_data)
target_temp2 <- target_temp$visits_count_wide

# replace NAs with 0
target_temp3 <- target_temp2 %>% 
  mutate(across(everything(), .fns = ~replace_na(.,0)))

# columns to sum row-wise
columns_to_sum_target1 <- colnames(target_temp3)[8:73]  

# adding a new column with row-wise sum of selected columns
target_temp4 <- target_temp3 %>%
  mutate(total_visits_per_w  = rowSums(select(., all_of(columns_to_sum_target1))),
         cohort = "target_matched")

# calculate the percentage of subjects for each total visit count
target_temp5 <- target_temp4 %>%
  group_by(total_visits_per_w) %>%
  summarize(Subject_Count = n(), .groups = 'drop') %>%
  mutate(Percent_Subjects = Subject_Count / sum(Subject_Count) * 100)

# plotting - target matched ----
(plot1 <- target_temp5 %>% 
   ggplot(aes(x = total_visits_per_w, y = Percent_Subjects)) +
   geom_bar(stat = "identity", fill="#A52A2A", alpha=0.9) +
   labs(x = "Total Number of Visits", y = "% of Subjects (entries)",
        title = "Target - visits") +
   hrbrthemes::theme_ipsum() +
   theme(
     plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
     axis.title = element_text(hjust = 0.5, size = 11),
     panel.spacing = unit(0.1, "lines")
   ))

# convert to long format
target_temp6 <- target_temp4 %>%
  gather(specialty, visits, 8:73) %>%
  complete(subject_id, specialty, fill = list(visits = 0))

# grouping by specialty to see which specialties had the majority of visits
target_specialty_visits_top10 <- target_temp6 %>% 
  group_by(specialty) %>% 
  summarise(Subject_Count_visits = sum(visits), .groups = 'drop') %>%
  arrange(desc(Subject_Count_visits)) %>% 
  slice(1:10)