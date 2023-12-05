# ************************************************************************* #
# Purpose: To generate plots for visits and cohorts separate for three cohorts
# Authors: LNN & GF
# Date:
# Last date edited:
# ************************************************************************** #

# # Libraries ----
# install.packages("renv") # if not already installed, install renv from CRAN
# renv::activate() 
# renv::restore() # this should prompt you to install the various packages required for the study

# Libraries for plots
library(ggplot2) # for plots
library(hrbrthemes) # for theme of the plots
library(plotly) # to make the graphs interactive
library(htmlwidgets) # to save the interactive plots as html file



# Graphs ----
# Target Cohort ----

# target_visits_count_wide <- target_results$visits_count_wide
# 
# 
# # Assuming the first 7 columns are not specialties, and the rest are
# target_specialty_cols <- names(target_visits_count_wide)[9:ncol(target_visits_count_wide)]
# 
# # Add a new column for the sum of all visits per subject
# target_visits_count_wide$total_visits <- rowSums(target_visits_count_wide[, target_specialty_cols], na.rm = TRUE)
# 


### Filtering visits based on the cohort_combined
# convert the dataframes into data.table object
setDT(visit_data)
target_combinedDT <- setDT(target_combined)

# This has been reciprocated from the functions.R
filtered_visits_dt <- visit_data[target_combinedDT, on = .(subject_id)]

filtered_visits_dt1 <- filtered_visits_dt[visit_detail_start_date >= index_date & visit_detail_start_date <= follow_up_end,]


filtered_visits_dt1 <- filtered_visits_dt1[
  ,`visit_count` := .N, by = list(subject_id, index_date, specialty)
]

# filtered_visits_temp <- visit_data %>%
#   left_join(target_combinedDT, by = "subject_id", relationship = "many-to-many") %>%
#   filter(visit_detail_start_date >= index_date & visit_detail_start_date <= follow_up_end) %>%
#   group_by(subject_id, index_date, specialty) %>% # we group by index date to ensure each visit is associated with an entry
#   summarise(visit_count = n()) %>%
#   ungroup()

visits_count_wide_temp <- filtered_visits_dt1 %>%
  pivot_wider(names_from = specialty, values_from = visit_count, values_fill = NA)

# replace all NAs with 0
visits_count_wide_temp_1 <- visits_count_wide_temp %>% mutate(across(everything(), .fns = ~replace_na(.,0)))
# count the visits
# Columns to sum row-wise
columns_to_sum <- colnames(visits_count_wide_temp_1)[12:77]  # Include all 66 column names here

# Adding a new column with row-wise sum of selected columns
visits_count_wide_temp_2 <- visits_count_wide_temp_1 %>%
  mutate(total_visits_per_w  = rowSums(select(., all_of(columns_to_sum_pt1))),
         cohort = "target")

# Calculate the percentage of subjects for each total visit count
target_total_visits_percentage <- visits_count_wide_temp_2 %>%
  group_by(total_visits_per_w) %>%
  summarize(Subject_Count = n(), .groups = 'drop') %>%
  mutate(Percent_Subjects = Subject_Count / sum(Subject_Count) * 100)

# Plotting
(plot1 <- target_total_visits_percentage %>% 
    # filter(total_visits_per_w > 0) %>% 
    ggplot(aes(x = total_visits_per_w, y = Percent_Subjects)) +
  geom_bar(stat = "identity") +
  labs(x = "Total Number of Visits", y = "% of Subjects (entries)",
       title = "Target cohort - visits") +
  hrbrthemes::theme_ipsum() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
    axis.title = element_text(hjust = 0.5, size = 10),
    panel.spacing = unit(0.1, "lines")
  ))

# save the plot
ggsave(here::here("4_Healtheconomics", "Results", "target_distribution_of_visits.PNG"))

# create an interactive plot
(plotly1 <- ggplotly(plot1))

# save interactive plot as a html file
htmlwidgets::saveWidget(plotly::ggplotly(plot1), here::here("4_Healtheconomics", "Results", "target_distribution_of_visits_v2.html"))

# Filter subjects with more than 100 visits
target_subjects_over_100_visits <- visits_count_wide_temp_2 %>%
  filter(total_visits_per_w > 100) %>%
  select(subject_id) 

# View the resulting data
print(target_subjects_over_100_visits)

# convert to long format
target_visits_count_wide_to_long <- visits_count_wide_temp_2 %>% 
  gather(specialty, visits, 12:(ncol(.))) %>% 
  complete(subject_id, specialty, fill = list(visits = 0)) 

# grouping by specialty to see which specilities had the majority of visits
target_specialty_visits <- target_visits_count_wide_to_long %>% 
  group_by(specialty) %>% 
  summarise(Subject_Count_visits = sum(visits), .groups = 'drop') %>%
  arrange(desc(Subject_Count_visits)) 


# Top 3 visited specialties: General Medical Practitioner,Community Practitioner, Health Care Support Worker
target_total_visits_sp <- target_visits_count_wide_to_long %>% 
 # filter(specialty %in% c("General Medical Practitioner")) %>% 
  group_by(visits) %>% 
  summarise(count_v = n()) %>% 
  mutate(percent_count = count_v/sum(count_v) *100) %>% 
  dplyr::ungroup()

joined_df <- full_join(target_visits_count_wide_to_long, target_total_visits_sp,
                       by =  "visits")
# target_total_visits_top3 <- target_visits_count_wide_to_long %>% 
#   filter(specialty %in% c("General Medical Practitioner",
#                           "Community Practitioner", "Health Care Support Worker"))
# Plotting -
(plot2 <- joined_df %>% 
    # filter(visits >0) %>% 
    ggplot(aes(x = visits, y = percent_count)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    # geom_histogram(bins = 30)+
    labs(x = "Total Number of Visits", y = "% of Subjects (entries)",
         title = "Target cohort top 3 with the most visits") +
    # facet_grid(rows = vars(specialty), scales = "free", space = "free") +
    facet_wrap(~ specialty, scales = "free_x", ncol = 12)+
    hrbrthemes::theme_ipsum() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
      axis.title = element_text(hjust = 0.5, size = 10),
      panel.spacing = unit(0.1, "lines"),
      strip.text.y = element_text(angle = 0)
    ) 
  )

# save the plot
ggsave(here::here("4_Healtheconomics", "Results", "target_distribution_of_visits_top3.PNG"))

# create an interactive plot
ggplotly(plot2)

htmlwidgets::saveWidget(plotly::ggplotly(plot2), here::here("4_Healtheconomics", "Results", "target_distribution_of_visits_top3.html"))


# Cohort1 ----

# Cohort2 ----
