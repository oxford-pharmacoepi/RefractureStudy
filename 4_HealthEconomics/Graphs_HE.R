# ************************************************************************* #
# Purpose: To generate plots
# Authors: LN 
# ************************************************************************** #
# To begin with, please, add visit_count_wide as a resulting output of analyse_visit function

# Graphs ----
# Target Cohort ----

plotFolder <- here(sub_output_folder, "plots_HE")
if (!dir.exists(plotFolder)) {
  dir.create(plotFolder)
}

target_temp <- analyse_visits(target_matched, visit_data = cdm[["visit_data"]])
target_temp2 <- target_temp$visits_count_wide

# replace NAs with 0
target_temp3 <- target_temp2 %>% 
  dplyr::mutate(across(everything(), .fns = ~replace_na(.,0)))

# columns to sum row-wise
columns_to_sum_target1 <- colnames(target_temp3)[(colnames(target_temp3)%in% specialty_names)]

# adding a new column with row-wise sum of selected columns
target_temp3 <- target_temp3 %>% 
  dplyr::select(-subject_id, -exposed_yrs) %>% 
  dplyr::mutate_if(is.numeric,as.integer)
target_temp4 <- target_temp3 %>%
  dplyr::mutate(total_visits_per_w  = rowSums(select(., all_of(columns_to_sum_target1))),
         cohort = "target_matched")

# calculate the percentage of subjects for each total visit count
target_temp5 <- target_temp4 %>%
  dplyr::group_by(total_visits_per_w) %>%
  dplyr::summarize(Subject_Count = n(), .groups = 'drop') %>%
  dplyr::mutate(Percent_Subjects = Subject_Count / sum(Subject_Count) * 100)

# plotting - target matched ----
(plot1 <- target_temp5 %>% 
   ggplot(aes(x = total_visits_per_w, y = Percent_Subjects)) +
   geom_bar(stat = "identity", fill="#A52A2A", alpha=0.9) +
   labs(x = "Total Number of Visits", y = "% of Subjects (entries)",
        title = "Target - visits") +
   scale_y_continuous(limits = c(0, 9) , 
                      breaks = seq(0, 9, by = 1))+
   coord_cartesian(xlim = c(0, 400)) +
   hrbrthemes::theme_ipsum() +
   theme(
     plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
     axis.title = element_text(hjust = 0.5, size = 11),
     panel.spacing = unit(0.1, "lines")
   ))

not_in <- colnames(target_temp4)[(colnames(target_temp4)%in% specialty_names)]
# convert to long format
target_temp6 <- target_temp4 %>%
  pivot_longer(all_of(not_in), names_to = "specialty", values_to = "visits")

# grouping by specialty to see which specialties had the majority of visits
target_specialty_visits_top10 <- target_temp6 %>% 
  dplyr::group_by(specialty) %>% 
  dplyr::summarise(Subject_Count_visits = sum(visits), .groups = 'drop') %>%
  dplyr::arrange(desc(Subject_Count_visits)) %>% 
  dplyr::slice(1:10)

# filter the data with the top10 visited specialties
levels(as.factor(target_specialty_visits_top10$specialty))

target_temp6_top10 <- target_temp6 %>% 
  filter(specialty %in% c("Community Nurse", "Community Practitioner",
                          "General Medical Practitioner", "GP Registrar",
                          "Health Care Support Worker", "Medical Secretary",
                          "Salaried General Practitioner", "Sessional GP",
                          "Specialist Nurse Practitioner", "Staff Nurse"))

# box plot of top10 ----
(bx_plot1 <- ggplot(data = target_temp6_top10,
                    aes(x=reorder(specialty, visits), y=visits, fill=specialty)) +
   geom_boxplot(outlier.shape = NA ) + # outlier.shape = NA removes outliers
   coord_flip() +
   scale_y_continuous(limits = c(1, 50)) +
   stat_summary(fun = "mean", geom = "point", shape = 2, size = 1.2, color = "red") +
   viridis::scale_fill_viridis(discrete = T, alpha = 0.7, option = "D") +
   labs(title = "Top10 specialties visited by Target Cohort", 
        subtitle = "Only service users & absence of outliers",
        x = "Specialties", y = "Number of visits") +
   theme_minimal() +
   theme(
     legend.position="none",
     plot.title = element_text(size=14, hjust = 0.5),
     axis.text.x = element_text(angle = 45)
   ) 
)

# save the plot
ggsave(here::here(plotFolder, "target_distribution_of_visits_top10_specialties.PNG"),
       bx_plot1, width = 8, height = 6)


# create a plotly object
bx_plotly1 <- plot_ly(target_temp6_top10, y = ~specialty, x = ~visits, boxpoints = "all", color = ~specialty, type = "box",
                      showlegend = FALSE)

# save theplot
htmlwidgets::saveWidget(as_widget(bx_plotly1), here::here(plotFolder, "target_distribution_of_visits_top10_specialties_plotly.html"))

# cohort1_matched_to ----
cohort1_matched_to_temp <- analyse_visits(cohort1_matched_to, visit_data = cdm[["visit_data"]])
cohort1_matched_to_temp2 <- cohort1_matched_to_temp$visits_count_wide

# replace NAs with 0
cohort1_matched_to_temp3 <- cohort1_matched_to_temp2 %>% 
  mutate(across(everything(), .fns = ~replace_na(.,0)))

# columns to sum row-wise
columns_to_sum_cohort1_matched_to1 <- colnames(cohort1_matched_to_temp3)[(colnames(cohort1_matched_to_temp3)%in% specialty_names)]

# adding a new column with row-wise sum of selected columns
cohort1_matched_to_temp3 <- cohort1_matched_to_temp3 %>% 
  dplyr::select(-subject_id, -exposed_yrs) %>% 
  dplyr::mutate_if(is.numeric,as.integer)
cohort1_matched_to_temp4 <- cohort1_matched_to_temp3 %>%
  mutate(total_visits_per_w  = rowSums(select(., all_of(columns_to_sum_cohort1_matched_to1))),
         cohort = "target_matched")

# calculate the percentage of subjects for each total visit count
cohort1_matched_to_temp5 <- cohort1_matched_to_temp4 %>%
  group_by(total_visits_per_w) %>%
  summarize(Subject_Count = n(), .groups = 'drop') %>%
  mutate(Percent_Subjects = Subject_Count / sum(Subject_Count) * 100)

# plotting - target matched ----
(plot1.1 <- cohort1_matched_to_temp5 %>% 
   ggplot(aes(x = total_visits_per_w, y = Percent_Subjects)) +
   geom_bar(stat = "identity", fill="#B3EE3A", alpha=0.9) +
   labs(x = "Total Number of Visits", y = "% of Subjects (entries)",
        title = "Matched-Cohort 1 - visits") +
   scale_y_continuous(limits = c(0, 9) , 
                      breaks = seq(0, 9, by = 1))+
   coord_cartesian(xlim = c(0, 400)) +
   hrbrthemes::theme_ipsum() +
   theme(
     plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
     axis.title = element_text(hjust = 0.5, size = 11),
     panel.spacing = unit(0.1, "lines")
   ))

# let's combine the two bar graphs
(plot_combo1 <- plot1 + plot1.1)

ggsave(here::here(plotFolder, "target-matched_cohort1_distribution_of_visits.PNG"),
       plot_combo1, width = 8, height = 6)

# ---- Cohort1 ----
cohort1_temp <- analyse_visits(cohort1_matched_from, visit_data = cdm[["visit_data"]])
cohort1_temp2 <- cohort1_temp$visits_count_wide

# replace NAs with 0
cohort1_temp3 <- cohort1_temp2 %>% 
  mutate(across(everything(), .fns = ~replace_na(.,0)))

# columns to sum row-wise
columns_to_sum_cohort1 <- colnames(cohort1_temp3)[(colnames(cohort1_temp3)%in% specialty_names)]

# adding a new column with row-wise sum of selected columns
cohort1_temp3 <- cohort1_temp3 %>% 
  dplyr::select(-subject_id, -exposed_yrs) %>% 
  dplyr::mutate_if(is.numeric,as.integer)

cohort1_temp4 <- cohort1_temp3 %>%
  mutate(total_visits_per_w  = rowSums(select(., all_of(columns_to_sum_cohort1))),
         cohort = "cohort1")

# calculate the percentage of subjects for each total visit count
cohort1_temp5 <- cohort1_temp4 %>%
  group_by(total_visits_per_w) %>%
  summarize(Subject_Count = n(), .groups = 'drop') %>%
  mutate(Percent_Subjects = Subject_Count / sum(Subject_Count) * 100)

# plotting
(plot2 <- cohort1_temp5 %>% 
    ggplot(aes(x = total_visits_per_w, y = Percent_Subjects)) +
    geom_bar(stat = "identity", fill="#69b3a2", alpha=0.9) +
    labs(x = "Total Number of Visits", y = "% of Subjects (entries)",
         title = "Cohort1 - visits") +
    scale_y_continuous(limits = c(0, 10) , 
                       breaks = seq(0, 10, by = 2))+
    coord_cartesian(xlim = c(0, 400)) +
    hrbrthemes::theme_ipsum() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
      axis.title = element_text(hjust = 0.5, size = 10),
      panel.spacing = unit(0.1, "lines")
    ))

# save the plot
ggsave(here::here(plotFolder, "cohort1_distribution_of_visits.PNG"),
       plot2, width = 6, height = 4)

# box plot of count of visits by specialty
# convert to long format
not_in <- colnames(cohort1_temp4)[(colnames(cohort1_temp4)%in% specialty_names)]
cohort1_temp6 <- cohort1_temp4 %>%
  pivot_longer(all_of(not_in), names_to = "specialty", values_to = "visits")

# grouping by specialty to see which specialties had the majority of visits
cohort1_specialty_visits_top10 <- cohort1_temp6 %>% 
  dplyr::group_by(specialty) %>% 
  dplyr::summarise(Subject_Count_visits = sum(visits), .groups = 'drop') %>%
  dplyr::arrange(desc(Subject_Count_visits)) %>% 
  dplyr::slice(1:10)

# filter the data with the top10 visited specialties
levels(as.factor(cohort1_specialty_visits_top10$specialty))

# top 10 visited by cohort1
cohort1_temp6_top10 <- cohort1_temp6 %>% 
  filter(specialty %in% c("Community Nurse", "Community Practitioner",
                          "General Medical Practitioner", "GP Registrar",
                          "Health Care Support Worker", "Medical Secretary",
                          "Salaried General Practitioner", "Sessional GP",
                          "Specialist Nurse Practitioner", "Staff Nurse"))

# boxplot cohort1 ----
(bx_plot2 <- ggplot(data = cohort1_temp6_top10,
                    aes(x=reorder(specialty, visits), y=visits, fill=specialty)) +
   geom_boxplot() +
   coord_flip() +
   scale_y_continuous(limits = c(1, 50)) +
   stat_summary(fun = "mean", geom = "point", shape = 2, size = 1.2, color = "red") +
   viridis::scale_fill_viridis(discrete = T, alpha = 0.7, option = "D") +
   labs(title = "Top10 specialties visited by Cohort1", 
        subtitle = "Only service users & absence of outliers",
        x = "Specialties", y = "Number of visits") +
   theme_minimal() +
   theme(
     legend.position="none",
     plot.title = element_text(size=16, hjust = 0.5),
     axis.text.x = element_text(angle = 45)
   ) 
)

# save the object
ggsave(here::here(plotFolder, "cohort1_distribution_of_visits_top10_specialties.PNG"),
       bx_plot2, width = 8, height = 6)

# creating a plotly object
bx_plotly2 <- plot_ly(cohort1_temp6_top10, y = ~specialty, x = ~visits, boxpoints = "all", color = ~specialty, type = "box",
                      showlegend = FALSE)
htmlwidgets::saveWidget(as_widget(bx_plotly2), here::here(plotFolder, "cohort1_distribution_of_visits_top10_specialties_plotly.html"))


# cohort1_matched_from ----
cohort1_matched_from_temp <- analyse_visits(cohort1_matched_from, visit_data = cdm[["visit_data"]])
cohort1_matched_from_temp2 <- cohort1_matched_to_temp$visits_count_wide

# replace NAs with 0
cohort1_matched_from_temp3 <- cohort1_matched_from_temp2 %>% 
  mutate(across(everything(), .fns = ~replace_na(.,0)))

# columns to sum row-wise
columns_to_sum_cohort1_matched_from1 <- colnames(cohort1_matched_from_temp3)[(colnames(cohort1_matched_from_temp3)%in% specialty_names)]

# adding a new column with row-wise sum of selected columns
cohort1_matched_from_temp3 <- cohort1_matched_from_temp3 %>% 
  dplyr::select(-subject_id, -exposed_yrs) %>% 
  dplyr::mutate_if(is.numeric,as.integer)

cohort1_matched_from_temp4 <- cohort1_matched_from_temp3 %>%
  dplyr::mutate(total_visits_per_w  = rowSums(select(., all_of(columns_to_sum_cohort1_matched_from1))),
         cohort = "target_matched")

# calculate the percentage of subjects for each total visit count
cohort1_matched_from_temp5 <- cohort1_matched_from_temp4 %>%
  dplyr::group_by(total_visits_per_w) %>%
  dplyr::summarize(Subject_Count = n(), .groups = 'drop') %>%
  dplyr::mutate(Percent_Subjects = Subject_Count / sum(Subject_Count) * 100)

# plotting - target matched ----
(plot2.1 <- cohort1_matched_from_temp5 %>% 
   ggplot(aes(x = total_visits_per_w, y = Percent_Subjects)) +
   geom_bar(stat = "identity", fill="#8B4C39", alpha=0.9) +
   labs(x = "Total Number of Visits", y = "% of Subjects (entries)",
        title = "Cohort1 matched to target - visits") +
   scale_y_continuous(limits = c(0, 10) , 
                      breaks = seq(0, 10, by = 2))+
   coord_cartesian(xlim = c(0, 400)) +
   hrbrthemes::theme_ipsum() +
   theme(
     plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
     axis.title = element_text(hjust = 0.5, size = 11),
     panel.spacing = unit(0.1, "lines")
   ))

# let's combine the two bar graphs
(plot_combo2 <- plot2 + plot2.1)

ggsave(here::here(plotFolder, "cohort1-matched_from_cohort1_distribution_of_visits.PNG"),
       plot_combo2, width = 8, height = 6)


# Cohort2 ----
cohort2_temp <- analyse_visits(cohort2_matched, visit_data = cdm[["visit_data"]])
cohort2_temp2 <- cohort2_temp$visits_count_wide

# replace NAs with 0
cohort2_temp3 <- cohort2_temp2 %>% 
  mutate(across(everything(), .fns = ~replace_na(.,0)))

# columns to sum row-wise
# please, help me confirm the number of cols, then make the changes if needed
columns_to_sum_cohort2 <- colnames(cohort2_temp3)[(colnames(cohort2_temp3)%in% specialty_names)]

# adding a new column with row-wise sum of selected columns
cohort2_temp3 <- cohort2_temp3 %>% 
  dplyr::select(-subject_id, -exposed_yrs) %>% 
  dplyr::mutate_if(is.numeric,as.integer)

cohort2_temp4 <- cohort2_temp3 %>%
  mutate(total_visits_per_w  = rowSums(select(., all_of(columns_to_sum_cohort2))),
         cohort = "cohort2_matched")

# calculate the percentage of subjects for each total visit count
cohort2_temp5 <- cohort2_temp4 %>%
  group_by(total_visits_per_w) %>%
  summarize(Subject_Count = n(), .groups = 'drop') %>%
  mutate(Percent_Subjects = Subject_Count / sum(Subject_Count) * 100)

# plotting
(plot3 <- cohort2_temp5 %>% 
    ggplot(aes(x = total_visits_per_w, y = Percent_Subjects)) +
    geom_bar(stat = "identity", fill="hotpink3", alpha=0.9) +
    labs(x = "Total Number of Visits", y = "% of Subjects (entries)",
         title = "Matched-Cohort2 - visits") +
    scale_y_continuous(limits = c(0, 6) , 
                       breaks = seq(0, 6, by = 2))+
    coord_cartesian(xlim = c(0, 400)) +
    hrbrthemes::theme_ipsum() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
      axis.title = element_text(hjust = 0.5, size = 10),
      panel.spacing = unit(0.1, "lines")
    ))

# save the plot
ggsave(here::here(plotFolder, "cohort2_distribution_of_visits.PNG"),
       plot3, width = 6, height = 4)


# cohort1_matched_from ----
cohort1_matched_from_temp <- analyse_visits(cohort1_matched_from, visit_data = cdm[["visit_data"]])
cohort1_matched_from_temp2 <- cohort1_matched_to_temp$visits_count_wide

# replace NAs with 0
cohort1_matched_from_temp3 <- cohort1_matched_from_temp2 %>% 
  mutate(across(everything(), .fns = ~replace_na(.,0)))

# columns to sum row-wise
columns_to_sum_cohort1_matched_from1 <- colnames(cohort1_matched_from_temp3)[(colnames(cohort1_matched_from_temp3)%in% specialty_names)]

# adding a new column with row-wise sum of selected columns
cohort1_matched_from_temp3 <- cohort1_matched_from_temp3 %>% 
  dplyr::select(-subject_id, -exposed_yrs) %>% 
  dplyr::mutate_if(is.numeric,as.integer)

cohort1_matched_from_temp4 <- cohort1_matched_from_temp3 %>%
  dplyr::mutate(total_visits_per_w  = rowSums(select(., all_of(columns_to_sum_cohort1_matched_from1))),
         cohort = "cohort1_matched_from")

# calculate the percentage of subjects for each total visit count
cohort1_matched_from_temp5 <- cohort1_matched_from_temp4 %>%
  dplyr::group_by(total_visits_per_w) %>%
  dplyr::summarize(Subject_Count = n(), .groups = 'drop') %>%
  dplyr::mutate(Percent_Subjects = Subject_Count / sum(Subject_Count) * 100)

# plotting - target matched ----
(plot3.1 <- cohort1_matched_from_temp5 %>% 
   ggplot(aes(x = total_visits_per_w, y = Percent_Subjects)) +
   geom_bar(stat = "identity", fill="#8B4C39", alpha=0.9) +
   labs(x = "Total Number of Visits", y = "% of Subjects (entries)",
        title = "Cohort 1 - visits") +
   scale_y_continuous(limits = c(0, 6) , 
                      breaks = seq(0, 6, by = 2))+
   coord_cartesian(xlim = c(0, 400)) +
   hrbrthemes::theme_ipsum() +
   theme(
     plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
     axis.title = element_text(hjust = 0.5, size = 11),
     panel.spacing = unit(0.1, "lines")
   ))

# let's combine the two bar graphs
(plot_combo3 <- plot3 + plot3.1)

ggsave(here::here(plotFolder, "cohort2-matched_from_cohort1_distribution_of_visits.PNG"),
       plot_combo3, width = 8, height = 6)

suppressWarnings(
  rm(all_summary,
     cohort_combined,
     cohort1_comp1_non_service_users,
     cohort1_comp1_results_all,
     cohort1_comp1_results_all_cost,
     cohort1_comp1_results,
     cohort1_comp2_results_all,
     cohort1_comp2_non_service_users,
     cohort1_comp1_summary,
     cohort1_comp1_results_user,
     cohort1_comp1_results_user_cost,
     cohort1_matched_from,
     cohort1_matched_to,
     cohort1_comp1_results_cost,
     cohort1_comp2_results,
     cohort1_comp2_results_cost,
     cohort1_comp2_results_all_cost,
     cohort1_comp2_results_user,
     cohort1_comp2_results_user_cost,
     cohort1_comp2_summary,
     cohort2_matched,
     cohort2_non_service_users,
     cohort2_results_all,
     cohort2_results_all_cost,
     cohort2_results_user,
     cohort2_results_user_cost,
     cohort2_summary,
     cohort2_results,
     cohort2_results_cost,
     non_service_users,
     summary_cohort_comp1,
     summary_cohort_comp2,
     target_non_service_users,
     target_matched,
     target_results_all,
     target_results_all_cost,
     target_results_user,
     target_results_user_cost,
     target_summary,
     target_results,
     target_results_cost,
     targetCohort,
     user_only_summary,
     bx_plot1,
     bx_plot2,
     bx_plotly1,
     bx_plotly2,
     plot_combo1,
     plot_combo2,
     plot_combo3,
     plot1,
     plot1.1,
     plot2,
     plot2.1,
     plot3,
     plot3.1,
     cohort1_matched_from_temp2,
     cohort1_matched_from_temp3,
     cohort1_matched_from_temp4,
     cohort1_matched_from_temp5,
     cohort1_matched_to_temp2,
     cohort1_matched_to_temp3,
     cohort1_matched_to_temp4,
     cohort1_matched_to_temp5,
     cohort1_specialty_visits_top10,
     cohort1_temp2,
     cohort1_temp3,
     cohort1_temp4,
     cohort1_temp5,
     cohort1_temp6,
     cohort1_temp6_top10,
     cohort1_matched_from_temp5,
     cohort1_matched_from_temp4,
     cohort1_matched_from_temp3,
     cohort1_matched_from_temp2,
     cohort1_temp6_top10,
     cohort1_specialty_visits_top10,
     cohort1_temp6,
     cohort1_temp5,
     cohort1_temp4,
     cohort1_temp3,
     cohort1_temp2,
     cohort1_matched_to_temp5,
     target_specialty_visits_top10,
     target_temp2,
     target_temp3, 
     target_temp4,
     target_temp5,
     target_temp6,
     target_temp6_top10,
     target_temp,
     cohort1_matched_from_temp5,
     cohort1_matched_from_temp4,
     cohort1_matched_from_temp3,
     cohort1_matched_from_temp2,
     cohort1_temp6_top10,
     cohort1_specialty_visits_top10,
     cohort1_temp6,
     cohort1_temp5,
     cohort1_temp4,
     cohort1_temp3,
     cohort1_temp2,
     cohort1_matched_to_temp5,
     cohort1_matched_to_temp4,
     cohort1_matched_to_temp3,
     cohort1_matched_to_temp2,
     cohort1_matched_from_temp5,
     cohort1_matched_from_temp4,
     cohort1_matched_from_temp3,
     cohort1_matched_from_temp2,
     cohort1_temp6_top10,
     cohort1_specialty_visits_top10,
     cohort2_temp2,
     cohort2_temp3,
     cohort2_temp4,
     cohort2_temp5,
     cohort1_matched_from_temp,
     cohort1_matched_to_temp,
     cohort1_temp,
     cohort2_temp
     )
)
