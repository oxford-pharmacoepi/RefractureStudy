(test <- target_condition$freq_condition %>% 
  dplyr::select(condition_source_value, counts) %>% 
  dplyr::slice(1:10) %>% 
  dplyr::mutate(counts = as.numeric(counts)))

(bx_plot10 <- ggplot(data = test,
                    aes(x=reorder(condition_source_value, counts), y=counts, fill=condition_source_value)) +
    geom_boxplot() + # outlier.shape = NA removes outliers
    coord_flip() +
    scale_y_continuous(limits = c(0,10000)) +
    # stat_summary(fun = "mean", geom = "point", shape = 2, size = 1.2, color = "red") +
    viridis::scale_fill_viridis(discrete = T, alpha = 0.7, option = "D") +
    labs(title = "Top10 specialties visited by Target Cohort in the T-C1 matching", 
         subtitle = "Only service users & absence of outliers",
         x = "Specialties", y = "Number of visits") +
    theme_minimal() +
    theme(
      legend.position="none",
      plot.title = element_text(size=14, hjust = 0.5),
      axis.text.x = element_text(angle = 45)
    ) 
)
