library(dplyr)
library(stringr)
library(ggplot2)
library(khroma)

# ---- ordering ----

global_levels <- c(
  "France (LPD)",
  "Germany (IQVIA DA)",
  "Italy (LPD)",
  "Netherlands (IPCI)",
  "Spain (SIDIAP)",
  "UK (CPRD)",
  "Spain (CMBD)",
  "UK (HES)"
)

outcome_levels <- c("Hip", "NHNV", "Vertebral")

pretty_db <- function(x) gsub(" \\(", "\n(", x)

prep_data <- function(df) {
  df %>%
    mutate(Database = str_replace(Database, ",\\s*(primary care|hospital)", "")) %>%
    mutate(
      Database = factor(Database, levels = global_levels),
      outcome  = factor(outcome, levels = outcome_levels)
    ) %>%
    filter(!is.na(est))
}

df_long1_plot <- prep_data(df_long1)
df_long2_plot <- prep_data(df_long2)
df_long3_plot <- prep_data(df_long3)

all_hi <- c(df_long1_plot$hi, df_long2_plot$hi, df_long3_plot$hi)
y_max <- max(all_hi, na.rm = TRUE)

# colour-blind friendly palette
db_cols <- khroma::color("muted")(length(global_levels))
names(db_cols) <- global_levels

plot_bar_figure <- function(dfin, outfile) {
  
  p <- ggplot(dfin, aes(x = outcome, y = est, fill = Database)) +
    geom_col(
      position = position_dodge(width = 0.8),
      width = 0.7,
      colour = "black",
      linewidth = 0.25
    ) +
    geom_errorbar(
      aes(ymin = lo, ymax = hi),
      position = position_dodge(width = 0.8),
      width = 0.2,
      linewidth = 0.8,
      colour = "black"
    ) +
    scale_fill_manual(
      values = db_cols,
      breaks = global_levels,
      limits = global_levels,
      drop = FALSE
    ) +
    scale_y_continuous(
      limits = c(0, y_max * 1.05),
      expand = expansion(mult = c(0, 0.05))
    ) +
    labs(
      title = NULL,
      x = NULL,
      y = "Cumulative Incidence (%)",
      fill = "Database"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
      axis.text.x = element_text(face = "bold", size = 14),
      axis.text.y = element_text(size = 12),
      axis.title.y = element_text(face = "bold", size = 14),
      panel.grid.major.x = element_blank(),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.text = element_text(size = 10),
      legend.key.height = unit(0.4, "cm"),
      legend.key.width = unit(0.6, "cm")
    ) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE))
  
  ggsave(
    outfile,
    p,
    width = 12,
    height = 8,
    dpi = 600,
    bg = "white"
  )
  
  p
}

figure1 <- plot_bar_figure(df_long1_plot, "figure1_bar.png")
figure2 <- plot_bar_figure(df_long2_plot, "figure2_bar.png")
figure3 <- plot_bar_figure(df_long3_plot, "figure3_bar.png")
