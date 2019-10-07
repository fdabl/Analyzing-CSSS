#!/usr/bin/env RScript
#
# Plots a point and line graph that shows the proportion of women over each iteration
# of the summer school

library(ggplot2)
library(dplyr)

# Parse command line arguments
args <- commandArgs(trailingOnly=TRUE)

path_to_data <- args[1]
data <- read.csv(path_to_data)

output_path <- args[2]

# Summarize the data and produce the figure
fig <- data %>%
  group_by(Iteration_display) %>%
  summarize(
    prop_female = mean(Gender == 'Female', na.rm = TRUE)
  ) %>%
  ggplot(aes(x = Iteration_display, y = prop_female, group = 1)) +
    stat_summary(fun.y=sum, geom="line", color = "grey") +
    geom_point(color = "orange", size = 3) +
    scale_y_continuous(limits = c(0, .6), breaks = scales::pretty_breaks(n = 10)) +
    geom_hline(yintercept = .5, linetype = 'dashed', size = 1.2) +
    geom_text(x = 8, y = 0.55, label = "Parity") +
    ggtitle('Proportion of Women across CSSS Iterations') +
    ylab('% Women') +
    xlab('') +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, size = 12, vjust = 1, hjust = 1),
      axis.text.y = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      plot.title = element_text(hjust = .5, size = 14, face = "bold")
    )

# Save to the specified output path
ggsave(output_path, fig, height = 5, width = 6)
