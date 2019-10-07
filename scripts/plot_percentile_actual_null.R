#!/usr/bin/env RScript
#
# This script will plot the histogram of the null model
library(dplyr)
library(ggplot2)


# Parse command line arguments
args <- commandArgs(trailingOnly=TRUE)

# Load the node data
path.to.file <- args[1]
percentiles <- read.csv(path.to.file)

# Second argument indicates the attribute of interest
attribute <- args[2]


percentile_plot <- percentiles %>%
  # First filter out the beijing data, its a little messy for the current plots
  filter(!iter %in% c("2005.Beijing","2006.Beijing", "2007.Beijing")) %>%
  # Then select the attribute of interest
  filter(attr == attribute) %>%
  rowwise() %>%
  mutate(
    Year = unlist(strsplit(as.character(iter), ".", fixed = T))[1]
  ) %>%
  ggplot(aes(x = Year, y = percentile, color = func, shape = func, group = func)) +
    geom_point(size = 2) +
    geom_line() +
    facet_wrap(~func, ncol = 1) +
    scale_color_discrete(name = "Homophily function", labels = c("E-I", "HHI", "Percent Similarity")) +
    scale_shape_discrete(name = "Homophily function", labels = c("E-I", "HHI", "Percent Similarity")) +
    theme_minimal() +
    theme(
      legend.position = "right",
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    labs(y = "Percentile of actual vs. null")


# Output file
path.to.output <- args[3]
# Save the figure
ggsave(path.to.output, percentile_plot, height = 5, width = 6)
