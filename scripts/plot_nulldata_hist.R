#!/usr/bin/env RScript
#
# This script will plot the histogram of the null model
library(dplyr)
library(ACSSS)
library(ggplot2)


# Parse command line arguments
args <- commandArgs(trailingOnly=TRUE)

# Load the node data
path.to.file <- args[1]
nulldata <- read.csv(path.to.file)

# Extract the actual value  
actual <- (nulldata %>% filter(actual == T))$value[1]

nulldata %>%
  filter(actual == F) %>%
  ggplot(aes(x = value)) +
    geom_histogram(bins = 20, alpha = 0.8, color = "black") +
    geom_vline(xintercept = actual, color = "dodgerblue", size = 1.5, alpha = 0.8) +
    theme_minimal()

# Output file
path.to.output <- args[2]
# Save the figure
ggsave(path.to.output, height = 5, width = 6)