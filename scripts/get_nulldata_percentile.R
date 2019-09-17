#!/usr/bin/env RScript
#
# This script calculate all the percentile values comparing the actual
# value to the null distribution
library(dplyr)

# Parse command line arguments
args <- commandArgs(trailingOnly=TRUE)

# Load the node data
path.to.file <- args[1]
all_nulldata <- read.csv(path.to.file)

all_percentile <- all_nulldata %>%
  group_by(iter, attr, func) %>%
  summarize(
    percentile = ACSSS::get_percentile(value[2:n()], value[1])
  )

# Output file
path.to.output <- args[2]
# Save the figure
write.csv(all_percentile, path.to.output)
