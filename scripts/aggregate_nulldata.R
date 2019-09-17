#!/usr/bin/env RScript
#
# Aggregates all the null data into a single file that can be
# more easily worked with for calculations.

library(dplyr)

# Parse command line arguments
args <- commandArgs(trailingOnly=TRUE)

input.paths <- args[1:length(args)[1] - 1]

all_nulldata <- data.table::rbindlist(lapply(input.paths, function(path) {

  # Load the data
  nulldata <- read.csv(path)

  # decompose the path into its components
  path_components <- unlist(strsplit(basename(path), "_"))

  # return the data but with added variables
  return(nulldata %>%
    mutate(
      iter = path_components[1],
      attr = path_components[2],
      func = path_components[3],
    )
  )
}))

# Output file is the last file
path.to.output <- last(args)
# Save the figure
write.csv(all_nulldata, path.to.output)
