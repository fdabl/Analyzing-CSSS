#!/usr/bin/env RScript
#
# This script will generate the null model data
library(dplyr)
library(ACSSS)

# The number of times to shuffle and generate a distribution
NUM_ITERATIONS = 500

# Parse command line arguments
args <- commandArgs(trailingOnly=TRUE)


# Load the node data
path.to.nodes <- args[1]
nodes <- read.csv(path.to.nodes) %>%
  select(-X)

# Load the edge data
path.to.edges <- args[2]
edges <- read.csv(path.to.edges) %>%
  select(-X)

# The column that will be processed
column <- args[3]

# homophily function: set the correct function to use
func_name <- args[4]
func <- NULL
if (func_name == "ei") {
  func <- ACSSS::get_ei
} else if (func_name == "hhi") {
  func <- ACSSS::get_hhi
} else if (func_name == "perc_sim") {
  func <- ACSSS::get_perc_sim
} else {
  stop(paste0("Error: \"", func_name, "\" is not a valid homophily function"))
}

# The path to save the output
path.to.output <- args[5]

if (any(!is.na(nodes[[column]]))) {
  # get the avtual value
  actual <- mean(func(nodes, edges, column)$value, na.rm = T)
  actual.df <- data.frame(value = actual, actual = T)
  
  
  # perform a number of iterations of shuffling
  dist <- sapply(c(1:NUM_ITERATIONS), function(x) {
    # Shuffle the node characteristics
    nodes.shuffled <- nodes
    nodes.shuffled[[column]] <- nodes[[column]][sample(nrow(nodes))]
    return(mean(func(nodes.shuffled, edges, column)$value, na.rm = T))
  })
  
  # Combine the actual with the null model data
  df <- rbind(actual.df, data.frame(value = dist, actual = F))
  
  # Save the file
  write.csv(df, path.to.output)
} else {
  # Otherwise, the entire row is blank and no useful data can be obtained
  blank.df <- data.frame(value = 0, actual = F)
  write.csv(blank.df, path.to.output)
}
