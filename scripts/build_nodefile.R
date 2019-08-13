#!/usr/bin/env RScript
#
# This is a simple script to produce the nodefile

# Parse command line arguments
args <- commandArgs(trailingOnly=TRUE)

path_to_data <- args[1]
data <- read.csv(path_to_data)

output_path <- args[2]

# This extracts, from the filename, the iteration that we will be making
iter_var <- unlist(strsplit(basename(output_path), "_"))[1]

# Create the nodefile using the appropriate ACSSS function
node_df <- ACSSS::build_node_dataframe(data, iter_var)

# Finally, save the file
write.csv(node_df, file = output_path)
