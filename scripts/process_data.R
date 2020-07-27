#!/usr/bin/env RScript
#
# This is a simple script to process the ACSSS data and output the result

# Parse command line arguments
args <- commandArgs(trailingOnly=TRUE)

# the path to the data should be the first command line argument
path_to_data <- args[1]

# And output path is the second argument
output_path <- args[2]

# Read in the raw data
raw_data <- read.csv(path_to_data, na.strings = '')

print(names(raw_data))
# Perform the processing using the function in the ACSSS package
processed <- ACSSS::process_acsss_data(raw_data)

# And finally write the file
write.csv(processed, file = output_path)
