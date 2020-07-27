#!/usr/bin/env RScript
#
# This script will generate the null model data
library(dplyr)
library(ACSSS)

# The number of times to shuffle and generate a distribution
NUM_ITERATIONS <- 50
ATTRS <- c("gender", "prstg", "discp", "pos.var", "cntry")
EXP <- c("Male", "Top 50", "Physical and Natural Science", "academia", "USA")

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

# The path to save the output
path.to.output <- last(args)

# helper function to sample from the cross-links
samplemean <-function(x, d) {
  return(mean(x[d]))
}

df <- data.table::rbindlist(lapply(1:length(ATTRS), function(i) {
  attr.i <- ATTRS[i]
  # Build the limited node dataframe
  nodes.attr <- nodes %>%
    select(id, attr.i) %>%
    rename(attribute = attr.i)

  df <- data.frame(
    attr = attr.i,
    actual = NA,
    expected = NA,
    ci.lower = NA,
    ci.upper = NA
  )

  # If there are no non-null values,
  if (!any(!is.na(nodes.attr$attribute))) {
    return(df)
  } else {

    edge.links <- edges %>%
      left_join(nodes.attr, by = c("from" = "id")) %>%
      left_join(nodes.attr, by = c("to" = "id")) %>%
      mutate(link = attribute.x != attribute.y)

    # get the vector of cross-links
    cross.links <- as.numeric(edge.links$link)

    # Treat missing cross-links as non-links
    cross.links[is.na(cross.links)] <- 0
    # Calculate the proportion of edge links we expect, given the number of nodes
    prop.expected <- sum(as.numeric(nodes.attr$attribute == EXP[i]), na.rm = T) / dim(nodes)[1]
    # then calculate test statistic
    test.statistic <- 2 * prop.expected * (1 - prop.expected)

    # get the actual number
    df$actual <- mean(cross.links)
    df$expected <- test.statistic

    # If the data is completely homogeneous, return a blank row
    if (df$actual == 1) {
      return(df)
    }


    # obtain bootstrap estimates of the standard deviation of the distribution# of the mean:
    b <- boot::boot(cross.links, samplemean, R = NUM_ITERATIONS)
    ci <- boot::boot.ci(b, type = "basic") # The bootstrap CI

    df$ci.lower <- ci$basic[4]
    df$ci.upper <- ci$basic[5]

    return(df)
  } # end else
} # end function
) # end lapply
) # end rbindlist


# Save the file
write.csv(df, path.to.output)
