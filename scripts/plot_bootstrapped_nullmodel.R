#!/usr/bin/env RScript
#
library(dplyr)
library(ggplot2)

TO_FILTER <- c("2005.Beijing","2006.Beijing", "2007.Beijing")

# Parse command line arguments
args <- commandArgs(trailingOnly=TRUE)

# Load the node data
path.to.file <- first(args)
bootdata <- read.csv(path.to.file)

print(levels(bootdata$attr))
bootplot <- bootdata %>%
  filter(!iter %in% TO_FILTER) %>%
  mutate(
    attr = factor(attr, labels = c("Country", "Discipline", "Gender", "Position", "Prestige"))
  ) %>%
  rowwise() %>%
  mutate(
    Year = unlist(strsplit(as.character(iter), ".", fixed = T))[1]
  ) %>%
  ggplot(aes(x = as.numeric(Year), y = actual)) +
  geom_errorbar(aes(ymin = ci.lower, ymax = ci.upper), width = 0.4) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = c(2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019)) +
  geom_segment(aes(x = as.numeric(Year) - 0.4, xend = as.numeric(Year) + 0.4, y = expected, yend = expected), color = "orange", size = 1) +
  facet_wrap(~attr, ncol = 1) +
  guides(color = F) +
  labs(x = "Year",
       y = "Actual") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11),
    strip.text = element_text(face = "bold", size = 12)
  )

# Output file
path.to.output <- last(args)

# Save the figure
ggsave(path.to.output, bootplot, height = 10, width = 8)
