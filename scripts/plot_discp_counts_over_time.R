###disiplines per group
library(dplyr)
library(ggplot2)
library(tidyr)
#source("ACSSS/R/process_acsss_data.R")
source("scripts/clean_raw_data.R")

data <- read.csv("data/raw/cleaned_csss-all.csv")
data <- data %>% filter(Year != 2011)
processed <- clean_raw_data(data)
groups <- processed %>% group_indices(Title)

count_disciplines_per_group <- function(data, groups) {
  data$group.id <- groups
  discp.count <- data %>% group_by(group.id) %>%
    summarize(count1 = n_distinct(discp1),
              #count2 = n_distinct(discp2),
              year = first(Year), 
              topic1 = first(topic1),
              topic2 = first(topic2), 
              Location = first(Location), 
              Year = first(Year)) %>%
    unite(Iteration_display, Year, Location, sep = " ")
  #discp.count$total.count <- discp.count$count1 + discp.count$count2
  return(discp.count)
}



plot_discp_counts_per_year <- function(counts) {
  counts <- counts %>%
    filter(!str_detect(Iteration_display, "Beijing"))
  return(ggplot(counts, aes(x = Iteration_display, y = count1, fill = Iteration_display)) +
           geom_boxplot(outlier.shape = NA) +
           geom_jitter(color = "black", size = 0.6, alpha = 0.6) +
           theme_bw() +
           coord_flip() +
           guides(fill = F, alpha = F) +
           labs(y = "Unique Disciplines", x = "", 
                title = "Number of unique disciplines in a group")
  )
}

plot_discp_counts_by_topic <- function(counts) {
  return(ggplot(data = counts, aes(x = topic1, y = count1, group = topic1, color = topic1)) +
           geom_jitter() +
           geom_boxplot(alpha = 0.5) +
           theme_bw() +
           coord_flip() +
           guides(color = F, alpha = F) +
           labs(y = "Unique Disciplines", x = "", 
                title = "Number of average unique disciplines")
         )
}

counts <- count_disciplines_per_group(processed, groups)
ggsave("figures/counts_per_year.png", plot_discp_counts_per_year(counts), 
       width = 3.35, height = 3.89, scale = 1.75)
ggsave("figures/counts_by_topic.png", plot_discp_counts_by_topic(counts), 
       width = 3.75, height = 3.89, scale = 1.75)

