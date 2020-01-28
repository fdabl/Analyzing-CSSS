###disiplines per group
library(dplyr)
library(ggplot2)
library(tidyr)
#source("ACSSS/R/process_acsss_data.R")
source("scripts/clean_raw_data.R")

data <- read.csv("data/raw/cleaned_csss-all.csv")
data <- data %>% filter(Year != 2011)
processed <- clean_raw_data(data)

count_disciplines_per_group <- function(data) {
  data <- data %>% filter(discp1 != "")
  discp.count <- data %>% group_by(Title) %>%
    summarize(count1 = n_distinct(discp1),
              count.p = n_distinct(Name),
              year = first(Year), 
              topic1 = first(topic1),
              topic2 = first(topic2),
              size = first(Size),
              Location = first(Location), 
              Year = first(Year)) %>%
    unite(Iteration_display, Year, Location, sep = " ")
  #discp.count$total.count <- discp.count$count1 + discp.count$count2
  return(discp.count)
}


#not used in SFI paper
plot_discp_counts_per_year <- function(counts) {
  counts <- counts %>%
    filter(!str_detect(Iteration_display, "Beijing"))
  return(ggplot(counts, aes(x = Iteration_display, y = count1, fill = Iteration_display)) +
           geom_boxplot(outlier.shape = NA) +
           geom_jitter(color = "black", size = 0.6, alpha = 0.6) +
           theme_bw() +
           #coord_flip() +
           guides(fill = F, alpha = F) +
           labs(y = "Unique Disciplines", x = "", 
                title = "Number of unique disciplines in a group")
  )
}

plot_discp_counts_by_topic <- function(counts) {
  counts <- counts %>% filter(topic1 != "")
  counts[counts$count1 == 1 & counts$count.p != 1,]$count1 = 0
  counts$prop.dif <- counts$count1/counts$size
  return(ggplot(data = counts, aes(x = reorder(topic1, prop.dif, FUN=median), y = prop.dif, group = topic1, fill = topic1)) +
           geom_boxplot(outlier.shape = NA) +
           geom_jitter(color = "black", size = 0.6, alpha = 0.6) +
           theme_minimal() +
           #coord_flip() +
           guides(fill = F, alpha = F) +
           labs(y = "Proportion of unique disciplines per group", x = "") +
           theme(axis.text.x = element_text(angle = 32, hjust = 1))

         )
}

counts <- count_disciplines_per_group(processed)
#plot_discp_counts_by_topic(counts)
# ggsave("figures/counts_per_year.png", plot_discp_counts_per_year(counts), 
#        width = 3.35, height = 3.89, scale = 1.75)
ggsave("figures/counts_by_topic.png", plot_discp_counts_by_topic(counts), 
       width = 5, height = 3, scale = 1.75, dpi = "print")

# t.test(counts$count1[counts$topic1 == "Agriculture and forestry and fishery"], 
#        counts$count1[counts$topic1 == "Architecture and building"])
