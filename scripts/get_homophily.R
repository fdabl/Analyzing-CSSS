##Homophily measures by person grouped by discipline 
#- to see which disciplines are more likely to work interdisciplinarily

library(dplyr)
library(tidyr)
library(DiagrammeR)
library(ggplot2)
source("ACSSS/R/build_edge_dataframe.R")
source("ACSSS/R/build_node_dataframe.R")
source("scripts/clean_raw_data.R")
source("ACSSS/R/homophily_functions.R")

data <- read.csv("data/raw/cleaned_csss-all.csv")
data <- data %>% filter(Year != 2011)
processed <- clean_raw_data(data)
processed <- processed %>% 
  mutate(Iteration = str_c(Year, Location, sep = ".")) %>%
  mutate(Topic_isced = topic1, 
         Discipline_isced = discp1)

get_homophily_by_year <- function(df, iter) {
  edges <- build_edge_dataframe(df, iter)
  nodes <- build_node_dataframe(df, iter)
  
  hhi <- get_hhi(nodes, edges, "discp")
  colnames(hhi) <- c("id", "hhi")
  ps <- get_perc_sim(nodes, edges, "discp")
  colnames(ps) <- c("id", "ps")
  
  df <- nodes %>% left_join(hhi, by = "id") %>%
    left_join(ps, by = "id")
  return(df)
}

get_all_homophily <- function(df) {
  iters <- unique(df$Iteration)
  all.hmph <- get_homophily_by_year(df, iters[1])
  for(i in 2:length(iters)) {
    all.hmph <- rbind(all.hmph, get_homophily_by_year(df, iters[i]))
  }
  all.hmph <- all.hmph %>%
    mutate(year = str_replace(year, "[.]", ",")) %>%
    separate(year, c("year", "location"), sep = ",") %>%
    filter(location == "Santa Fe", year != "2005")
  return(all.hmph)
}

all.hmph <- get_all_homophily(processed)

plot_hhi_ps <- function(all.hmph) {
  graph.df <- all.hmph %>% 
    select(year, location, hhi, ps, discp) %>%
    filter(discp != "") %>%
    filter(!is.nan(hhi) | !is.nan(ps)) %>%
    mutate(hhi_norm = hhi/10000) %>%
    gather(type, value, hhi_norm, ps)
    
  labels <- c(hhi_norm = "HHI", ps = "Percent Similar")
  dlevels <- graph.df %>% 
    filter(type == "hhi_norm") %>%
    group_by(discp) %>%
    summarize(med.hhi = median(value, na.rm = T)) %>%
    arrange(med.hhi) %>%
    pull(discp)
    
  return(ggplot(graph.df, aes(x=discp, y=value, fill=discp, group=discp)) +
           geom_boxplot(aes(middle = median(value)), outlier.shape = NA) +
           geom_jitter(color = "black", size = 0.6, alpha = 0.6) +
           theme_bw() +
           facet_grid(type ~ ., switch = "both", labeller = labeller(type = labels)) +
           guides(fill = F) +
           theme(axis.text.x = element_text(angle = 32, hjust = 1, size = 7), 
                 strip.placement = "outside") +
           labs(x = "", y = "") +
           scale_x_discrete(limits = dlevels)
    
  )
}  
#plot(plot_hhi_ei(all.hmph))
ggsave("figures/homophily_by_discp.png", plot_hhi_ps(all.hmph), 
       width = 5, height = 3, scale = 1.75)
