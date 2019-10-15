#get eigenvector centrality

##preliminary playing around with data -- need to refamiliarize with DiagrammeR
source("ACSSS/R/build_edge_dataframe.R")
source("ACSSS/R/build_node_dataframe.R")
source("scripts/clean_raw_data.R")
library(dplyr)
library(tidyr)
library(DiagrammeR)
library(ggplot2)

data <- read.csv("data/raw/cleaned_csss-all.csv")
data <- data %>% filter(Year != 2011)
processed <- clean_raw_data(data)
processed <- processed %>% 
  mutate(Iteration = str_c(Year, Location, sep = ".")) %>%
  mutate(Topic_isced = topic1, 
         Discipline_isced = discp1)

get_eigen_by_year <- function(df, iter) {
  edges <- build_edge_dataframe(df, iter)
  nodes <- build_node_dataframe(df, iter)
  
  graph <- create_graph(nodes, edges)
  nodes <- nodes %>%
    left_join(get_eigen_centrality(graph), by = "id")
  ec <- nodes %>%  
    group_by(discp) %>%
    summarize(mean.ec = mean(eigen_centrality), 
              max.ec = max(eigen_centrality), 
              min.ec = min(eigen_centrality)) %>%
    arrange(discp) %>%
    mutate(iteration = iter)
  return(ec)
}

get_all_eigencent <- function(df) {
  iters <- unique(df$Iteration)
  all.ec <- get_eigen_by_year(df, iters[1])
  for(i in 2:length(iters)) {
    all.ec <- rbind(all.ec, get_eigen_by_year(df, iters[i]))
  }
  return(all.ec)
}
all.ec <- get_all_eigencent(processed)

plot_eigen_lines <- function(df) {
  graph.df <- df %>%
    filter(discp %in% c("Computing", "Humanities", "Life sciences", "Physical sciences", "Social and behavioral sciences"))
  return(ggplot(data = graph.df) +
           geom_line(aes(x = iteration, y = mean.ec, color = discp, group = discp))
         )
}



