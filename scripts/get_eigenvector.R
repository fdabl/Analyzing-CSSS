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

get_prop_people_by_year <- function(df) {
  prop.df <- df %>% filter(Location == "Santa Fe", Year != "2005") %>%
    group_by(Year, discp1) %>%
    summarize(n.ind = n_distinct(Name)) %>%
    group_by(Year) %>% 
    mutate(total = sum(n.ind)) %>%
    mutate(prop = n.ind/total)
}

get_all_eigencent <- function(df) {
  iters <- unique(df$Iteration)
  all.ec <- get_eigen_by_year(df, iters[1])
  for(i in 2:length(iters)) {
    all.ec <- rbind(all.ec, get_eigen_by_year(df, iters[i]))
  }
  all.ec <- all.ec %>%
    mutate(iteration = str_replace(iteration, "[.]", ",")) %>%
    separate(iteration, c("year", "location"), sep = ",") %>%
    filter(location == "Santa Fe", year != "2005")
  return(all.ec)
}
all.ec <- get_all_eigencent(processed)

#get separate graph for eigencentrality scores
plot_eigen_lines <- function(df) {
  graph.df <- df %>%
    filter(discp %in% c("Computing", "Life sciences", "Physical sciences", "Social and behavioral sciences", 
                        "Mathematics and statistics", "Engineering and engineering trades"))
  return(ggplot(data = graph.df) +
           geom_line(aes(x = year, y = mean.ec, color = discp, group = discp)) +
           geom_point(aes(x = year, y = mean.ec, color = discp, group = discp), size = 3) +
           scale_color_discrete(name = "Discipline") +
           labs(x = "Year", y = "Eigencentrality", title = "Eigencentrality of disciplines") +
           theme_bw()
         )
}

#get separate graph for proportion of participant lines
plot_prop_lines <- function(df) {
  graph.df <- df %>%
    filter(discp1 %in% c("Computing", "Life sciences", "Physical sciences", "Social and behavioral sciences", 
                        "Mathematics and statistics", "Engineering and engineering trades"))
  return(ggplot(data = graph.df) +
           geom_line(aes(x = Year, y = prop, color = discp1, group = discp1)) +
           geom_point(aes(x = Year, y = prop, color = discp1, group = discp1), size = 3) +
           scale_color_discrete(name = "Discipline") +
           labs(x = "Year", y = "Proportion of participants", title = "Proportion of participants by disciplines") +
           theme_bw()
  )
}

plot_eigen_and_prop <- function(all.ec, prop.discp) {
  graph.df <- merge(all.ec, prop.discp, by = c("year", "discp")) %>%
    gather(type, value, mean.ec, prop) %>%
    select(year, discp, type, value) %>%
    filter(discp %in% c("Computing", "Life sciences", "Physical sciences", "Social and behavioral sciences", 
                         "Mathematics and statistics", "Engineering and engineering trades"))
  
  labels <- c(mean.ec = "Eigencentrality", prop = "Proportion of participants")
  return(ggplot(graph.df, aes(x=year, y=value, color = discp, group = discp)) +
           geom_line() +
           geom_point(size = 3) +
           facet_grid(type ~ ., labeller = labeller(type = labels)) +
           scale_color_discrete(name = "Discipline") +
           labs(x = "Year", y = "", title = "Eigencentrality and proportion of participants by discipline ") +
           theme_bw()
         )
}

all.ec <- get_all_eigencent(processed)
prop.discp <- get_prop_people_by_year(processed) %>%
  mutate(discp = discp1, year = Year)
#joined <- merge(all.ec, prop.discp, by = c("year", "discp"))

ggsave("figures/eigencentrality.png", plot_eigen_lines(all.ec), 
       width = 5, height = 2.5, scale = 1.75)
ggsave("figures/eigen_prop-of-people.png", plot_eigen_and_prop(all.ec, prop.discp), 
       width = 6.5, height = 4, scale = 1.75)

#not used in 
plot(plot_avg_eigen(all.ec))

