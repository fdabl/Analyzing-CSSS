#get eigenvector centrality
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

####Create Eigencentrality Dataframes####
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

get_eigen_by_node = function(df) {
  iters <- unique(df$Iteration)
  edges <- build_edge_dataframe(df, iters[1])
  nodes <- build_node_dataframe(df, iters[1])
  graph <- create_graph(nodes, edges)
  ec.nodes <- nodes %>%
    left_join(get_eigen_centrality(graph), by = "id")
  
  for(i in 2:length(iters)) {
    edges <- build_edge_dataframe(df, iters[i])
    nodes <- build_node_dataframe(df, iters[i])
    graph <- create_graph(nodes, edges)
    nodes <- nodes %>%
      left_join(get_eigen_centrality(graph), by = "id")
    ec.nodes = rbind(ec.nodes, nodes)
    
  }
  return(ec.nodes)
}

node.ec <- get_eigen_by_node(processed)
hist(node.ec$eigen_centrality)

fit1 = lm(eigen_centrality ~ as.factor(discp) + as.factor(pos.var) + as.factor(prstg) + as.factor(gender),
          data = node.ec, na.action = na.omit)
plot(fit1, which = 2)
plot(fit1, which = 1)
summary(fit1)
#OLS is not best because distribution of eigencentrality values is not normal - need to figure out a better regression method

# fitgamma = glm(eigen_centrality ~ as.factor(discp) + as.factor(pos.var) + as.factor(prstg) + as.factor(gender), 
#                data = node.ec, na.action = na.omit, family = Gamma(link = "identity"))

####Plotting Functions####
plot_eigen_lines <- function(df) {
  graph.df <- df %>%
    filter(discp %in% c("Computing", "Life sciences", "Physical sciences", "Social and behavioral sciences", 
                        "Mathematics and statistics", "Engineering and engineering trades"))
  return(ggplot(data = graph.df) +
           geom_line(aes(x = year, y = mean.ec, color = discp, group = discp)) +
           geom_point(aes(x = year, y = mean.ec, color = discp, group = discp), size = 3) +
           scale_color_discrete(name = "Discipline") +
           labs(x = "Year", y = "Eigencentrality", title = "Eigencentrality of disciplines") +
           theme_minimal()
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
           theme_minimal()
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
           facet_grid(type ~ ., labeller = labeller(type = labels), switch = "both") +
           scale_color_brewer(name = "Discipline", palette = "Dark2") +
           labs(x = "Year", y = "") +
           theme_minimal() +
           theme(strip.placement = "outside", panel.spacing = unit(2, "lines"))
  )
}

#put facet labels on the other side
plot_all_eigen <- function(df) {
  df <- df %>% filter(discp != "")
  return(ggplot(df, aes(x=year, y=mean.ec, group=discp)) +
           geom_line() +
           geom_point(size = 2) +
           facet_wrap(~ discp) + 
           theme_minimal() +
           labs(x = "Year", y = "Eigencentrality") +
           theme(axis.text.x = element_text(angle = 90), panel.spacing = unit(2, "lines"))
  )
}

all.ec <- get_all_eigencent(processed)
prop.discp <- get_prop_people_by_year(processed) %>%
  mutate(discp = discp1, year = Year)

ggsave("figures/eigencentrality.png", plot_eigen_lines(all.ec), 
       width = 5, height = 2.5, scale = 1.75)
ggsave("figures/eigen_prop-of-people.png", plot_eigen_and_prop(all.ec, prop.discp), 
       width = 6.5, height = 4, scale = 1.75)
ggsave("figures/all-discp_eigen-values.png", plot_all_eigen(all.ec), 
       width = 8, height = 5.5, scale = 1.25)

