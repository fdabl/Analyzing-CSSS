#get eigenvector centrality
source("ACSSS/R/build_edge_dataframe.R")
source("ACSSS/R/build_node_dataframe.R")
source("scripts/clean_raw_data.R")
library(tidyr)
library(DiagrammeR)
library(ggplot2)
#library(lme4)
#library(MASS)
library(dplyr)
#library(censReg)
#library(pscl)

data <- read.csv("data/raw/cleaned_csss-all.csv")
#data <- data %>% filter(Year != 2011)
processed <- clean_raw_data(data)
processed <- processed %>% 
  mutate(Iteration = str_c(Year, Location, sep = ".")) %>%
  mutate(Topic_isced = topic1, 
         Discipline_isced = discp1) %>%
  filter(Location == "Santa Fe")

####Create Eigencentrality Dataframes####

get_eigen_by_year <- function(df, iter, attr = "discp") {
  edges <- build_edge_dataframe(df, iter)
  nodes <- build_node_dataframe(df, iter)
  n = nrow(nodes) -1
  
  graph <- create_graph(nodes, edges)
  nodes <- nodes %>%
    left_join(get_eigen_centrality(graph), by = "id")
    #left_join(get_degree_total(graph), by = "id") %>%
    #mutate(deg_cen = total_degree/n)
  ec <- nodes %>%  
    group_by_at(attr) %>%
    summarize(mean.ec = mean(eigen_centrality),
              max.ec = max(eigen_centrality),
              min.ec = min(eigen_centrality)) %>%
    #summarize(mean.ec = mean(deg_cen)) %>%
    #arrange(discp) %>%
    mutate(iteration = iter)
  return(ec)
}

get_prop_people_by_year <- function(df) {
  prop.df <- df %>% filter(Location == "Santa Fe"
                           #, Year != "2005"
                           ) %>%
    group_by(Year, discp1) %>%
    summarize(n.ind = n_distinct(Name)) %>%
    group_by(Year) %>% 
    mutate(total = sum(n.ind)) %>%
    mutate(prop = n.ind/total)
}

get_all_eigencent <- function(df, attr = "discp") {
  iters <- unique(df$Iteration)
  all.ec <- get_eigen_by_year(df, iters[1], attr)
  for(i in 2:length(iters)) {
    all.ec <- rbind(all.ec, get_eigen_by_year(df, iters[i], attr))
  }
  
  all.ec <- all.ec %>%
    mutate(iteration = str_replace(iteration, "[.]", ",")) %>%
    separate(iteration, c("year", "location"), sep = ",") %>%
    filter(location == "Santa Fe"
           #, year != "2005"
           )
  
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

# node.ec <- get_eigen_by_node(processed)
# hist(node.ec$eigen_centrality)
# node.ec$pos.var = factor(node.ec$pos.var, levels = c("Student", "Postdoc", "Professor", "Researcher", "Government", "Industry", "Medicine", "Other"))
# node.ec$prstg = factor(node.ec$prstg, levels = c("Top 50", "51-100", "101-150", "151-200", "201-300", "301-400", "401-500", "501-600", "Unlisted", "Not University"))
# node.ec$gender = factor(node.ec$gender, levels = c("Male", "Female"))
# #node.ec$discp = factor(node.ec2$discp, levels = c("Physical sciences"))
# node.ec[node.ec == ""] = NA
# node.ec2 = node.ec %>% filter(eigen_centrality != 0)
# hist(node.ec2$eigen_centrality)
# 
# fit1 = glm(eigen_centrality ~ as.factor(discp) + as.factor(pos.var) + as.factor(prstg) + as.factor(gender),
#           data = node.ec, na.action = na.omit)
# plot(fit1, which = 2)
# plot(fit1, which = 1)
# summary(fit1)
# #OLS is not best because distribution of eigencentrality values is not normal - need to figure out a better regression method
# 
# ##Box-Cox power transformation
# bc = boxcox(eigen_centrality ~ as.factor(discp) + as.factor(pos.var) + as.factor(prstg) + as.factor(gender),
#             data = node.ec2, na.action = na.omit)
# lambda = bc$x[which.max(bc$y)]
# 
# node.ec2$ec.transformed = (((node.ec2$eigen_centrality)^lambda)-1)/lambda
# hist(node.ec2$ec.transformed)
# node.ec2[node.ec2 == ""] = NA
# node.ec2$pos.var = factor(node.ec2$pos.var, levels = c("Student", "Postdoc", "Professor", "Researcher", "Government", "Industry", "Medicine", "Other"))
# node.ec2$prstg = factor(node.ec2$prstg, levels = c("Top 50", "51-100", "101-150", "151-200", "201-300", "301-400", "401-500", "501-600", "Unlisted", "Not University"))
# node.ec2$gender = factor(node.ec2$gender, levels = c("Male", "Female"))
# #node.ec2$discp = factor(node.ec2$discp, levels = c("Physical sciences"))
# 
# fit2 = lm(ec.transformed ~ as.factor(discp) + as.factor(pos.var) + as.factor(prstg) + as.factor(gender),
#           data = node.ec2, na.action = na.omit)
# plot(fit2, which = 2)
# plot(fit2, which = 1)
# summary(fit2)


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

all.ec <- get_all_eigencent(processed, attr = "discp")
# prop.discp <- get_prop_people_by_year(processed) %>%
#   mutate(discp = discp1, year = Year)
# 
# ggsave("figures/eigencentrality.png", plot_eigen_lines(all.ec), 
#        width = 5, height = 2.5, scale = 1.75)
# ggsave("figures/eigen_prop-of-people.png", plot_eigen_and_prop(all.ec, prop.discp), 
#        width = 6.5, height = 4, scale = 1.75)
# ggsave("figures/all-discp_eigen-values.png", plot_all_eigen(all.ec), 
#        width = 8, height = 5.5, scale = 1.25)

####Null Models####
randomize_node_values = function(processed, iter, attr = "discp"){
  nodes <- build_node_dataframe(processed, iter)
  shuffled = nodes %>% dplyr::select_at(attr)
  rows = sample(nrow(shuffled))
  nodes[attr] = shuffled[rows,]
  return(nodes)
}

generate_null_ec_data = function(processed, attr = "discp", n) {
  iters = unique(processed$Iteration)
  edges <- build_edge_dataframe(processed, iters[1])
  nodes = randomize_node_values(processed, iters[1], attr)
  graph = create_graph(nodes, edges)
  nodes = nodes %>%
    left_join(get_eigen_centrality(graph), by = "id")
  ec = nodes %>%  
    group_by_at(attr) %>%
    summarize(mean.ec = mean(eigen_centrality), 
              max.ec = max(eigen_centrality), 
              min.ec = min(eigen_centrality)) %>%
    #arrange(attr) %>%
    mutate(iteration = iters[1])
  for(i in 2:n) {
    nodes = randomize_node_values(processed, iters[1], attr)
    
    graph = create_graph(nodes, edges)
    nodes = nodes %>%
      left_join(get_eigen_centrality(graph), by = "id")
    ec1 = nodes %>%  
      group_by_at(attr) %>%
      summarize(mean.ec = mean(eigen_centrality), 
                max.ec = max(eigen_centrality), 
                min.ec = min(eigen_centrality)) %>%
      #arrange(attr) %>%
      mutate(iteration = iters[1])
    ec = rbind(ec, ec1)
  }
  
  for(i in 2:length(iters)) {
    print(i)
    for(j in 1:n) {
      ##shuffle attributes
      edges <- build_edge_dataframe(processed, iters[i])
      nodes = randomize_node_values(processed, iters[i], attr)
      
      graph = create_graph(nodes, edges)
      nodes = nodes %>%
        left_join(get_eigen_centrality(graph), by = "id")
      ec1 = nodes %>%  
        group_by_at(attr) %>%
        summarize(mean.ec = mean(eigen_centrality), 
                  max.ec = max(eigen_centrality), 
                  min.ec = min(eigen_centrality)) %>%
        #arrange(attr) %>%
        mutate(iteration = iters[i])
      ec = rbind(ec, ec1)
    }
  }
  return(ec)
}

#generate plot for discipline eigencentrality
plot_ec_null_models = function(processed, attr = "discp") {
  all.ec = get_all_eigencent(processed, attr)
  all.ec[all.ec == ""] = NA
  nulldf = generate_null_ec_data(processed, attr, 100) %>%
    mutate(iteration = str_replace(iteration, "[.]", ",")) %>%
    separate(iteration, c("year", "location"), sep = ",") %>%
    filter(location == "Santa Fe", year != "2005", year != "2011")
  grouping = c(attr, "year")
  ci = nulldf %>% group_by_at(grouping) %>%
    summarize(upper_ci = quantile(mean.ec, 0.975), 
              lower_ci = quantile(mean.ec, 0.025))
  ci[ci == ""] = NA
  p = ggplot() +
    geom_errorbar(data = na.omit(ci), aes(x = year, group = year, ymin = lower_ci, ymax = upper_ci), color = "grey") +
    geom_point(data = na.omit(all.ec), aes(x = year, group = year, y = mean.ec), color = "black", size = 2) +
    facet_wrap(as.character(attr)) +
    theme_minimal() +
    labs(x = "Year", y = "Eigencentrality") +
    theme(axis.text.x = element_text(angle = 90), panel.spacing = unit(2, "lines"))
  return(p)
}

dplot = plot_ec_null_models(processed, attr = "discp")
plot(dplot)
ggsave("figures/eigencentrality_null_discp.png", dplot,
       width = 8, height = 5.5, scale = 1.25)

gplot = plot_ec_null_models(processed, attr = "gender")
plot(gplot)
ggsave("figures/eigencentrality_null_gender.png", gplot, 
       width = 5, height = 2.5, scale = 1.25)

pplot = plot_ec_null_models(processed, attr = "pos.var")
plot(pplot)
ggsave("figures/eigencentrality_null_position.png", pplot,
       width = 6.5, height = 4, scale = 1.25)

prplot = plot_ec_null_models(processed, attr = "prstg")
plot(prplot)
ggsave("figures/eigencentrality_null_prestige.png", prplot,
       width = 6.5, height = 4, scale = 1.25)


###Top 5 Discp Plot
attr = "discp"
all.ec = get_all_eigencent(processed, attr)
all.ec[all.ec == ""] = NA
nulldf = generate_null_ec_data(processed, attr, 100) %>%
  mutate(iteration = str_replace(iteration, "[.]", ",")) %>%
  separate(iteration, c("year", "location"), sep = ",") %>%
  filter(location == "Santa Fe"
         #, year != "2005"
         , year != "2011"
         )

plotdf = nulldf %>% filter(discp %in% c("Social and behavioral sciences", 
                                        "Engineering and engineering trades", 
                                        "Physical sciences", 
                                        "Life sciences", 
                                        "Computing")) %>%
  mutate(year = as.numeric(year))
actual = all.ec %>% filter(discp %in% c("Social and behavioral sciences", 
                                        "Engineering and engineering trades", 
                                        "Physical sciences", 
                                        "Life sciences", 
                                        "Computing")) %>%
  mutate(year = as.numeric(year)) %>%
  filter(year != "2011")
top = ggplot(data = plotdf, aes(x = year, y = mean.ec, group = year)) +
  geom_boxplot(alpha = 0.6, color = "darkgrey", width = 0.5) +
  geom_point(data = na.omit(actual), aes(y = mean.ec), color = "black", fill = "violet", size = 3, shape = 21) +
  facet_wrap(~discp, ncol = 1) +
  scale_x_continuous(breaks = c(2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019)) +
  labs(x = "Year",
       y = "Eigencentrality Value") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11),
    strip.text = element_text(face = "bold", size = 12),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

ggsave("figures/null-eigen_top5-discp.pdf", plot = top, height = 8, width = 5)

