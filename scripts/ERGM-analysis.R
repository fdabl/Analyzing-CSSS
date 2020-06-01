###preferential attachment regressions

library(dplyr)
library(ggplot2)
library(tidyr)
library(DiagrammeR)
library(network)
library(ergm)
source("ACSSS/R/process_acsss_data.R")
source("scripts/clean_raw_data.R")
source("ACSSS/R/build_edge_dataframe.R")
source("ACSSS/R/build_node_dataframe.R")

data <- read.csv("data/raw/cleaned_csss-all.csv")
data <- data %>% filter(Year != 2011)
processed <- clean_raw_data(data)
processed <- processed %>% 
  mutate(Iteration = str_c(Year, Location, sep = ".")) %>%
  mutate(Topic_isced = topic1, 
         Discipline_isced = discp1)

edges = build_edge_dataframe(processed, "2019.Santa Fe")
nodes = build_node_dataframe(processed, "2019.Santa Fe")[,c(1,7,9:12)]
nodes$gender = as.character(nodes$gender)
nodes$prstg = as.character(nodes$prstg)
nodes = as_tibble(nodes)
#print(nodes)

net = network(as.matrix(edges[,2:3]), vertex.attr = nodes, vertex.attrnames = c(colnames(nodes)), 
               directed = F, matrix.type = "edgelist")

gender = ergm(net ~ edges + nodematch("gender"))
summary(base)

discp = ergm(net ~ edges + nodematch("discp"))
summary(discp)

prstg = ergm(net ~ edges + nodematch("prstg"))
summary(prstg)

pos = ergm(net ~ edges + nodematch("pos.var"))
summary(pos)
