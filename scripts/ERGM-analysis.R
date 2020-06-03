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
binaries = process_acsss_data(data)
processed <- processed %>% 
  mutate(Iteration = str_c(Year, Location, sep = ".")) %>%
  mutate(Topic_isced = topic1, 
         Discipline_isced = discp1) %>%
  left_join(binaries[])

edges = build_edge_dataframe(processed, "2019.Santa Fe")
nodes = build_node_dataframe(processed, "2019.Santa Fe")[,c(1,7,9:12)]
nodes$gender = as.character(nodes$gender)
nodes$prstg = as.character(nodes$prstg)
nodes = as_tibble(nodes)
#print(nodes)


####ERGM Analysis for nodematching####
net = network(as.matrix(edges[,2:3]), vertex.attr = nodes, vertex.attrnames = c(colnames(nodes)), 
               directed = F, matrix.type = "edgelist")

gender = ergm(net ~ edges + nodematch("gender"))
summary(gender)

discp = ergm(net ~ edges + nodematch("discp"))
summary(discp)

prstg = ergm(net ~ edges + nodematch("prstg"))
summary(prstg)

pos = ergm(net ~ edges + nodematch("pos.var"))
summary(pos)

all = ergm(net ~ edges + nodematch("gender") + nodematch("discp") + nodematch("prstg") + nodematch("pos.var"))
summary(all)
stargazer::stargazer(all, type = "text")


gender2 = ergm(net ~ edges + nodemix("gender", base = c(1)))
summary(gender2)

pos2 = ergm(net ~ edges + nodemix("pos.var", base = c(1)))
stargazer::stargazer(pos2, type = "text")

topdiscp = c("Life sciences", 
             "Social and behavioral sciences", 
             "Physical sciences", 
             "Computing", 
             "Engineering and engineering trades", 
             "Mathematics and statistics")
discp2 = ergm(net ~ edges + nodemix("discp", base = c(1), levels = topdiscp))
summary(discp2)
stargazer::stargazer(discp2, type = "text")

discp3 = ergm(net ~ edges + nodemix("discp", levels = c("Life sciences", "Social and behavioral sciences", "Physical sciences")))
summary(discp3)
stargazer::stargazer(discp3, type = "text")

