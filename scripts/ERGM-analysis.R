###preferential attachment regressions
##need to do updates so that more than just binaries for discipline

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

processed <- process_acsss_data(data)
graph = create_graph(nodes_df = build_node_dataframe(processed, "2019.SantaFe"),
                     edges_df = build_edge_dataframe(processed, "2019.SantaFe"))

edges = build_edge_dataframe(processed, "2019.SantaFe")
nodes = build_node_dataframe(processed, "2019.SantaFe")[,c(1,7,9:12)]
nodes$gender = as.character(nodes$gender)
nodes = as_tibble(nodes)
print(nodes)

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
