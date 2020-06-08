###preferential attachment regressions
library(dplyr)
library(ggplot2)
library(tidyr)
library(DiagrammeR)
library(network)
library(ergm)
library(igraph)
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

iters = c(unique(processed$Iteration))

edges = build_edge_dataframe(processed, iters[2])
nodes = build_node_dataframe(processed, iters[2])[,c(1,7,9:12)]

# for(i in 3:length(iters)) {
#   edges = rbind(edges, build_edge_dataframe(processed, iters[i]))
#   nodes = rbind(nodes, build_node_dataframe(processed, iters[i])[,c(1,7,9:12)])
# }

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


####Dyad Counting####
edges2 = as.data.frame(get.edgelist(simplify(graph_from_edgelist(as.matrix(build_edge_dataframe(processed, iters[2])[,2:3])))))
nodes2 = build_node_dataframe(processed, iters[2])[,c(1,7,9:12)]
e = edges2 %>% left_join(nodes2, by = c("V1" = "id")) %>% left_join(nodes2, by = c("V2" = "id"))

for(i in 3:length(iters)) {
  edges2 = as.data.frame(get.edgelist(simplify(graph_from_edgelist(as.matrix(build_edge_dataframe(processed, iters[i])[,2:3])))))
  nodes2 = build_node_dataframe(processed, iters[i])[,c(1,7,9:12)]
  e.add = edges2 %>% left_join(nodes2, by = c("V1" = "id")) %>% left_join(nodes2, by = c("V2" = "id"))
  e = rbind(e, e.add)
}

dyad.discp = as.data.frame(table(e$discp.x, e$discp.y)) %>% filter(Var1 != "") %>% filter(Var2 != "")


dyad.gender = as.data.frame(table(e$gender.x, e$gender.y)) %>% filter(Var1 != "") %>% filter(Var2 != "") %>% filter(Freq != 0)
dyad.pos = as.data.frame(table(e$pos.var.x, e$pos.var.y)) %>% filter(Var1 != "") %>% filter(Var2 != "") %>% filter(Freq != 0)
