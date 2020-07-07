###preferential attachment regressions
library(dplyr)
library(ggplot2)
library(tidyr)
library(DiagrammeR)
library(network)
library(ergm)
library(igraph)
library(ggthemes)
source("ACSSS/R/process_acsss_data.R")
source("scripts/clean_raw_data.R")
source("ACSSS/R/build_edge_dataframe.R")
source("ACSSS/R/build_node_dataframe.R")

data <- read.csv("data/raw/cleaned_csss-all.csv")
#data <- data %>% filter(Year != 2011)
processed <- clean_raw_data(data)
binaries = process_acsss_data(data)
processed <- processed %>% 
  mutate(Iteration = str_c(Year, Location, sep = ".")) %>%
  mutate(Topic_isced = topic1, 
         Discipline_isced = discp1) %>%
  left_join(binaries[])

iters = c(unique(processed$Iteration))

edges = build_edge_dataframe(processed, iters[12])
nodes = build_node_dataframe(processed, iters[12])[,c(1,6:7,9:12)]

# for(i in 3:length(iters)) {
#   edges = rbind(edges, build_edge_dataframe(processed, iters[i]))
#   nodes = rbind(nodes, build_node_dataframe(processed, iters[i])[,c(1,7,9:12)])
# }

nodes$gender = as.character(nodes$gender)
nodes$prstg = as.character(nodes$prstg)
nodes = as_tibble(nodes)
#print(nodes)

####Dyad Counting####
edges2 = as.data.frame(get.edgelist(simplify(graph_from_edgelist(as.matrix(build_edge_dataframe(processed, iters[2])[,2:3])))))
nodes2 = build_node_dataframe(processed, iters[2])[,c(1,6:7,9:12)]
e = edges2 %>% left_join(nodes2, by = c("V1" = "id")) %>% left_join(nodes2, by = c("V2" = "id"))
n = nodes2

for(i in 3:length(iters)) {
  edges2 = as.data.frame(get.edgelist(simplify(graph_from_edgelist(as.matrix(build_edge_dataframe(processed, iters[i])[,2:3])))))
  nodes2 = build_node_dataframe(processed, iters[i])[,c(1,6:7,9:12)]
  e.add = edges2 %>% left_join(nodes2, by = c("V1" = "id")) %>% left_join(nodes2, by = c("V2" = "id"))
  e = rbind(e, e.add)
  n = rbind(n, nodes2)
}

create_edgelist_count = function(df, varlist) {
  occ = matrix(data = 0, nrow = length(varlist), ncol = length(varlist))
  rownames(occ) = c(varlist)
  colnames(occ) = c(varlist)
  for(i in 1:nrow(occ)) {
    for(j in 1:ncol(occ)) {
      row = rownames(occ)[i]
      col = rownames(occ)[j]
      if(i != j) {
        drows1 = df %>% filter(Var1 == row & Var2 == col)
        drows2 = df %>% filter(Var2 == row & Var1 == col)
        occ[i,j] = as.numeric(occ[i,j]) + ifelse(is.na(drows1$Freq[1]), 0, drows1$Freq[1]) + ifelse(is.na(drows2$Freq[1]), 0, drows2$Freq[1])
      } else {
        drows = df %>% filter(Var1 == row & Var2 == col)
        occ[i,j] = occ[i,j] + drows$Freq[1]
      }
    }
  }
  occ2 = occ
  occ2[upper.tri(occ2)] = 0
  list = reshape::melt(occ2) %>% filter(value != 0)
  return(list)
}

dyad.discp = as.data.frame(table(e$discp.x, e$discp.y)) %>% filter(Var1 != "") %>% filter(Var2 != "")
dlist = unique(c(unique(as.character(dyad.discp$Var1)), unique(as.character(dyad.discp$Var2))))
discp.occ = create_edgelist_count(dyad.discp, dlist)
discp.count = n %>% group_by(discp, year) %>% summarize(count = n()) %>% filter(discp != "")
discp.occ$freq = 0
for(i in 1:nrow(discp.occ)) {
  discp1 = as.character(discp.occ$X1[i])
  discp2 = as.character(discp.occ$X2[i])
  total = 0
  for(iter in iters[2:14]) {
    dc.year = discp.count %>% filter(year == iter)
    n = ifelse(discp1 %in% dc.year$discp, as.numeric(dc.year$count[dc.year$discp == discp1]), 0)
    m = ifelse(discp2 %in% dc.year$discp, as.numeric(dc.year$count[dc.year$discp == discp2]), 0)
    total = total + n*m
  }
  if(total == 0) {
    discp.occ$freq[i] = NA
  }else { discp.occ$freq[i] = discp.occ$value[i]/total }
  
}

##need to add proportions of participants for each pair to discp.occ in order to plot
dc = discp.count %>% group_by(discp) %>% summarize(count = sum(count))
discp.occ$pcount = 0
for(i in 1:nrow(discp.occ)) {
  d1 = as.character(discp.occ$X1[i])
  d2 = as.character(discp.occ$X2[i])
  discp.occ$pcount[i] = (dc %>% filter(discp == d1))$count + (dc %>% filter(discp == d2))$count
}
discp.occ$pprop = discp.occ$pcount/sum(dc$count)

discp.hm1 = ggplot(data = discp.occ, aes(x = X1, y = X2, size = pprop, color = freq)) +
  geom_point() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 32, hjust = 1, size = 7),
        axis.text.y = element_text(hjust = 1, size = 7), 
        axis.title = element_blank()) +
  labs(color = "Frequency of Dyad", size = "Proportion of Participants") +
  scale_color_gradientn(colors = wesanderson::wes_palette("Zissou1", 100, type = "continuous"))
plot(discp.hm1)
ggsave("figures/dyad-heatmap1.pdf", discp.hm1, units = "in", 
       height = 5, width = 8, dpi = 300)

discp.hm2 = ggplot(data = discp.occ, aes(x = X1, y = X2, size = freq, color = pprop)) +
  geom_point() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 32, hjust = 1, size = 7),
        axis.text.y = element_text(hjust = 1, size = 7), 
        axis.title = element_blank()) +
  labs(color = "Proportion of Participants", size = "Frequency of Dyad") +
  scale_color_continuous_tableau()
ggsave("figures/dyad-heatmap2.pdf", discp.hm2, units = "in", 
       height = 5, width = 8, dpi = 300)

# dyad.gender = as.data.frame(table(e$gender.x, e$gender.y)) %>% filter(Var1 != "") %>% filter(Var2 != "")
# glist = unique(c(unique(as.character(dyad.gender$Var1)), unique(as.character(dyad.gender$Var2))))
# gender.occ = create_edgelist_count(dyad.gender, glist)
# gen.count = n %>% group_by(gender) %>% summarize(count = n())
# gender.occ$freq = 0
# for(i in 1:nrow(gender.occ)) {
#   gen1 = as.character(gender.occ$X1[i])
#   gen2 = as.character(gender.occ$X2[i])
#   total = gen.count$count[gen.count$gender == gen1] + gen.count$count[gen.count$gender == gen2]
#   gender.occ$freq[i] = gender.occ$value[i]/total
# }
# 
# dyad.pos = as.data.frame(table(e$pos.var.x, e$pos.var.y)) %>% filter(Var1 != "") %>% filter(Var2 != "")
# plist = unique(c(unique(as.character(dyad.pos$Var1)), unique(as.character(dyad.pos$Var2))))
# pos.occ = create_edgelist_count(dyad.pos, plist)
# pos.count = n %>% group_by(pos.var) %>% summarize(count = n())
# pos.occ$freq = 0
# for(i in 1:nrow(pos.occ)) {
#   pos1 = as.character(pos.occ$X1[i])
#   pos2 = as.character(pos.occ$X2[i])
#   total = pos.count$count[pos.count$pos.var == pos1] + pos.count$count[pos.count$pos.var == pos2]
#   pos.occ$freq[i] = pos.occ$value[i]/total
# }




####ERGM Analysis for nodematching####
# net = network(as.matrix(edges[,2:3]), vertex.attr = nodes, vertex.attrnames = c(colnames(nodes)), 
#                directed = F, matrix.type = "edgelist")
# 
# gender = ergm(net ~ edges + nodematch("gender"))
# summary(gender)
# 
# discp = ergm(net ~ edges + nodematch("discp"))
# summary(discp)
# 
# prstg = ergm(net ~ edges + nodematch("prstg"))
# summary(prstg)
# 
# pos = ergm(net ~ edges + nodematch("pos.var"))
# summary(pos)
# 
# all = ergm(net ~ edges + nodematch("gender") + nodematch("discp") + nodematch("prstg") + nodematch("pos.var"))
# summary(all)
# stargazer::stargazer(all, type = "text")
# 
# 
# gender2 = ergm(net ~ edges + nodemix("gender", base = c(1)))
# summary(gender2)
# 
# pos2 = ergm(net ~ edges + nodemix("pos.var", base = c(1)))
# stargazer::stargazer(pos2, type = "text")
# 
# topdiscp = c("Life sciences", 
#              "Social and behavioral sciences", 
#              "Physical sciences", 
#              "Computing", 
#              "Engineering and engineering trades", 
#              "Mathematics and statistics")
# discp2 = ergm(net ~ edges + nodemix("discp", base = c(1), levels = topdiscp))
# summary(discp2)
# stargazer::stargazer(discp2, type = "text")
# 
# discp3 = ergm(net ~ edges + nodemix("discp", levels = c("Life sciences", "Social and behavioral sciences", "Physical sciences")))
# summary(discp3)
# stargazer::stargazer(discp3, type = "text")




