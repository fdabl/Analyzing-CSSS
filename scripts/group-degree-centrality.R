##Group Degree Centrality
source("ACSSS/R/build_edge_dataframe.R")
source("ACSSS/R/build_node_dataframe.R")
source("scripts/clean_raw_data.R")
library(tidyr)
library(DiagrammeR)
library(ggplot2)
library(dplyr)

data <- read.csv("data/raw/cleaned_csss-all.csv")
#data <- data %>% filter(Year != 2011)
processed <- clean_raw_data(data)
processed <- processed %>% 
  mutate(Iteration = str_c(Year, Location, sep = ".")) %>%
  mutate(Topic_isced = topic1, 
         Discipline_isced = discp1) %>%
  filter(Location == "Santa Fe") %>%
  filter(Year != 2011)

iters = c(unique(processed$Iteration))
disciplines = c(unique(processed$discp1))

# #edges = build_edge_dataframe(processed, iters[15])
# edges = as.data.frame(get.edgelist(simplify(graph_from_edgelist(as.matrix(build_edge_dataframe(processed, iters[15])[,2:3])))))
# nodes = build_node_dataframe(processed, iters[15])
# 
# #edf = edges %>% left_join(nodes, by = c("from" = "node_id")) %>% left_join(nodes, by = c("to" = "node_id"))
# edf = edges %>% left_join(nodes, by = c("V1" = "node_id")) %>% left_join(nodes, by = c("V2" = "node_id"))
# ed1 = edf %>% filter(discp.x == disciplines[1] & discp.y != disciplines[1]) %>% dplyr::select(V1, V2, discp.x, discp.y)
# ed2 = edf %>% filter(discp.y == disciplines[1] & discp.x != disciplines[1]) %>% dplyr::select(V1, V2, discp.x, discp.y)
# ingroup = nodes %>% filter(discp == disciplines[1])
# gdcent = (nrow(ed1) + nrow(ed2))/(nrow(nodes) - nrow(ingroup))

calculate_n.group.deg.cent = function(edges, nodes, d) {
  edf = edges %>% left_join(nodes, by = c("V1" = "node_id")) %>% left_join(nodes, by = c("V2" = "node_id"))
  ed1 = edf %>% filter(discp.x == d & discp.y != d) %>% dplyr::select(V1, V2, discp.x, discp.y)
  ed2 = edf %>% filter(discp.y == d & discp.x != d) %>% dplyr::select(V1, V2, discp.x, discp.y)
  ingroup = nodes %>% filter(discp == d)
  return( (nrow(ed1) + nrow(ed2))/(nrow(nodes) - nrow(ingroup)) )
}

create_gdc_dataframe = function(nodes, edges, iter, disciplines) {
  ddf = data.frame(iter = iter, 
                   discp = disciplines, 
                   gdc = 0)
  for(i in 1:length(disciplines)) {
    ddf$gdc[i] = calculate_n.group.deg.cent(edges, nodes, disciplines[i])
  }
  return(ddf)
}

edges = as.data.frame(get.edgelist(simplify(graph_from_edgelist(as.matrix(build_edge_dataframe(processed, iters[1])[,2:3])))))
nodes = build_node_dataframe(processed, iters[1])
all.gdc = create_gdc_dataframe(nodes, edges, iters[1], disciplines)
for(i in 2:length(iters)) {
  edges = as.data.frame(get.edgelist(simplify(graph_from_edgelist(as.matrix(build_edge_dataframe(processed, iters[i])[,2:3])))))
  nodes = build_node_dataframe(processed, iters[i])
  all.gdc = rbind(all.gdc, create_gdc_dataframe(nodes, edges, iters[i], disciplines))
}

randomize_node_values = function(processed, iter, attr = "discp"){
  nodes <- build_node_dataframe(processed, iter)
  shuffled = nodes %>% dplyr::select_at(attr)
  rows = sample(nrow(shuffled))
  nodes[attr] = shuffled[rows,]
  return(nodes)
}

generate_null_data = function(n, processed, attr = "discp", iters, disciplines) {
  #initialize dataframe structure with first year
  print(1)
  nodes = randomize_node_values(processed, iters[1], attr)
  edges = as.data.frame(get.edgelist(simplify(graph_from_edgelist(as.matrix(build_edge_dataframe(processed, iters[1])[,2:3])))))
  null.gdc = create_gdc_dataframe(nodes, edges, iters[1], disciplines)
  for(i in 2:n) {
    nodes = randomize_node_values(processed, iters[1], attr)
    null.gdc = rbind(null.gdc, create_gdc_dataframe(nodes, edges, iters[1], disciplines))
  }
  #run through all years of summer school
  for(i in 2:length(iters)) {
    print(i)
    for(j in 1:n) {
      nodes = randomize_node_values(processed, iters[i], attr)
      edges = as.data.frame(get.edgelist(simplify(graph_from_edgelist(as.matrix(build_edge_dataframe(processed, iters[i])[,2:3])))))
      null.gdc = rbind(null.gdc, create_gdc_dataframe(nodes, edges, iters[i], disciplines))
    }
  }
  return(null.gdc)
}

null.gdc = generate_null_data(10, processed, attr = "discp", iters, disciplines)

actual = all.gdc %>% separate(iter, c("year", "location"), sep = "[.]") %>% filter(discp %in% c("Social and behavioral sciences", 
                                                                                                "Engineering and engineering trades",
                                                                                                "Physical sciences",
                                                                                                "Life sciences",
                                                                                                "Computing", 
                                                                                                "Mathematics and statistics"))
null = null.gdc %>% separate(iter, c("year", "location"), sep = "[.]") %>% filter(discp %in% c("Social and behavioral sciences", 
                                                                                               "Engineering and engineering trades",
                                                                                               "Physical sciences",
                                                                                               "Life sciences",
                                                                                               "Computing", 
                                                                                               "Mathematics and statistics"))

p = ggplot(data = null, aes(x = year, y = gdc, group = year)) +
  geom_boxplot(alpha = 0.6, color = "darkgrey", width = 0.8) +
  geom_point(data = na.omit(actual), aes(y = gdc), color = "black", fill = "violet", size = 1, shape = 21) +
  facet_wrap(~discp, ncol = 2) +
  #scale_x_discrete(breaks = c(2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019)) +
  labs(x = "Year",
       y = "Normalized Group Degree Centrality") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8, angle = 90),
    strip.text = element_text(face = "bold", size = 8),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

ggsave("figures/group-deg-cent_top6-discp.pdf", p, 
       width = 8 , height = 7)
