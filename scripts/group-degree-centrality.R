##Group Degree Centrality
source("ACSSS/R/build_edge_dataframe.R")
source("ACSSS/R/build_node_dataframe.R")
source("scripts/clean_raw_data.R")
library(tidyr)
library(DiagrammeR)
library(ggplot2)
library(dplyr)
library(igraph)

set.seed(1234)

data <- read.csv("data/raw/cleaned_csss-all.csv")
data <- data %>% filter(Year != 2011)
processed <- clean_raw_data(data)
processed <- processed %>% 
  mutate(Iteration = str_c(Year, Location, sep = ".")) %>%
  mutate(Topic_isced = topic1, 
         Discipline_isced = discp1) %>%
  filter(Location == "Santa Fe")

iters = c(unique(processed$Iteration))
disciplines = c(unique(processed$discp1))
genders = c(unique(as.character(processed$Gender)))
positions = c(unique(as.character(processed$Position)))
prestige = c(unique(as.character(processed$Prestige)))
countries = c("USA", "Not USA")

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

calculate_n.group.deg.cent = function(edges, nodes, d, attr = "discp") {
  attr1 = paste0(attr, ".x")
  attr2 = paste0(attr, ".y")
  edf = edges %>% left_join(nodes, by = c("V1" = "node_id")) %>% left_join(nodes, by = c("V2" = "node_id"))
  ed1 = edf %>% filter(eval(parse(text=attr1)) == d & eval(parse(text=attr2)) != d) #%>% dplyr::select(V1, V2, get(attr1), get(attr2))
  ed2 = edf %>% filter(eval(parse(text=attr2)) == d & eval(parse(text=attr1)) != d) #%>% dplyr::select(V1, V2, discp.x, discp.y)
  ingroup = nodes %>% filter(get(attr) == d)
  alters = unique(c(unique(ed1$V2), unique(ed2$V1)))
  return( (length(alters))/(nrow(nodes) - nrow(ingroup)) )
}

create_gdc_dataframe = function(nodes, edges, iter, disciplines, attr = "discp") {
  ddf = data.frame(iter = iter, 
                   discp = disciplines, 
                   gdc = 0)
  for(i in 1:length(disciplines)) {
    ddf$gdc[i] = calculate_n.group.deg.cent(edges, nodes, disciplines[i], attr = attr)
  }
  return(ddf)
}

edges = as.data.frame(get.edgelist(simplify(graph_from_edgelist(as.matrix(build_edge_dataframe(processed, iters[1])[,2:3])))))
nodes = build_node_dataframe(processed, iters[1])
all.gdc = create_gdc_dataframe(nodes, edges, iters[1], disciplines, attr = "discp")
for(i in 2:length(iters)) {
  edges = as.data.frame(get.edgelist(simplify(graph_from_edgelist(as.matrix(build_edge_dataframe(processed, iters[i])[,2:3])))))
  nodes = build_node_dataframe(processed, iters[i])
  all.gdc = rbind(all.gdc, create_gdc_dataframe(nodes, edges, iters[i], disciplines, attr = "discp"))
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
  null.gdc = create_gdc_dataframe(nodes, edges, iters[1], disciplines, attr = attr)
  for(i in 2:n) {
    nodes = randomize_node_values(processed, iters[1], attr)
    null.gdc = rbind(null.gdc, create_gdc_dataframe(nodes, edges, iters[1], disciplines, attr = attr))
  }
  #run through all years of summer school
  for(i in 2:length(iters)) {
    print(i)
    for(j in 1:n) {
      nodes = randomize_node_values(processed, iters[i], attr)
      edges = as.data.frame(get.edgelist(simplify(graph_from_edgelist(as.matrix(build_edge_dataframe(processed, iters[i])[,2:3])))))
      null.gdc = rbind(null.gdc, create_gdc_dataframe(nodes, edges, iters[i], disciplines, attr = attr))
    }
  }
  return(null.gdc)
}

null.gdc = generate_null_data(500, processed, attr = "discp", iters, disciplines)

null = null.gdc %>% separate(iter, c("year", "location"), sep = "[.]") %>% 
  filter(discp %in% c("Social and behavioral sciences", "Engineering and engineering trades",
                      "Physical sciences", "Life sciences", "Computing")) %>%
  mutate(year = as.numeric(year))


highlight <- null %>%
  mutate(year = as.character(year)) %>%
  group_by(year, discp) %>%
  summarize(
    dev = sd(gdc),
    mu = mean(gdc),
    upper.2 = mu + (2 * dev),
    lower.2 = mu - (2 * dev),
    upper.1 = mu + (dev),
    lower.1 = mu - (dev)
  ) %>%
  ungroup() %>%
  mutate(year = as.character(year))

actual = all.gdc %>% separate(iter, c("year", "location"), sep = "[.]") %>% 
  filter(discp %in% c("Social and behavioral sciences", "Engineering and engineering trades",
                      "Physical sciences", "Life sciences", "Computing")) %>%
  left_join(highlight, by = c("year", "discp")) %>%
  mutate(highlight = ifelse(gdc > upper.2 | gdc < lower.2, "> 2 SD", 
                            ifelse(gdc > upper.1 | gdc < lower.1, "> 1 SD", "< 1 SD")),
         highlight = factor(highlight, levels = c("> 2 SD", "> 1 SD", "< 1 SD"))
  ) %>%
  mutate(year = as.numeric(year))

p = ggplot(data = null, aes(x = year, y = gdc, group = year)) +
  geom_boxplot(alpha = 0.6, color = "darkgrey", width = 0.5) +
  geom_point(data = na.omit(actual), aes(y = gdc, fill = highlight), color = "black", size = 2, shape = 21) +
  facet_wrap(~discp, ncol = 1) +
  scale_x_continuous(breaks = c(2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019)) +
  scale_fill_manual(values = c("orange", "darkslategrey", "lightgrey"))  +
  labs(x = "Year",
       y = "Normalized Group Degree Centrality") +
  theme_minimal() +
  theme(
    text = element_text(family = "Helvetica"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11),
    strip.text = element_text(face = "bold", size = 12),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom"
  )

ggsave("figures/group-deg-cent_top5-discp.pdf", p, 
       width = 5, height = 8)

plot_gdc_null = function(all.gdc, null.gdc, attr = "discp") {
  p.null = null.gdc %>% separate(iter, c("year", "location"), sep = "[.]") %>% filter(get(attr) != "") %>%
    mutate(attr = get(attr))
  highlight <- p.null %>%
    mutate(year = as.character(year)) %>%
    group_by(year, attr) %>%
    summarize(
      dev = sd(gdc),
      mu = mean(gdc),
      upper.2 = mu + (2 * dev),
      lower.2 = mu - (2 * dev),
      upper.1 = mu + (dev),
      lower.1 = mu - (dev)
    ) %>%
    ungroup() %>%
    mutate(year = as.character(year))
  p.ac = all.gdc %>% separate(iter, c("year", "location"), sep = "[.]") %>% filter(get(attr) != "") %>%
    mutate(attr = get(attr)) %>%
    left_join(highlight, by = c("year", "attr")) %>%
    mutate(highlight = ifelse(gdc > upper.2 | gdc < lower.2, "> 2 SD", 
                              ifelse(gdc > upper.1 | gdc < lower.1, "> 1 SD", "< 1 SD")),
           highlight = factor(highlight, levels = c("> 2 SD", "> 1 SD", "< 1 SD"))
    )
  
  p1 = ggplot(data = p.null, aes(x = year, y = gdc, group = year)) +
    geom_boxplot(alpha = 0.6, color = "darkgrey", width = 0.5) +
    geom_point(data = na.omit(p.ac), aes(y = gdc, fill = highlight), color = "black", size = 2, shape = 21) +
    facet_wrap(attr) +
    #scale_x_discrete(breaks = c(2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019)) +
    scale_fill_manual(values = c("> 2 SD" = "orange", "> 1 SD" = "darkslategrey", "< 1 SD" = "lightgrey"))  +
    labs(x = "Year",
         y = "Normalized Group Degree Centrality") +
    theme_minimal() +
    theme(
      text = element_text(family = "Helvetica"),
      axis.title = element_text(size = 8),
      axis.text.x = element_text(angle = 90),
      #strip.text = element_text(face = "bold", size = 12),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      legend.title = element_blank(),
      legend.position = "bottom"
    )
}

dplot = plot_gdc_null(all.gdc, null.gdc, attr = "discp")
#plot(dplot)
ggsave("figures/group-deg-cent_null_discp.png", dplot,
       width = 8, height = 5.5, scale = 1.25)

#gender
edges = as.data.frame(get.edgelist(simplify(graph_from_edgelist(as.matrix(build_edge_dataframe(processed, iters[1])[,2:3])))))
nodes = build_node_dataframe(processed, iters[1])
gen.gdc = create_gdc_dataframe(nodes, edges, iters[1], genders, attr = "gender")
for(i in 2:length(iters)) {
  edges = as.data.frame(get.edgelist(simplify(graph_from_edgelist(as.matrix(build_edge_dataframe(processed, iters[i])[,2:3])))))
  nodes = build_node_dataframe(processed, iters[i])
  gen.gdc = rbind(gen.gdc, create_gdc_dataframe(nodes, edges, iters[i], genders, attr = "gender"))
}
null.gen.gdc = generate_null_data(500, processed, attr = "gender", iters, genders)
gplot = plot_gdc_null(gen.gdc, null.gen.gdc)
plot(gplot)
ggsave("figures/group-deg-cent_null_gender.png", gplot, 
       width = 5, height = 2.5, scale = 1.25)

#position
edges = as.data.frame(get.edgelist(simplify(graph_from_edgelist(as.matrix(build_edge_dataframe(processed, iters[1])[,2:3])))))
nodes = build_node_dataframe(processed, iters[1])
pos.gdc = create_gdc_dataframe(nodes, edges, iters[1], positions, attr = "pos.var")
for(i in 2:length(iters)) {
  edges = as.data.frame(get.edgelist(simplify(graph_from_edgelist(as.matrix(build_edge_dataframe(processed, iters[i])[,2:3])))))
  nodes = build_node_dataframe(processed, iters[i])
  pos.gdc = rbind(pos.gdc, create_gdc_dataframe(nodes, edges, iters[i], positions, attr = "pos.var"))
}
null.pos.gdc = generate_null_data(500, processed, attr = "pos.var", iters, positions)
pos.gdc = pos.gdc %>% mutate(discp = factor(discp, levels = c("Student", "Faculty", "Not Academia")))
null.pos.gdc = null.pos.gdc %>% mutate(discp = factor(discp, levels = c("Student", "Faculty", "Not Academia")))
pplot = plot_gdc_null(pos.gdc, null.pos.gdc)
ggsave("figures/group-deg-cent_null_position.png", pplot,
       width = 6.5, height = 2.5, scale = 1.25)

#prestige
edges = as.data.frame(get.edgelist(simplify(graph_from_edgelist(as.matrix(build_edge_dataframe(processed, iters[1])[,2:3])))))
nodes = build_node_dataframe(processed, iters[1])
pres.gdc = create_gdc_dataframe(nodes, edges, iters[1], prestige, attr = "prstg")
for(i in 2:length(iters)) {
  edges = as.data.frame(get.edgelist(simplify(graph_from_edgelist(as.matrix(build_edge_dataframe(processed, iters[i])[,2:3])))))
  nodes = build_node_dataframe(processed, iters[i])
  pres.gdc = rbind(pres.gdc, create_gdc_dataframe(nodes, edges, iters[i], prestige, attr = "prstg"))
}
pres.gdc = pres.gdc %>% mutate(discp = factor(discp, levels = c("Top 50", "Not Top 50")))
null.pres.gdc = generate_null_data(500, processed, attr = "prstg", iters, prestige)
null.pres.gdc = null.pres.gdc %>% mutate(discp = factor(discp, levels = c("Top 50", "Not Top 50")))
prplot = plot_gdc_null(pres.gdc, null.pres.gdc)
ggsave("figures/group-deg-cent_null_prestige.png", prplot,
       width = 5, height = 2.5, scale = 1.25)

#country of study
edges = as.data.frame(get.edgelist(simplify(graph_from_edgelist(as.matrix(build_edge_dataframe(processed, iters[1])[,2:3])))))
nodes = build_node_dataframe(processed, iters[1])
c.gdc = create_gdc_dataframe(nodes, edges, iters[1], countries, attr = "cntry")
for(i in 2:length(iters)) {
  edges = as.data.frame(get.edgelist(simplify(graph_from_edgelist(as.matrix(build_edge_dataframe(processed, iters[i])[,2:3])))))
  nodes = build_node_dataframe(processed, iters[i])
  c.gdc = rbind(c.gdc, create_gdc_dataframe(nodes, edges, iters[i], countries, attr = "cntry"))
}
null.c.gdc = generate_null_data(500, processed, attr = "cntry", iters, countries)
cplot = plot_gdc_null(c.gdc, null.c.gdc)
ggsave("figures/group-deg-cent_null_cntry.png", cplot, 
       width = 5, height = 2.5, scale = 1.25)

