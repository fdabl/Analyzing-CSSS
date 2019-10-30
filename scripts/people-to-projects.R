library(dplyr)
library(ggplot2)
library(tidyr)
source("scripts/clean_raw_data.R")

data <- read.csv("data/raw/cleaned_csss-all.csv")
data <- data %>% filter(Year != 2011)
processed <- clean_raw_data(data)

people <- processed %>% group_by(Name) %>%
  summarize(discp = first(discp1)) %>%
  filter(discp != "")
projects <- processed %>% group_by(Title) %>%
  summarize(topic = first(topic1)) %>%
  filter(topic != "")

peop.tab <- as.data.frame(table(people$discp)) %>%
  mutate(Total = sum(Freq), 
         Prop = Freq/Total)
proj.tab <- as.data.frame(table(projects$topic)) %>%
  mutate(Total = sum(Freq), 
         Prop = Freq/Total)
compare <- proj.tab %>%
  left_join(peop.tab, by = "Var1") %>%
  mutate(discp = Var1, 
         peop.prop = Prop.y, 
         proj.prop = Prop.x,
         dif = Prop.x - Prop.y) %>%
  select(discp, peop.prop, proj.prop, dif) %>%
  filter(!is.na(dif))

glevels <- compare %>%
  arrange(dif) %>%
  pull(discp)

# ggplot(compare, aes(x = discp)) +
#   geom_point(aes(y = peop.prop, color = "Proportion of participants")) +
#   geom_point(aes(y = proj.prop, color = "Proportion of projects")) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 32, hjust = 1)) +
#   labs(x = "", y = "Proportion of total", color = "") +
#   scale_x_discrete(limits = glevels)

prop.plot <- ggplot(compare, aes(x = peop.prop, y = proj.prop, color = discp)) +
  geom_point() +
  geom_abline() +
  geom_text(aes(label = ifelse(abs(dif) > 0.02, discp, ""), vjust = 1, hjust = 1), size = 3) +
  theme_minimal() +
  guides(color = F) +
  labs(x = "Proportion of total participants", y = "Proportion of total projects")

ggsave("figures/prop-people-projects.png", prop.plot, 
       width = 6, height = 5.5)
