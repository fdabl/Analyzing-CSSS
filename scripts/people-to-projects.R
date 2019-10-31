library(dplyr)
library(ggplot2)
library(tidyr)
source("scripts/clean_raw_data.R")

data <- read.csv("data/raw/cleaned_csss-all.csv")
data <- data %>% filter(Year != 2011)
processed <- clean_raw_data(data)

people <- processed %>% group_by(Name) %>%
  summarize(discp = first(discp1), 
            year = first(Year)) %>%
  filter(discp != "")
projects <- processed %>% group_by(Title) %>%
  summarize(topic = first(topic1), 
            year = first(Year)) %>%
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
  geom_point(size = 3) +
  geom_abline() +
  ggrepel::geom_text_repel(aes(label = ifelse(abs(dif) > 0.02, discp, "")), size = 3) +
  theme_minimal() +
  labs(color = "Discipline") +
  labs(x = "Proportion of total participants", y = "Proportion of total projects") +
  xlim(0.0, 0.375) + ylim(0.0, 0.375)

peop.year <- as.data.frame(table(people$discp, people$year)) %>%
  mutate(Total = sum(Freq), 
         Prop = Freq/Total)
proj.year <- as.data.frame(table(projects$topic, projects$year)) %>%
  mutate(Total = sum(Freq), 
         Prop = Freq/Total)

compare2 <- proj.year %>%
  left_join(peop.year, by = c("Var1", "Var2")) %>%
  mutate(discp = Var1, 
         year = Var2,
         peop.prop = Prop.y, 
         proj.prop = Prop.x,
         dif = Prop.x - Prop.y) %>%
  select(discp, peop.prop, proj.prop, dif, year) %>%
  filter(!is.na(dif)) %>%
  filter(year != 2005)

prop.plot.year <- ggplot(compare2, aes(x = peop.prop, 
                                       y = proj.prop, 
                                       color = discp)) +
  geom_point(size = 2) +
  geom_abline() +
  ggrepel::geom_text_repel(aes(label = ifelse(abs(dif) >= 0.01, discp, "")), size = 2.5) +
  theme_minimal() +
  labs(color = "Discipline") +
  labs(x = "Proportion of total participants", y = "Proportion of total projects") +
  xlim(0.0, 0.04) + ylim(0.0, 0.04) +
  facet_wrap(. ~ year, ncol = 4)

ggsave("figures/prop-people-projects.png", prop.plot, 
       width = 8, height = 5.5)
ggsave("figures/prop-peop-proj_by-year.png", prop.plot.year, 
       width = 11, height = 8.5)
