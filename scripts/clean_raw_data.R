library(dplyr)
library(stringr)

data <- read.csv("data/raw/cleaned_csss-all.csv")
data <- data %>% filter(Year != 2011)


clean_raw_data <- function(data) {
isced.split <- data %>%
  mutate(Topic_isced = str_replace(Topic_isced, ",", ";")) %>%
  separate(Topic_isced, c("topic1", "topic2"), sep = ";") %>% 
  mutate(Discipline_isced = str_replace(Discipline_isced, ",", ";")) %>%
  separate(Discipline_isced, c("discp1", "discp2"), sep = ";") %>%
  filter(topic1 != "", discp1 != "") %>%
  mutate(topic2 = str_trim(topic2, side = "left"), 
         discp2 = str_trim(discp2, side = "left")) %>%
  mutate(discp1 = ifelse(str_detect(discp1, "Social and"), 
                         "Social and behavioral sciences", 
                         ifelse(str_detect(discp1, "Physical"), 
                                "Physical sciences", 
                                discp1))) %>%
  mutate(discp2 = ifelse(str_detect(discp2, "Social and"), 
                         "Social and behavioral sciences", 
                         ifelse(str_detect(discp2, "Life science"), 
                                "Life sciences", 
                                ifelse(str_detect(discp2, "hysical "), 
                                       "Physical sciences", 
                                       ifelse(str_detect(discp2, "Engineering"), 
                                              "Engineering and engineering trades", discp2))))) %>%
  mutate(topic1 = ifelse(str_detect(topic1, "Environmental "), 
                         "Environmental protection", 
                         ifelse(str_detect(topic1, "Social and"), 
                                "Social and behavioral sciences", 
                                ifelse(str_detect(topic1, "duction"), 
                                       "Education", 
                                       ifelse(str_detect(topic1, "Life science"), 
                                              "Life sciences", topic1))))) %>%
  mutate(topic2 = ifelse(str_detect(topic2, "Social and"), 
                         "Social and behavioral sciences", 
                         ifelse(str_detect(topic2, "Life "), 
                                "Life sciences", 
                                ifelse(str_detect(topic2, "Physical "), 
                                       "Physical sciences", topic2))))
  return(isced.split)
}
