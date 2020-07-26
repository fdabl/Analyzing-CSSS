library(dplyr)
library(stringr)
library(tidyr)

data <- read.csv("data/raw/cleaned_csss-all.csv")
data <- data %>% filter(Year != 2011)


clean_raw_data <- function(data) {
isced.split <- data %>%
  filter(Location == "Santa Fe") %>%
  mutate(Topic_isced = str_replace(Topic_isced, ",", ";")) %>%
  separate(Topic_isced, c("topic1", "topic2"), sep = ";") %>% 
  mutate(Discipline_isced = str_replace(Discipline_isced, ",", ";")) %>%
  separate(Discipline_isced, c("discp1", "discp2"), sep = ";") %>%
  #filter(topic1 != "", discp1 != "") %>%
  mutate(topic2 = str_trim(topic2, side = "left"), 
         discp2 = str_trim(discp2, side = "left"), 
         Position = str_trim(Position, side = "both")) %>%
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
                                ifelse(str_detect(topic1, "Education"), 
                                       "Teacher training and education science", 
                                       ifelse(str_detect(topic1, "Life "), 
                                              "Life sciences", 
                                              ifelse(str_detect(topic1, "Medicine"), 
                                                     "Health", topic1)))))) %>%
  mutate(topic2 = ifelse(str_detect(topic2, "Social and"), 
                         "Social and behavioral sciences", 
                         ifelse(str_detect(topic2, "Life "), 
                                "Life sciences", 
                                ifelse(str_detect(topic2, "Physical "), 
                                       "Physical sciences", topic2)))) %>%
  mutate(Position = ifelse(str_detect(Position, "stu"), 
                           "Student", 
                           ifelse(str_detect(Position, "Post"), 
                                  "Postdoc",
                                  ifelse(str_detect(Position, "PhD"), 
                                         "Student", Position)))) %>%
  mutate(Position = ifelse(Position %in% c("Student"), "Student", 
                           ifelse(Position %in% c("Postdoc", "Professor", "Researcher"), "Faculty", 
                                  ifelse(Position %in% c("Industry", "Medicine", "Government", "Other"), "Not Academia", 
                                         Position))))
  return(isced.split)
}
