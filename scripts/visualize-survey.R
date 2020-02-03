library('dplyr')
library('ggplot2')
library('stringr')
library('reshape2')


main_disciplines <- c(
  'Computing', 'Mathematics and Statistics', 'Physical Sciences', 'Life Sciences',
  'Social and Behavioral Science', 'Education', 'Arts', 'Humanities', 'Journalism',
  'Law', 'Engineering', 'Agriculture', 'Health', 'Environmental Protection', 'Social Services'
)

create_newdat <- function(dats, main_disciplines) {
  newdat <- c()
  
  for (i in seq(nrow(dats))) {
    row <- dats[i, ]
    disciplines <- str_split(row$Discipline_isced, ';')[[1]]
    
    for (j in seq(length(disciplines))) {
      if (disciplines[j] %in% main_disciplines) {
        row$Discipline_isced <- disciplines[j]
        newdat <- rbind(row, newdat)
      }
    }
  }
  
  newdat
}

survey_dat <- read.csv('data/raw/survey-data.csv')
sdat <- create_newdat(survey_dat, main_disciplines)

datp <- datss %>% 
  select(
    Discipline_isced, CSSS_Influence, Input_Usefulness, Communication_Difficulty
  ) %>% 
  melt %>% 
  mutate(
    variable = ifelse(
      variable == 'CSSS_Influence', 'Influence of CSSS',
      ifelse(variable == 'Input_Usefulness',
             'Usefulness of CSSS Input',
             'Difficulty of Communication'
      )
    ),
    variable = factor(
      datp$variable,
      levels = c('Influence of CSSS', 'Usefulness of CSSS Input', 'Difficulty of Communication')
    )
  )

p <- ggplot(datp, aes(y = value, x = Discipline_isced, fill = Discipline_isced)) +
  facet_wrap(~ variable) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = position_jitter(width = .3, height = .05), size = .6, color = 'black', alpha = .6) +
  theme_bw() +
  ylab('Value') +
  xlab('') +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    # axis.text.y = element_blank(),
    panel.spacing = unit(1, 'lines'),
    strip.text.x = element_text(size = 14, face = 'bold'),
    axis.title.x = element_text(size = 12),
    plot.title = element_text(hjust = .5, size = 16),
    # legend.position = 'left',
    legend.title = element_blank(),
    legend.key.size = unit(.75, "cm"),
    legend.text = element_text(size = 14),
    legend.position = 'none'
  ) +
  coord_flip()

pdf('img/Survey-Figures.pdf', width = 14, height = 5)
p
dev.off()
