library(data.table)

heroes_dt <- 
  fread(
    'data/raw/heroes_information.csv',
    na.strings = c('NA', '-', '-99')
  )

heroes_dt
class(heroes_dt)

gender_group <- heroes_dt[, mean(Height, na.rm = TRUE), by = Gender]
gender_good_group <- heroes_dt[Alignment == 'good',
                               .(mean_height = mean(Height, na.rm = TRUE)),
                               by = Gender][
                                 order(-mean_height)
                               ]

library(tidyverse)
heroes_tbl <- read_csv("https://raw.githubusercontent.com/Pozdniakov/tidy_stats/master/data/heroes_information.csv",
                       na = c("NA", "-", "-99"))
heroes_tbl
class(heroes_tbl)
heroes_tbl %>% 
  filter(Alignment == 'good') %>% 
  group_by(Gender) %>% 
  summarise(mean_height = mean(Height, na.rm = TRUE)) %>% 
              arrange(desc(mean_height))