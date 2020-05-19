# Get the Data

vb <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-19/vb_matches.csv', guess_max = 76000)

library(tidyverse)
library(cowplot)

vb <- vb %>% pivot_longer(names_to = 'winning_age', values_from = c(w_p1_age, w_p2_age))

head(vb)
# vb<-vb %>% group_by(country, year, date, match_num, w_p1_country) %>% summarise(age = mean(w_p1_age))

