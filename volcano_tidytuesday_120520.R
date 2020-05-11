setwd('tidytuesday')
library(tidyverse); library(funk)

volcano <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv')
eruptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv')
events <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/events.csv')

head(volcano) %>% data.frame()
head(eruptions) %>% data.frame()
head(events) %>% data.frame()


obs<-eruptions %>% filter(evidence_method_dating == 'Historical Observations' & start_year > 0) %>%
          select(volcano_name, volcano_number, vei, start_year, end_year, latitude, longitude) %>%
          group_by(volcano_name) %>% mutate(freq = length(volcano_name), max.vei = max(vei, na.rm=TRUE))

obs<-left_join(obs, volcano)



ggplot(obs %>% filter(start_year > 1882 & !is.na(vei)), aes(country, start_year, fill=vei)) + 
          geom_tile() + 
          coord_flip() + 
          theme_bw()

ggplot(obs %>% filter(start_year > 1882 & !is.na(vei)), aes(vei, start_year, group=volcano_name)) + 
  geom_line() + 
  coord_flip() + 
  theme_bw()



ggplot(obs %>% filter(start_year > 1882 & !is.na(vei)), aes(longitude, latitude, col=start_year, size=vei)) + 
  geom_point(alpha=0.2) + 
  theme_bw()

ggplot(obs, aes(longitude, latitude, col=freq, size=max.vei)) + 
  geom_point(alpha=0.2) + 
  theme_bw()





