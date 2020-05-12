setwd('tidytuesday')
library(tidyverse); library(funk)

volcano <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv')
eruptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv')
events <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/events.csv')

head(volcano) %>% data.frame()
head(eruptions) %>% data.frame()
head(events) %>% data.frame()


obs<-eruptions %>% filter(evidence_method_dating == 'Historical Observations' & start_year > 1800) %>%
          select(volcano_name, volcano_number, vei, start_year, end_year, latitude, longitude) %>%
          group_by(volcano_name) %>% mutate(freq = length(volcano_name), max.vei = max(vei, na.rm=TRUE))

obs<-left_join(obs, volcano)

obs$volcano_name<-order(obs$volcano_name, obs$region)

ggplot(obs %>% filter(vei>3 & freq>10), aes(volcano_name, start_year, size=vei, fill=population_within_10_km)) + 
  geom_point(alpha=0.5, shape=21,colour='black',stroke=1.2) + 
  coord_flip() +
  labs(x='', y='', title='Volcano Explosivity Index of the 100 most active volcanoes') +
  scale_size_area(breaks=c(4,5,6), trans='exp', max_size=40) +
  scale_fill_gradient(na.value='white', low='#ffffcc', high='#800026') +
  scale_y_continuous(breaks=seq(1800, 2020, 20)) +
  theme_classic() 

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
  theme_bw()+
  scale_colour_gradient(high='red', low='white')





