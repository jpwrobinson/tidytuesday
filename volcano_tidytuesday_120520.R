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
          group_by(volcano_name) %>% mutate(freq = length(volcano_name), max.vei = max(vei, na.rm=TRUE)) %>%
          ungroup() 
obs$region<-factor(obs$region)

obs<-left_join(obs, volcano) %>%
          mutate(volcano_name = fct_reorder(volcano_name, longitude))


plotter<-obs %>% filter(vei > 3 & freq > 10)

left<-ggplot(plotter, aes(volcano_name, start_year, fill = population_within_10_km, size=vei)) +
  geom_point(alpha=0.5, shape=21,colour='black',stroke=1.2) + 
  coord_flip() +
  labs(x='', y='') +
  scale_size_area(breaks=c(4,5,6), trans='exp', max_size=30) +
  scale_fill_gradient(na.value='white', low='#ffffcc', high='#800026') +
  scale_y_continuous(breaks=seq(1800, 2020, 20)) +
  scale_x_discrete(expand=c(0.05,0.05)) +
  theme_classic() +
  theme(legend.position = 'none',
        panel.grid.major.y = element_line(), 
        plot.margin=unit(c(0.5, 0, 0, 0), 'cm'))

plotter2<-plotter %>% group_by(volcano_name, region) %>% summarise(freq = unique(freq))
right<-ggplot(plotter2) +
  geom_bar(aes(volcano_name, freq, fill=region), stat='identity') +
  geom_text(aes(x=volcano_name, y=freq, label = region, col=region), size=2,hjust = -0.1) +
  coord_flip() + 
  labs(y = 'Number of eruptions since 1800', x = '') +
  theme_classic() + 
  theme(axis.text.y = element_blank(), axis.ticks=element_blank(), legend.position = 'none',
        plot.margin=unit(c(0.5, 0, 0, 0), 'cm')) +
  scale_y_continuous(expand=c(0, 0), breaks=seq(0, 100, 25)) +
  scale_x_discrete(expand=c(0.05,0.05)) 

cowplot::plot_grid(left, right, rel_widths = c(1, 0.3), nrow=1)

# 
# ggplot(obs %>% filter(start_year > 1882 & !is.na(vei)), aes(country, start_year, fill=vei)) + 
#           geom_tile() + 
#           coord_flip() + 
#           theme_bw()
# 
# ggplot(obs %>% filter(start_year > 1882 & !is.na(vei)), aes(vei, start_year, group=volcano_name)) + 
#   geom_line() + 
#   coord_flip() + 
#   theme_bw()
# 
# 
# 
# ggplot(obs %>% filter(start_year > 1882 & !is.na(vei)), aes(longitude, latitude, col=start_year, size=vei)) + 
#   geom_point(alpha=0.2) + 
#   theme_bw()
# 
# ggplot(obs, aes(longitude, latitude, col=freq, size=max.vei)) + 
#   geom_point(alpha=0.2) + 
#   theme_bw()+
#   scale_colour_gradient(high='red', low='white')
# 
# 
# 
# 
# 
