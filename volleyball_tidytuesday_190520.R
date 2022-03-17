# Get the Data
setwd('tidytuesday')
# vb <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-19/vb_matches.csv', guess_max = 76000)

library(countrycode)
library(tidyverse)
library(cowplot)
library(ggflags)
library(ggthemes)

theme_set(theme_solarized())
head(vb)
vb2<-vb %>% filter(circuit == 'FIVB') %>%
        mutate(age_win = (w_p1_age + w_p2_age)/2,
               age_lose = (l_p1_age + l_p2_age)/2,
               hgt_win = (w_p1_hgt + w_p2_hgt)/2,
               hgt_lose = (l_p1_hgt + l_p2_hgt)/2,
               country_win = w_p1_country,
               country_lose = l_p1_country) %>%
      select(country, year, date, match_num, w_p1_country, 
             age_win, age_lose, hgt_win, hgt_lose, 
             country_win, country_lose)

vb_avg<-vb2 %>% group_by(year) %>% 
      summarise(
        age_win = mean(age_win, na.rm=TRUE), age_lose = mean(age_lose, na.rm=TRUE), 
        hgt_win = mean(hgt_win, na.rm=TRUE), hgt_lose = mean(hgt_lose, na.rm=TRUE))

vb_nation<-vb2 %>% group_by(country_win) %>%
        count() %>% ungroup() %>%
        mutate(country_win = fct_reorder(country_win, -n))

top<-ggplot(vb_avg) + 
  geom_point(aes(year, age_win), col='#1b9e77') +
  geom_line(aes(year, age_win), col='#1b9e77') +
  geom_point(aes(year, age_lose), col='#d95f02') +
  geom_line(aes(year, age_lose), col='#d95f02') +
  annotate('text', 2004, 29.4, label='Winning team', col='#1b9e77') +
  annotate('text', 2002, 27.2, label='Losing team', col='#d95f02') +
  labs(x = '', y = 'age, yrs', subtitle='Winners are older (but getting younger)') +
  scale_x_continuous(breaks=seq(2000, 2020,3)) +
  theme( 
        plot.margin=margin(0.1, 0.1, -1, 0.1, 'cm'))

mid<-ggplot(vb_avg) + 
  geom_point(aes(year, hgt_win),col='#1b9e77') +
  geom_line(aes(year, hgt_win),col='#1b9e77') +
  geom_point(aes(year, hgt_lose), col='#d95f02') +
  geom_line(aes(year, hgt_lose), col='#d95f02') +
  labs(x = '', y = 'height, cm', subtitle='Winners are taller (and growing)',
    caption = '@jamespwr\n #tidytuesday, 19th May 2020') +
  scale_x_continuous(breaks=seq(2000, 2020,3)) +
  theme(plot.margin=margin(-1, 0.1, 0, 0.1, 'cm'),
    plot.caption = element_text(color='#1b9e77'))


vb_nation$code<-tolower(countrycode(vb_nation$country_win, origin = 'country.name', destination = 'iso2c'))

bot<-ggplot(vb_nation %>% filter(n > 1000), 
        aes(country_win, n)) + 
  geom_bar(stat='identity', fill='#7fcdbb') +
  geom_flag(aes(country=code), size = 10) +
  labs(x = '', y = 'Games won',
    title='The FIVB Beach Volleyball World Tour',
    subtitle='Brazil are the champs (since 2000)') +
  scale_x_discrete(labels=NULL, expand=c(0,0)) +
  scale_y_continuous(labels=scales::comma, expand=c(0,0)) +
  theme(axis.ticks = element_blank(),
    plot.margin=margin(0.5, 0.1, 0, 0.1, 'cm'),
    plot.subtitle=element_text(hjust=0.5),
    plot.title=element_text(hjust=0.5, face='bold'))

pdf(file='volleyball.pdf', height=6, width=9)
l<-plot_grid(top, mid, ncol=2, align='hv')
plot_grid(bot, l, nrow=2)
dev.off()

