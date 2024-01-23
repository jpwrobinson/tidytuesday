library(tidyverse)
## tidytuesday for use-R group. 24th Jan 2024
## James Robinson


tuesdata <- tidytuesdayR::tt_load(2024, week = 2)

canada_births_1991_2022 <- tuesdata$canada_births_1991_2022
nhl_player_births <- tuesdata$nhl_player_births
nhl_rosters <- tuesdata$nhl_rosters
nhl_teams <- tuesdata$nhl_teams

births<-nhl_rosters %>% 
  # tidy some variables and prepare to plot
  mutate(cat=ifelse(birth_country == 'CAN', 'Canada', 'Not Canada'),
         season = as.numeric(substr(season, 1, 4)),
         month = lubridate::month(birth_date)) %>% 
  group_by(team_code, season) %>% 
  mutate(roster = n_distinct(player_id)) %>% 
  # estimate average birth month
  group_by(team_code, season, cat, month, roster) %>% 
  summarise(births_per_month = n_distinct(player_id)) %>% 
  ungroup() %>% 
  mutate(prop_births = births_per_month / roster) %>% 
  group_by(season, cat, month) %>% 
  summarise(b = mean(prop_births)*100) %>% 
  mutate(month_name = month.abb[month])

ggplot(births, aes(season, fct_reorder(month_name, -month), size=b, col=cat)) + 
  geom_point(position = position_dodge(width=0.7)) +
  scale_size(range=c(.01, 5)) +
  labs(x='', y = '', col='', size = '% of roster',
       subtitle = 'Average birth month of NHL teams') +
  theme_classic()
