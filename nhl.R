library(tidyverse)
## tidytuesday for use-R group. 24th Jan 2024
## James Robinson


tuesdata <- tidytuesdayR::tt_load(2024, week = 2)

canada_births_1991_2022 <- tuesdata$canada_births_1991_2022
nhl_player_births <- tuesdata$nhl_player_births
nhl_rosters <- tuesdata$nhl_rosters
nhl_teams <- tuesdata$nhl_teams