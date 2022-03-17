library(tidyverse)
## tidytuesday for use-R group. Weds 16th March 2022
## James Robinson

tuesdata <- tidytuesdayR::tt_load(2022, week = 10)

erasmus <- tuesdata$erasmus

# Or read in the data manually
erasmus <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-08/erasmus.csv')
erasmus<-erasmus[!erasmus$receiving_country_code == erasmus$sending_country_code,]


e2<-erasmus %>% 
        pivot_longer(c(sending_country_code,receiving_country_code), names_to = 'direction', values_to = 'country') %>% 
        mutate(direction = toupper(str_replace_all(direction, '_country_code', ''))) %>% 
        group_by(country, direction) %>% 
        summarise(n = sum(participants)) 

# switch levels so send is before receive
e2$direction<-factor(e2$direction, levels=unique(e2$direction)[c(1,2)])

# get country names
library(countrycode)
e2$countryname<-countrycode(e2$country, origin = 'iso2c', destination= 'country.name')

## some errors fix by hand
unique(e2$country[is.na(e2$countryname)])
e2$countryname[e2$country=='EL']<-'Greece'
e2$countryname[e2$country=='UK']<-'UK'
e2$countryname[e2$country=='XK']<-'Kosovo'

## add colours based on direction (more or less sending/receiving)
# https://ibecav.netlify.app/post/more-on-slopegraphs/
colorvect <- e2 %>% group_by(countryname) %>% 
    summarise(difference = diff(n)) %>% 
    mutate(whatcolor = case_when(
        difference == 0 ~ "light gray",
        difference > 0 ~ "red",
        difference < 0 ~ "black"
    )) %>%
    select(countryname, whatcolor) %>%
    tibble::deframe()

# https://r-charts.com/evolution/newggslopegraph/
library(CGPfunctions)

gp<-newggslopegraph(
    e2 %>% filter(n > 100), direction, n, countryname, 
    LineThickness = 0.4,
    YTextSize = 2,
    XTextSize = 10,
    LineColor = colorvect,
    TitleJustify = 'center',
    SubTitleJustify = 'right',
    SubTitleTextSize = 8,
    Title = 'Direction of ERASMUS travel',
    SubTitle = 'Number of ERASMUS students 2014-19\n\n',
    Caption = '#tidytuesday',
    ThemeChoice = 'tufte')


pdf(file = 'erasmus.pdf', height=15, width=4)
gp
dev.off()
