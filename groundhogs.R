library(tidyverse)
## tidytuesday for use-R group. 14th Feb 2024
## James Robinson


tuesdata <- tidytuesdayR::tt_load(2024, week = 5)


groundhogs <- tuesdata$groundhogs
predictions <- tuesdata$predictions

predictions<-predictions %>% 
	left_join(groundhogs %>% select(id, latitude, type)) %>% 
 	filter(!is.na(shadow)) %>% 
 	mutate(p = ifelse(shadow ==TRUE, 1, 0))

prob<-predictions %>% group_by(type) %>% 
	summarise(n = length(p),p = mean(p))


ggplot(prob, aes(fct_reorder(type,p), p, alpha=log(n))) +
	geom_col() +
	coord_flip() +
	theme_classic()

ggplot(predictions, aes(latitude, fill=shadow)) +
	geom_density() +
	coord_flip()