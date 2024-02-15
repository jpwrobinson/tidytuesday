library(tidyverse)
library(cowplot)
## tidytuesday for use-R group. 14th Feb 2024
## James Robinson


tuesdata <- tidytuesdayR::tt_load(2024, week = 5)


groundhogs <- tuesdata$groundhogs
predictions <- tuesdata$predictions

predictions<-predictions %>% 
	left_join(groundhogs %>% select(id, latitude, type)) %>% 
 	filter(!is.na(shadow)) %>% 
 	mutate(p = ifelse(shadow ==TRUE, 1, 0),
 		p_name = ifelse(shadow ==TRUE, 'Spring', 'Winter'))

prob<-predictions %>% group_by(type) %>% 
	summarise(n = length(p),p = mean(p))

samples<-predictions %>% group_by(year, p, p_name) %>% 
		summarise(n = length(id)) %>% 
		mutate(n_dir = ifelse(p ==0, n*-1, n)) %>% 
		pivot_wider(-c(p, n), names_from = p_name, values_from = n_dir)


g1<-ggplot(prob, aes(fct_reorder(type,p), p)) +
	geom_col(aes(alpha=log(n))) +
	# geom_jitter(data = predictions, aes(fct_reorder(type,p), p)) +
	coord_flip() +
	labs(x = '', y= 'p(summer)') +
	ggthemes::theme_economist() +
	theme(axis.text.y = element_text(hjust=1))

g2<-ggplot(predictions, aes(latitude, fill=shadow)) +
	geom_density() +
	ggthemes::theme_economist()

g3<-ggplot(samples, aes(year, Spring)) + 
		geom_segment(aes(xend = year, y = Spring, yend = Winter, group=year), col='grey') +
		geom_point(col='#EA8300') +
		geom_point(aes(year, Winter), col='#3100CE') +
		ggthemes::theme_economist() +
		labs(y = 'Number of predictions', x = '') +
		scale_y_continuous(breaks=seq(-40, 40, by = 20), 
							labels =c(40, 20, 0, 20, 40))

pdf('groundhogs.pdf', height=7, width=12)
plot_grid(g1, 
	plot_grid(g2, g3, nrow=2),
	ncol = 2
	)
dev.off()