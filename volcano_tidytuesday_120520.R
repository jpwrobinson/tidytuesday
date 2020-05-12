setwd('tidytuesday')
library(tidyverse); library(funk)

# volcano <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv')
# eruptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv')
# events <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/events.csv')

head(volcano) %>% data.frame()
head(eruptions) %>% data.frame()
head(events) %>% data.frame()


obs<-eruptions %>% filter(evidence_method_dating == 'Historical Observations' & start_year > 1800 & volcano_name !='Unknown Source') %>%
          select(volcano_name, volcano_number, vei, start_year, end_year, latitude, longitude) %>%
          group_by(volcano_name) %>% mutate(freq = length(volcano_name), max.vei = max(vei, na.rm=TRUE)) %>%
          ungroup() 

obs<-left_join(obs, volcano) %>%
          mutate(volcano_name = fct_reorder(volcano_name, latitude))
obs$region<-factor(obs$region)



plotter<-obs %>% filter(vei > 3 & elevation > 0) %>% mutate(long.lab=round(longitude, 0), population_within_10_km=log10(population_within_10_km+1)) 

left<-ggplot(plotter, aes(volcano_name, start_year, fill = population_within_10_km, size=vei)) +
  geom_point(alpha=0.9, shape=21,colour='black',stroke=1.2) + 
  coord_flip() +
  labs(x='', y='', title='', subtitle='Volcanic Explosivity Index, coloured by population < 10 km') +
  # sec_axis(labels=longs) +
  scale_size_area(breaks=c(4,5,6), trans='exp', max_size=30) +
  scale_fill_gradient(na.value='white', low='#ffffcc', high='#800026') +
  scale_y_continuous(breaks=seq(1800, 2020, 20)) +
  scale_x_discrete(expand=c(0.025,0.025)) +
  theme_classic() +
  theme(legend.position = 'none',
        panel.grid.major.y = element_line(colour='grey90'), 
        axis.text = element_text(size=9, colour='white'),
        panel.border = element_blank(),
        title = element_text(colour='white', size=12, face='bold', hjust=0),
        plot.subtitle = element_text(colour='white', size=9, face='bold', hjust=0),
        plot.background = element_rect(color='black',fill = "black"),
        panel.background = element_rect(fill = "black",
                                        colour = "black",
                                        size = 0.5, linetype = "solid"),
        plot.margin=unit(c(0.5, 0.2, 0, 0), 'cm'))

plotter2<-plotter %>% group_by(long.lab, volcano_name, region) %>% summarise(freq = unique(freq))
lab<-plotter2$long.lab
lab<-ifelse(lab > 0, paste0(lab, 'N'), paste0(-lab, 'S'))

right<-ggplot(plotter2) +
  geom_bar(aes(volcano_name, freq, fill=region), stat='identity') +
  geom_text(aes(x=volcano_name, y=freq, label = region, col=region), size=2,hjust = -0.1) +
  coord_flip() + 
  labs(y = 'Number of eruptions since 1800', x = '', title='', subtitle='Number of eruptions, ordered from north - south') +
  theme_classic() + 
  theme(
    axis.text.y = element_text(size=9, colour='white', hjust=0.5),
    axis.text.x = element_text(size=9, colour='white'),
        axis.ticks=element_blank(), 
    axis.line.x = element_line(color='white'),
        legend.position = 'none',
    plot.background = element_rect(color='black', fill = "black"),
    panel.border = element_blank(),
    plot.subtitle = element_text(colour='white', size=9, face='bold', hjust=0),
    panel.background = element_rect(fill = "black",
                                    colour = "black",
                                    size = 0.5, linetype = "solid"),
        plot.margin=unit(c(0.5, 1, 0, -0.5), 'cm')) +
  scale_y_continuous(expand=c(0, 0), breaks=seq(0, 100, 25)) +
  scale_x_discrete(expand=c(0.025,0.025), label=lab) 



g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

legend <- g_legend(ggplot(plotter, aes(volcano_name, start_year, fill = population_within_10_km, size=vei)) +
                     geom_point(alpha=0.9, shape=21,colour='black',stroke=1.2) +
                     scale_size_area(breaks=c(4,5,6), trans='exp', max_size=30, name='Volcanic Explositivity Index') +
                     scale_fill_gradient(na.value='white', low='#ffffcc', high='#800026', breaks=seq(0, 6, 2), labels=c(0, 100, '10,000', '1 million'), name='Population < 10 km') +
                      theme(plot.margin=unit(c(0, 0, 0, 0), 'cm'),
                            legend.position = 'bottom',
                            legend.box.background = element_rect(color='black', fill = "black"),
                            plot.background = element_rect(color='black', fill = "black"),
                            panel.background = element_rect(color='black', fill = "black"),
                            panel.border=element_blank(),
                            legend.key=element_blank(),
                            legend.background = element_rect(fill='black'),
                        legend.text = element_text(colour='white'),
                        legend.title = element_text(colour='white')))

gleg2<-ggplot() + theme_void() + 
  annotation_custom(grob = legend, 
                    xmin = 0.5, xmax = 0.6, ymin = 0.6, ymax = 0.7) 

pdf(file = 'volcano.pdf', height=9, width=10)
t<-cowplot::plot_grid(left, right, rel_widths = c(1, 0.5), nrow=1, labels=c('Catastrophic volcano eruptions since 1880', ''), 
                   label_colour='white', label_x=-0.25)
cowplot::plot_grid(t, gleg2, nrow=2, rel_heights=c(1, 0.2))
dev.off()
