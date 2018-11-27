###-----------------------###
## Week 35 Tidytuesday     ##
## Created by Kigen Tembu  ##
## 27/11/2018              ##
#---------------------------#

# packages ----------------------------------------------------------------
library(readr)
library(tidyverse)
library(skimr)
library(ggrepel)


# read in data ------------------------------------------------------------

bridges_raw <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-11-27/baltimore_bridges.csv')
skim(bridges_raw)

# manipulate and plot -----------------------------------------------------


bridge_cond <- bridges_raw %>% select(yr_built,bridge_condition,total_improve_cost_thousands,avg_daily_traffic,carries)%>%
                    ggplot(aes(x = yr_built,y = avg_daily_traffic,label = carries))+
                    geom_jitter(color = 'steelblue')+
                    geom_label_repel(aes(label=ifelse(avg_daily_traffic>200000,carries,'')),
                                     box.padding = 0.35,point.padding = 0.5,alpha = 0.6,vjust = 0)+
                    facet_wrap(~bridge_condition)+
                    theme_classic()+
                    labs(x = 'Year Bridge was Built',
                         y = 'Average Daily Traffic',
                         title = 'Scatter Plot of Daily Traffic Vs Year Bridge was Built',
                         subtitle = 'Faceted by Bridge Condition',
                         caption = 'Source:Baltimore Sun\n@kigtembu')
bridge_cond

ggsave('week35.png',plot = bridge_cond)




