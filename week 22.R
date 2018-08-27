###-----------------------###
## Week 22 Tidytuesday     ##
## Created by Kigen Tembu  ##
## 22/08/2018              ##
#---------------------------#

library(readxl)
library(tidyverse)
library(ggplot2)
library(RCurl)

nfl <- read.csv(text = getURL('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-08-28/nfl_2010-2017.csv'),
                              header = T)


tds <- nfl %>%
        select(team,name,game_year,game_week,position,rush_tds,rec_tds,pass_tds)%>%
        filter(!is.na(game_year))%>%
        group_by(game_year)%>%
        summarise(rush = sum(rush_tds,na.rm = TRUE),pass = sum(pass_tds,na.rm = TRUE))
tds1 <- tds%>%
         gather(key = type_td,value = td,rush,pass)

ggplot(tds1,aes(x=as.factor(game_year),y=td))+
      geom_bar(stat = 'identity',aes(fill = type_td),position = 'dodge')+
      guides(fill=guide_legend(title ='Type of\nTouchdown',title.theme = element_text(size = 10)))+
      labs(title = 'Types of Touchdowns in the NFL',y='Number of Touchdowns',
           x= 'Year',caption = 'Source:Profootball Reference\n@kigtembu')+
      theme_minimal()

ggsave('week22.png')

