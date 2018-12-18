###-----------------------###
## Week 38 Tidytuesday     ##
## Created by Kigen Tembu  ##
## 10/12/2018              ##
#---------------------------#


# Packages ----------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(ggalt)


# Raw Data ----------------------------------------------------------------

url <- 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-12-18/allCetaceanData.csv'
cetaceans_raw <- read_csv(url)

cetaceans_raw %>% count(status,sort = TRUE)

cetaceans_raw %>% filter(acquisition == 'Born'| acquisition == 'Capture') %>% 
                  select(status,acquisition) %>% 
                  filter(!(status %in% c('Released?','Escaped (Found Dead)'))) %>% 
                  count(status,acquisition,sort = TRUE) %>% 
                  spread(acquisition,n) %>% 
                  mutate(Capture = if_else(is.na(Capture),0,as.double(Capture))) %>% 
                  ggplot(aes(y = status,x = Born , xend = Capture))+
                  geom_dumbbell(size_x = 0.25,size_xend = 0.75,
                                colour_x = 'coral2',
                                colour_xend = 'steelblue')+
                  labs(x = '# of cetaceans',y = 'Current Status',
                       title = 'Current Status of Born vs Captured Cetaceans')+
                  theme_light()
                    
   
                  
