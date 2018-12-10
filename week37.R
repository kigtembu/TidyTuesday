###-----------------------###
## Week 37 Tidytuesday     ##
## Created by Kigen Tembu  ##
## 10/12/2018              ##
#---------------------------#

# packages ---------------------------------------------------------------

library(tidyverse)
library(skimr)
library(ggthemes)

# Read Data ---------------------------------------------------------------
url <- 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-12-11/nyc_restaurants.csv'
nyc_restaurants <- read_csv(url)
skim(nyc_restaurants)

violations <- nyc_restaurants %>% select( boro, grade, violation_code) %>%
              filter(!is.na(violation_code))%>%
              group_by(boro)%>%
              count(violation_code)%>%
              summarise(total_violations = sum(n))%>%
              filter( boro != 'Missing') %>%
              mutate( boro = fct_reorder(boro,total_violations))%>%
              ggplot(aes(boro,total_violations))+
              geom_col(fill = 'steelblue')+
              geom_text(aes(label = total_violations), hjust = 1)+
              coord_flip()+
              labs(x = 'Borough', y = 'Code Violations',
                   title = 'Restaurant Code Violations\nby New york Borough')+
              theme_wsj()


restaurants <- nyc_restaurants %>% select( boro,camis) %>%
               distinct(camis,.keep_all = TRUE)%>%
               group_by(boro)%>%
               count(camis) %>%
               summarise(Total_restaurants = sum(n)) %>%
               filter( boro != 'Missing') %>%
               mutate( boro = fct_reorder(boro,Total_restaurants))%>% 
               ggplot(aes(boro,Total_restaurants))+
               geom_col(fill = 'steelblue')+
               geom_text(aes(label = Total_restaurants), hjust = 1)+
               coord_flip()+
               labs(x = 'Borough', y = '# of Restaurants',
               title = 'Restaurants by New york Borough')+
               theme_wsj()

ggsave('violations.png',plot = violations,width = 8.08 , height = 4.78)
ggsave('restaurants.png',plot = restaurants,width = 9.08, height = 4.78)
