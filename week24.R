###-----------------------###
## Week 24 Tidytuesday     ##
## Created by Kigen Tembu  ##
## 10/9/2018               ##
#---------------------------#

library(tidyverse)
library(RCurl)
library(sf)
library(tmap)
library(spData)
library(spDataLarge)

#animation
devtools::install_github("yihui/animation")

# Read in Data
url <- 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-09-11/cats_vs_dogs.csv'
pets <- read.csv(text = getURL(url),
                     header = T)
pets1 <- select(pets,state,dog_population,cat_population)%>%
              mutate(state1 = as.character(state))%>%
              rename(NAME = state1,Cats = cat_population, Dogs = dog_population)%>%
              select(-state)%>%
              gather(key = type,value = Population ,-NAME)


# US states sf from spData 

pets_sf <- left_join(us_states,pets1,by = 'NAME')

#map

petsanim = tm_shape(pets_sf)+
            tm_polygons()+
            tm_dots(size = 'Population')+
            tm_facets(along = 'type' ,nrow = 1, free.coords = FALSE)

tmap_animation(petsanim,filename = 'petsanim.gif',delay = 40)