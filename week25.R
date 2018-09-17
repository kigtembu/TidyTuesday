###-----------------------###
## Week 25 Tidytuesday     ##
## Created by Kigen Tembu  ##
## 17/9/2018               ##
#---------------------------#

#libraries

library(tidyverse)
library(ggplot2)
library(RCurl)
library(sf)
library(tmap)
library(spData)
library(spDataLarge)
library(rvest)

#animation
devtools::install_github("yihui/animation")

#read data
url <- 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-09-18/us-airports.csv'

airports <- read.csv(text = getURL(url),
                     header = T)


airport1 <- select(airports,state,year,passengers)%>%
            group_by(state,year)%>%
            summarise(total = sum(passengers,na.rm = TRUE))

statenames <- tibble(state = state.abb,NAME =state.name)

#merge to get full state names

airport2 <- left_join(airport1,statenames, by = 'state')%>%
            rename('Total Passengers'= total)

#merge with us_states sf

airport3 <- left_join(airport2,us_states, by = 'NAME')
airport_sf <- st_as_sf(airport3)


airport_anim = tm_shape(us_states) + 
               tm_polygons() +
               tm_shape(airport_sf)+tm_bubbles(size = 'Total Passengers',col ='black', border.col = NA)+
               tm_facets(along = 'year', free.coords = FALSE) 


tmap_animation(airport_anim,filename = 'airport.gif',delay = 90)
                
