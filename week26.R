###-----------------------###
## Week 26 Tidytuesday     ##
## Created by Kigen Tembu  ##
## 24/9/2018               ##
#---------------------------#

library(tidyverse)
library(tmap)
library(RCurl)
library(sf)
library(spData)
library(spDataLarge)

## read in data
url<- 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-09-25/africa_species.csv'
Invasive_sp <- read.csv(text = getURL(url),
                        header = T) 
Invasive_sp1 <- select(Invasive_sp,country,species)%>%
                distinct()%>%
                count(country)%>%
                mutate('No. of Species'= n,name = as.character(country))%>%
                mutate( name_long = str_replace_all(name,c('United Republic of Tanzania' = 'Tanzania',
                                                           'Gambia \\(the\\)' = 'The Gambia',
                                                            'Swaziland' = 'eSwatini')))
                                                        

##  Merge with Africa shape file

africa_sf <- filter(world,continent == 'Africa')
africa_mg <- left_join(africa_sf,Invasive_sp1,by = 'name_long')

## plot
breaks <- c(0,2.5,5,7.5,10,12.5,15,17.5,20,22.5)*100
africa <-tm_shape(africa_mg)+
         tm_polygons(col = 'No. of Species',breaks = breaks)+
         tm_style('classic')
tmap_save(tm = africa,'week26.png')

                