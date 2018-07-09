### Tidyverse tuesday challenge
### Week 14 03/07/2018
### created by Kigen tembu

#insert libraries here
library(RCurl)
library(rworldmap)
library(maptools)
library(leaflet)
library(tidyverse)
library(sp)
library(rgeos)
library(rgdal)
library(ggmap)
library(ggplot2)
library(raster)
  
                                 
#reading in data
                 
le_df <-read.csv(text=getURL("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/week14_global_life_expectancy.csv"), 
                      header=T)

#filter data for yr 2013 and missing codes
le_df1<-filter(le_df,year == 2013)%>%
        filter(code!= "")%>%
        mutate(NAME = country)


worldmap <- getMap()

str(worldmap@data)


#merge data to shape file

dat <- merge(worldmap,le_df1, by = 'NAME',duplicatesGeoms = TRUE)
some_dat <- subset(dat, year == 2013)


#sequential colour palette(min to max life expectancy)
pte <- seq((min(some_dat@data$life_expectancy)*0.2)/0.2,(max(some_dat@data$life_expectancy)*0.2)/0.2,10)

pal <- colorNumeric('magma', domain = pte)

leaflet(subset(some_dat, !is.na(code)),options = leafletOptions(minZoom = 2,maxZoom = 12)) %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik)%>%
  addLegend(pal= pal,values = pte, 'bottomright',title = 'Average Years',opacity = 0.6)%>%

  addPolygons(stroke = TRUE,weight = 1, color = 'Black',
              fillColor = ~pal(some_dat@data$life_expectancy),fillOpacity = 1,
              label = ~paste0(NAME,":",prettyNum(life_expectancy,big.mark = ',',format ='f')),
              highlight = highlightOptions(
                          weight = 5,
                          color = '#666',
                          dashArray = '',
                          fillOpacity = 0.8,
                          bringToFront = TRUE)
              )
             

