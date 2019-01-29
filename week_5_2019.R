##---------------##
## Tidy Tuesday  ##
## Week 5 2019   ##
## 1/29/2019     ##

# Packages ----------------------------------------------------------------

library(tidyverse)
library(sp)
library(spData)
library(spDataLarge)
library(tmap)
library(sf)


# Raw data ----------------------------------------------------------------

state_milk_production <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-29/state_milk_production.csv')


# Process -----------------------------------------------------------------


state_milk_production_processed <- state_milk_production %>% filter(!is.na(year)) %>% 
        mutate(decade = 10 * (year %/% 10),milk_produced = as.double(milk_produced)) %>% 
        group_by(decade,state) %>% 
        summarise('Milk Per State(lbs)' = sum(milk_produced)) %>% 
        rename(NAME = state)


# Merge -------------------------------------------------------------------

state_milk_merge <- left_join(state_milk_production_processed,us_states, by = 'NAME' )

state_milk_sf <- st_as_sf(state_milk_merge)
# Plot --------------------------------------------------------------------

milk_map =  tm_shape(us_states)+ tm_polygons() +
            tm_shape(state_milk_sf) +
            tm_dots(size = "Milk Per State(lbs)")+
            tm_layout(frame = FALSE, bg.color = NA)+
            tm_facets(along = 'decade',free.coords = FALSE)

tmap_animation(milk_map,filename = 'week5_2019.gif',delay = 30)
