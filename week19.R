### TidyTuesday 6/08/2018
## Created by Kigen Tembu
# Week 19

#Libraries
library(RCurl)
library(tidyverse)
library(ggplot2)

#Read in data

airlines <-read.csv(text=getURL("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/week19_airline_safety.csv"), 
                      header=T)
airlines_1 <- airlines %>% filter(!is.na(avail_seat_km_per_week))%>%
              mutate(yr1 = str_replace_all(year_range,c('85_99'='Y8599','00_14'='Y0014')))%>%
              spread(yr1,avail_seat_km_per_week)%>%
              filter(!is.na(Y8599))%>%
              select(airline,type_of_event,n_events,Y8599)%>%
              filter(!is.na(airline))

airlines_2 <-  airlines %>% filter(!is.na(avail_seat_km_per_week))%>%
               mutate(yr1 = str_replace_all(year_range,c('85_99'='Y8599','00_14'='Y0014')))%>%
               spread(yr1,avail_seat_km_per_week)%>%
               filter(!is.na(Y0014))%>%
               select(airline,type_of_event,n_events,Y0014)%>%
               filter(!is.na(airline))


#Merge the two by airline;

airline_mg <- inner_join(airlines_1,airlines_2,by = c('airline','type_of_event'))%>%
              mutate(diff_events = n_events.y - n_events.x)%>%
              filter(type_of_event == 'incidents')%>%
              arrange(desc(diff_events))%>%
              top_n(3)%>%
              gather(key = event , value = n , n_events.x, n_events.y)%>%
              mutate(yr = str_replace_all(event,c('n_events.x'='1985-1999','n_events.y'='2000-2014')))
              

#plot 
ggplot(airline_mg,aes(airline,n))+
      geom_bar(aes(fill = yr),position = 'dodge', stat = 'identity')+
      labs(title = 'Airlines with the most increase in incidents comparing\nthe period 1985-2000 with 2000-2014',
           x = 'Airline', y = 'Number of Incidents', caption = 'Source:FiveThirtyEight\n@kigtembu')+
      coord_flip()+
      guides(fill =guide_legend(title = 'Period'))+
      theme_light()
