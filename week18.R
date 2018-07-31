### Tidytuesday  challenge
##  created by kigen Tembu
##  31/7/2018

## libraries

library(tidyverse)
library(readxl)
library(lubridate)
library(ggplot2)

#--- Read in data
das <- read_excel('week18_dallas_animals.xlsx',sheet = 1)

#--- Transform and manipulate data
das1 <- das %>% filter(!is.na(intake_type))%>%
        select(animal_id,animal_type,intake_type,intake_date)%>%
        distinct %>%
        mutate(mth = month(intake_date,label = TRUE,abbr = TRUE),
                yr = year(intake_date),
                 yr2 = paste(mth,yr,sep =' '))%>%
        #find the three most common intake types
        group_by(intake_type)%>%
        summarise(count=n())%>%
        arrange(desc(count))%>%
        mutate(Tot = cumsum(count),mx = max(Tot),Per = (count/mx)*100)

#--- 'stray', 'owner surrender' and 'confiscated'  contribute to 90% of the data
#--- group all other intake type categories as 'other'

das2 <- das %>% filter(!is.na(intake_type))%>%
        select(animal_id,animal_type,intake_type,intake_date)%>%
        distinct %>%
        mutate(mth = month(intake_date,label = TRUE,abbr = TRUE),
               yr = year(intake_date),
               yr2 = paste(mth,yr,sep =' '),
               intake_type1 = str_replace_all(intake_type,c('LOST REPORT'= 'OTHER','WILDLIFE' = 'OTHER',
                                                            'FOSTER' = 'OTHER','FOUND REPORT'= 'OTHER',
                                                             'TRANSFER'='OTHER')))
      


#--- Plot

ggplot(das2,aes(x=yr2))+
       geom_bar(aes(fill = intake_type1))+
       scale_fill_brewer(palette = 'RdBu')+
       guides(fill =guide_legend(title = 'Intake Type'))+
       labs(title = "Number of animals taken in by Dallas Animal Shelter" ,
            x = "", y = 'Number of Animals',caption = 'Source:Dallas OpenData')+
       theme(axis.text.x = element_text(angle = 90,size = 10,face = 'plain'),
            axis.text.y = element_text(size = 10, face = 'plain'),
            legend.title = element_text(size = 10,face = 'bold'),
            legend.text = element_text(size = 9, face = 'plain'))
       





        