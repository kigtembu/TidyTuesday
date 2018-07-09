### Tidyverse tuesday challenge
### Week 15 09/07/2018
### created by Kigen tembu


library(readxl)
library(tidyverse)

#read in data and summarise to the 5 cities with the most breweries
brewery <- read_excel('week15_beers.xlsx',sheet =2)

brewery1 <-filter(brewery,city != '')%>%
           group_by(city)%>%
           summarise(count =n())%>%
           arrange(desc(count))%>%
           slice(1:5)
#ggplot geom bar
 ggplot(brewery1,aes(city))+
  
     geom_bar(aes(weight = count),position = position_stack(reverse = TRUE),fill = 'tan3',width = 0.6) +
     coord_flip()+
     theme_minimal()+
     geom_text(aes(y = count,hjust = -0.20,
                   label = format(brewery1$count,trim = TRUE)),color = 'tan3',size = 10) +
     geom_text(aes(y = count ,hjust = 2,
                label = format(brewery1$city,trim = TRUE)),color = 'tan4',size = 8)+
     theme(axis.ticks = element_blank(),axis.text.y = element_blank(),
           axis.title = element_blank(),axis.line = element_line(linetype = 'dotted'))
     