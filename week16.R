### Tidytuesday  challenge
##  created by kigen Tembu
##  17/7/2018

## libraries

library(tidyverse)
library(readxl)
library(ggplot2)
library(gridExtra)
library(grid)
library(ggpubr)

#read in data

exercise <-read_excel('week16_exercise.xlsx',sheet = 1)

#transpose data, filter out NAs then take top 5 per working category

exercise1 <- gather(exercise,'category','percent',3:9)%>%
            filter( percent != 'NA')%>%
            group_by(category)%>%
            mutate(percent1 = as.numeric(percent))%>%
            top_n(5,percent1)%>% 
            select(state,category,percent1)%>%
            arrange(category,desc(percent1))%>%
            filter(str_detect(category,'working'))
#plots 



plot1 <- ggplot(filter(exercise1,category == 'men_working'),aes(x =reorder(state,-percent1),y =percent1))+
         geom_bar(stat = 'identity',position = position_stack(reverse = FALSE),fill = 'seagreen1',width = 0.8)+
         theme_minimal()+
         labs(title = 'Male Workers', y = 'Percent',x = '')+
         geom_text(aes(y = percent1,label= state,hjust = 0.65))+
         theme(axis.ticks = element_blank(),axis.text.x = element_blank())+
         coord_polar()

plot2 <- ggplot(filter(exercise1,category == 'women_working'),aes(x=reorder(state,-percent1),y=percent1))+
         geom_bar(stat= 'identity',position = position_stack(reverse = FALSE),fill = 'seagreen1',width = 0.7)+
         theme_minimal()+
         labs(title = 'Female Workers', y = 'Percent',x ='')+
         geom_text(aes(y = percent1,label= state,hjust = 0.65 ))+
         theme(axis.ticks = element_blank(),axis.text.x = element_blank())+
         coord_polar()

plot3 <- ggplot(filter(exercise1,category == 'men_nonworking'),aes(x=reorder(state,-percent1),y=percent1))+
         geom_bar(stat='identity',position = position_stack(reverse = FALSE),fill = 'seagreen1',width = 0.8)+
         theme_minimal()+
         labs(title = 'Male Nonworkers', y = 'Percent',x ='')+
         geom_text(aes(y = percent1,label= state,hjust = 0.8 ))+
         theme(axis.ticks = element_blank(),axis.text.x = element_blank())+
         coord_polar()

plot4 <- ggplot(filter(exercise1,category == 'women_nonworking'),aes(x=reorder(state,-percent1),y =percent1))+
         geom_bar(stat = 'identity',position = position_stack(reverse = FALSE),fill = 'seagreen1',width = 0.8)+
         theme_minimal()+
         labs(title = 'Female Nonworkers', y = 'Percent',x ='')+
         geom_text(aes(y = percent1,label= state,hjust = 0.9 ))+
         theme(axis.ticks = element_blank(),axis.text.x = element_blank())+
         coord_polar()

footnote <- text_grob('States with the highest percentage of adults per category\nwho met both aerobic and muscle strengthening federal guidelines\n@kigtembu',face = 'bold',size =10)

grid.arrange(plot1,plot2,plot3,plot4,ncol = 2, nrow =2 , bottom = footnote )


