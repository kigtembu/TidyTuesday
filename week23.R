###-----------------------###
## Week 22 Tidytuesday     ##
## Created by Kigen Tembu  ##
## 22/08/2018              ##
#---------------------------#

library(tidyverse)
library(RCurl)
library(hrbrthemes)
# Read in Data

fastfood <- read.csv(text = getURL('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-09-04/fastfood_calories.csv'),
                header = T)

fastfood1 <- fastfood %>%
               select(restaurant,item,trans_fat) %>%
               distinct(restaurant,item,.keep_all = TRUE) %>%
               group_by(restaurant) %>%
               summarise(total_tr_ft = sum(trans_fat,na.rm = TRUE))%>%
               arrange(desc(total_tr_ft))

# ggplot

ggplot(fastfood1,aes(x = reorder(restaurant,total_tr_ft), y = total_tr_ft))+
      geom_bar(stat = 'identity',position = position_stack(reverse = TRUE))+
      geom_text(aes(label = total_tr_ft, hjust = 0))+
      coord_flip()+
      labs(x = 'Restaurant',y = 'Total Transfat in Menu',
           title = 'Transfat in Fast Food Restaurant Menus',
           caption = 'Source: Fastfoodnutrition.com\n @kigtembu')+
      theme_ipsum_rc(grid = 'X')

ggsave('week23.png')


