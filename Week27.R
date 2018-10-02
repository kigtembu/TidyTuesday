###-----------------------###
## Week 27 Tidytuesday     ##
## Created by Kigen Tembu  ##
## 02/10/2018              ##
#---------------------------#

library(tidyverse)
library(RCurl)

#read data
url <- 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-10-02/us_births_2000-2014.csv'
births <- read.csv(text = getURL(url),
                   header = T)
# assign day names
births$day[births$day_of_week == 1]<- 'Monday'
births$day[births$day_of_week == 2]<- 'Tuesday'
births$day[births$day_of_week == 3]<- 'Wednesday'
births$day[births$day_of_week == 4]<- 'Thursday'
births$day[births$day_of_week == 5]<- 'Friday'
births$day[births$day_of_week == 6]<- 'Saturday'
births$day[births$day_of_week == 7]<- 'Sunday'

#Manipulate data to get total births per year per day of the week
birth1 <- select(births,-date_of_month)%>%
             group_by(year,day_of_week,day)%>%
             summarise(TotalBirths = sum(births, na.rm = TRUE))%>%
             mutate(Sqrt.TotalBirths = sqrt(TotalBirths))%>%
             arrange(year,day_of_week)

#factor day to order x-axis
birth1$day1 <- factor(birth1$day,levels = c('Monday','Tuesday','Wednesday','Thursday',
                                            'Friday','Saturday','Sunday'))
#plot
ggplot(birth1,aes(x = day1, y = as.factor(year) , fill = Sqrt.TotalBirths))+
             geom_tile()+
             labs(x = 'Day of The Week', y = 'Year', 
                  caption = 'Source:FiveThirtyEight\n@kigtembu')+
             scale_fill_gradient(name = "Sqrt(TotalBirths)",
                               low = "#FFFFFF",
                               high = "#012345") +
             theme(plot.title = element_text(hjust = 0.5))+
             ggtitle(label = 'US Births Heatmap')+
             theme_bw()

ggsave('week27.png')
