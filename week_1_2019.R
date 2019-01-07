##---------------##
## Tidy Tuesday  ##
## Week 2 2019   ##
## 1/7/2019      ##


# packages ----------------------------------------------------------------

library(tidyverse)
library(skimr)


# Raw data ----------------------------------------------------------------

url <- 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-08/IMDb_Economist_tv_ratings.csv'
raw_tv_ratings <- read_csv(url)

#Get  10 longest running shows
longest_running <- raw_tv_ratings %>% count(title,sort = TRUE) %>% 
                   top_n(10)

#merge back to get ratings

longest_running_tv_ratings <- inner_join(longest_running,raw_tv_ratings,by = 'title') %>% 
                              select(title,av_rating)

#plot
plot_ratings <- ggplot(longest_running_tv_ratings,aes(x=title,y=av_rating,fill=title))+
                geom_boxplot()+
                theme_light()+
                guides(fill = FALSE)+
                coord_flip()+
                labs(
                  title = 'Boxplot of the Ratings of the 10 longest Running Shows',
                  x = 'Show Title',
                  y = 'Average Ratings',
                  caption = 'Source:The Economist\nPlot created by @kigtembu'
                )
ggsave('week_1_2019.png')
