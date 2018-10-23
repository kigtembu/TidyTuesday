###-----------------------###
## Week 30 Tidytuesday     ##
## Created by Kigen Tembu  ##
## 23/10/2018              ##
#---------------------------#


# packages ----------------------------------------------------------------

library(tidyverse)
library(RCurl)
library(skimr)
library(ggridges)
library(gghighlight)
library(scales)
library(viridis)


# read data --------------------------------------------------------------

url <- 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-10-23/movie_profit.csv'
movies <- read.csv(text = getURL(url),header = TRUE)


# data wrangling ----------------------------------------------------------
skim(movies)

movies1 <- select(movies,genre,movie,worldwide_gross)%>%
           ggplot(aes(x=worldwide_gross,y=genre))+
           stat_density_ridges(scale = 0.9,jittered_points = TRUE,position = 'raincloud',
           alpha = 0.7,quantile_lines = TRUE,quantiles = 2)+
           scale_x_continuous(labels = comma)+
           theme_ridges()+
           labs(title = 'Distribution of Worldwide Movie Revenue by Genre',
                x = 'Worldwide Gross Revenue($)',
                y = 'Movie genre',
                caption = 'Source:the_numbers.com/fivethirtyeight\n@kigtembu')
           
ggsave('week30.png',plot = movies1)

movies2 <- select(movies,genre,movie,worldwide_gross,production_budget)%>%
           mutate(revenue_cost_ratio = (worldwide_gross/production_budget) )%>%
           ggplot(aes(x=revenue_cost_ratio,y=genre,fill =0.5 - abs(0.5-..ecdf..)))+
           stat_density_ridges(geom = 'density_ridges_gradient',calc_ecdf = TRUE,
                               scale = 0.9,jittered_points = TRUE,position = 'raincloud',
                      alpha = 0.7,quantile_lines = TRUE,quantiles = 2)+
           scale_fill_viridis(name = 'Tail Probability',direction = -1)+
           theme_ridges()+
           labs(title = 'Distribution of Revenue to Cost Ratio by Genre',
           x = 'Revenue to Cost Ratio',
           y = 'Movie genre',
           caption = 'Source:the_numbers.com/fivethirtyeight\n@kigtembu')

ggsave('week30_Rev.png',plot = movies2)


