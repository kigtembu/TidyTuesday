###-----------------------###
## Week 34 Tidytuesday     ##
## Created by Kigen Tembu  ##
## 20/11/2018              ##
#---------------------------#

# packages ----------------------------------------------------------------
library(readr)
library(tidyverse)


# read in data ------------------------------------------------------------

thanksgiving_raw <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-11-20/thanksgiving_meals.csv')


# wrangling and plotting ----------------------------------------------------

dessert_df<- thanksgiving_raw %>% select(id,us_region,starts_with('dessert'))%>%
          gather(key = dessert_question,value = dessert,-c(id,us_region),na.rm = TRUE)%>%
          filter(!is.na(us_region),dessert_question != 'dessert12')

#change 'Other(please specify)' to 'Other
dessert_df$dessert[dessert_df$dessert =='Other (please specify)']<-'Other'

dessert_df1 <- dessert_df %>% mutate(dessert = as_factor(dessert),us_region = as_factor(us_region))%>%
               group_by(dessert,us_region)%>%
               count()%>%
               ggplot(aes(x = us_region,y = dessert, fill = n))+
               geom_tile()+
               scale_fill_gradient(
                  name = 'Count of Desserts',
                  low = "#FFFFFF",
                  high = "#012345"
               )+
               theme_classic()+
               theme(strip.placement = "outside",axis.text.x = element_text(angle = 90),
                     axis.ticks = element_blank(),axis.line = element_blank())+
               labs(x = 'US Region',y = 'Dessert',title = 'Heatmap of Dessert',
                    caption = 'Source:FiveThirtyEight\n @kigtembu')

# save plot ---------------------------------------------------------------

ggsave('week34.png',plot = dessert_df1)
  

          

