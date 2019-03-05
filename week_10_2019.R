##---------------##
## Tidy Tuesday  ##
## Week 10 2019 ##
## 3/5/2019    ##

library(tidyverse)
library(scales)
library(gganimate)
library(magick)

url <-"https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-05/jobs_gender.csv"

jobs_women_raw <- read_csv(url)

comp_math <- jobs_women_raw %>% 
  filter(occupation %in% c("Statisticians",
                           "Actuaries","Mathematicians")) %>%
  select(year,occupation,total_earnings_male,total_earnings_female)%>%
  gather(gender,earnings,-c(year,occupation)) %>% 
  mutate(gender = if_else(str_detect(gender,"female"),"Female",
                          "Male")) %>% 
  ggplot(aes(x = year, y = earnings,fill = gender))+
  geom_col(position = "dodge")+
  theme_light()+
  scale_y_continuous(labels = scales::comma)+
  labs(x = "Year",y = "Estimated Median Earnings($)",
       color = "Gender",
       caption = "Source:Census Bureau\nPlot by @kigtembu",
       title = "Occupation:{closest_state}")+
  transition_states(occupation)

animate_math <- animate(comp_math)

anim_save("week_10_2019.gif") 
