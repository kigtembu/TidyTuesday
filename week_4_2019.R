##---------------##
## Tidy Tuesday  ##
## Week 4 2019   ##
## 1/22/2019     ##


# Packages ----------------------------------------------------------------

library(tidyverse)

url <- 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-22/prison_summary.csv'

prisons_pop <- read_csv(url)

race_prisons_rate <- prisons_pop %>% 
  select(year,pop_category, rate_per_100000) %>% 
  filter(pop_category %in% c('Black', 'White','Asian', 'Latino', 'Native American'),
                             year >= 1990)%>% 
  group_by(year,pop_category) %>% 
  summarise(mean_rate = mean(rate_per_100000),
            median_rate = median(rate_per_100000)) %>% 
  ggplot(aes(x = year,y = mean_rate,color = pop_category))+
  geom_line()+
  theme_minimal()+
  labs(x = 'Year', y = 'Average Rate for Prison Population\nPer 100,000 People',
       color = 'Population Category', title = 'US Prison Population(1990 -2015)',
       caption = 'Source: Vera Institute\n Plot by @kigtembu')+
  theme( panel.grid = element_blank(),
         plot.background = element_rect(fill = 'black'),
         plot.title = element_text(color = 'white',face = 'bold',size = 15),
         axis.line.x = element_line(size = 0.5 , color = 'gray30'),
         axis.title.x = element_text(color = 'white',size = 13, face = 'bold'),
         axis.text.x = element_text(color = 'white'),
         axis.text.y = element_text(color = 'white'),
         axis.title.y = element_text(color = 'white',size = 13, face = 'bold'),
         plot.caption = element_text(color = 'white' ,size = 11),
         legend.background = element_rect(fill = 'black'),
         legend.text = element_text(color = 'white')
         )
ggsave('week4_2019.png') 
