###-----------------------###
## Week 29 Tidytuesday     ##
## Created by Kigen Tembu  ##
## 15/10/2018              ##
#---------------------------#


# packages ----------------------------------------------------------------

library(tidyverse)
library(RCurl)
library(skimr)
library(ggthemes)
library(ggalt)

#read in data

url <- 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-10-16/recent-grads.csv'

Recent_grads <- read.csv(text = getURL(url),header = T)

skim(Recent_grads)

Recent_grads1 <- select(Recent_grads,Major,Major_category,
                        Sample_size,Total,P25th,P75th)%>%
                 mutate(iqr = P75th-P25th)%>%
                 arrange(desc(iqr))%>%
                 top_n(10)
Recent_grads1$Major <- factor(Recent_grads1$Major,levels = Recent_grads1[order(Recent_grads1$iqr,decreasing=F),]$Major)

# Plot --------------------------------------------------------------------
 ggplot(Recent_grads1,aes(x = P25th,xend = P75th,y = Major ))+
        geom_dumbbell(colour_xend = 'brown1',size_x = 2,size_xend = 4)+
        labs( x = 'US $',y = '',
              title = 'Top 10 College Majors with the highest interquartile salary range',
              caption = 'Source: acs/FiveThirtyEight\n@kigtembu')+
              theme_bw()

ggsave('week29.png')

Recent_grads1$Major <- factor(Recent_grads1$Major,levels = Recent_grads1[order(Recent_grads1$Sample_size,decreasing=F),]$Major)

ggplot(Recent_grads1,aes(x = Major, y = Sample_size))+
  geom_bar(stat = 'identity',position = position_stack(reverse = TRUE),fill = 'aquamarine')+
  geom_text(aes(label = Sample_size, hjust = 0))+
  coord_flip()+
  labs(x = 'College Major',y = 'Sample Size(salary)',
       title = 'Sample Size for Median Salary',
       caption = 'Source: acs/FiveThirtyEight\n @kigtembu')+
  theme_bw()

ggsave('week29_samp.png')