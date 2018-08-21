#----------------------#
# Week 21 Tidy Tuesday #
# created by Kigen     #
# 21/08/2018           #
#----------------------#

#Packages
library(tidyverse)
library(ggplot2)
library(RCurl)
library(lubridate)

#Read in Data

damage <- read.csv(text = getURL("https://raw.githubusercontent.com/BuzzFeedNews/2018-07-wildfire-trends/master/data/calfire_damage.csv"),
                                  header = T)
calfire <- read.csv(text = getURL("https://raw.githubusercontent.com/BuzzFeedNews/2018-07-wildfire-trends/master/data/calfire_frap.csv"),
                                  header = T)
str(calfire)

calfire1 <- calfire %>% select(objectid,alarm_date,cont_date,gis_acres)%>%
            filter(alarm_date != '',cont_date != '')%>%
            mutate(startdate = as.Date(as.character(alarm_date)),
                   enddate = as.Date(as.character(cont_date)),
                   duration = enddate - startdate, yr_st = year(startdate))%>%
            group_by(yr_st)%>%
            summarise(totalDur = sum(duration),count = n())%>%
            transmute(yr = yr_st,avgdur = round(totalDur/count))

ggplot(calfire1,aes(yr,as.numeric(avgdur)))+
        geom_line(size = 1, color = 'blue')+
        labs(title = 'Duration of Wild Fires', x = 'Year', y = 'Average Duration in Days',
             caption = 'Source:US Forest Service\n@kigtembu')+
        theme_minimal()
