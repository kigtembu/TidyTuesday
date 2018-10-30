###-----------------------###
## Week 31 Tidytuesday     ##
## Created by Kigen Tembu  ##
## 23/10/2018              ##
#---------------------------#

# packages ----------------------------------------------------------------

library(tidyverse)
library(RCurl)
library(skimr)
library(ggimage)
library(countrycode)
library(ggrepel)
library(scales)



# read in data ------------------------------------------------------------

url <- 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-10-30/r_downloads_year.csv'
cran <- read.csv(text = getURL(url),header = T)

skim(cran)

cran1 <- cran %>% select(country,os,ip_id,size)%>%
         mutate(country = as.character(country),os = as.character(os))%>%
         filter(!is.na(country),!is.na(os))%>%
         group_by(country)%>%
         summarise(avg_size = mean(as.numeric(size),na.rm = TRUE),count = n())%>%
         mutate(code = country, country = countrycode(sourcevar = country, origin = "iso2c", destination = "genc.name"))%>%
         filter(!is.na(country))%>%
         arrange(desc(count))%>%
         top_n(20,wt=count)
        


cran2 <-ggplot(cran1,aes(x = count,y = avg_size,country = code))+
        geom_flag(aes(image = code))+
        scale_x_continuous(labels = comma)+
        scale_y_continuous(labels = comma)+
        labs(title = 'Average Size of Downloads Vs Number of Downloads',
           x = 'Number of Downloads',
           y = 'Average Size of Downloads(bytes)',
           caption = '@kigtembu')+
        theme_grey()

# Save Plot ---------------------------------------------------------------
ggsave('week31.png',plot = cran2)





