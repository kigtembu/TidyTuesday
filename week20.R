### TidyTuesday 13/08/2018
## Created by Kigen Tembu
# Week 20

#Libraries
library(RCurl)
library(tidyverse)
library(ggplot2)
library(tidytext)
library(ggplot2)

#Read in data

twt1 <-read.csv("IRAhandle_tweets_1.csv", 
                    header=T)


twt2 <-read.csv("IRAhandle_tweets_2.csv", 
                header=T)

twt3 <-read.csv("IRAhandle_tweets_3.csv", 
                header=T)

twt4 <-read.csv("IRAhandle_tweets_4.csv", 
                header=T)

twt5 <-read.csv("IRAhandle_tweets_5.csv", 
                header=T)

twt6 <-read.csv("IRAhandle_tweets_6.csv", 
                header=T)

twt7 <-read.csv("IRAhandle_tweets_7.csv", 
                header=T)

twt8 <-read.csv("IRAhandle_tweets_8.csv", 
                header=T)

twt9 <-read.csv("IRAhandle_tweets_9.csv", 
                header=T)

#append data

twt <- suppressWarnings(bind_rows(twt1,twt2,twt3,twt4,twt5,twt6,twt7,twt8,twt9))

tidy_twt <- twt %>%
            select(author,content,account_category,followers)%>%
            distinct%>%
            filter(account_category == 'RightTroll' | account_category == 'LeftTroll')%>%
            group_by(account_category,author)%>%
            summarize(max_followers = max(followers,na.rm = TRUE),
                      No_tweets = n())

#plot
ggplot(tidy_twt,aes(No_tweets,max_followers))+
      geom_jitter(aes(color = account_category))+
      geom_smooth()+
      facet_grid(.~account_category)+
      labs(title = 'Is there a relationship between number of tweets and\nnumber of followers among trolls ?',
            x = 'Number of Tweets', y = 'Number of Followers',
            caption = 'Source:FiveThirtyEight\n@kigtembu')+
      theme_light()+
      theme(legend.position = 'none')

ggplot(tidy_twt,aes(account_category,log(No_tweets)))+
      geom_boxplot(aes(fill = account_category,alpha = 0.6))+
      labs(title = 'Box plots of log adjusted number of tweets',
       x = 'Troll Type', y = 'Log(Number of Tweets)',
       caption = 'Source:FiveThirtyEight\n@kigtembu')+
      theme_light()+
      theme(legend.position = 'none')+
      scale_fill_manual(values = c('blue','red'))
