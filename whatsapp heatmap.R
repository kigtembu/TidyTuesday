library(rwhatsapp)
library(lubridate)
library(tidyverse)

#The chat export should be a txt file with a naming convention 'WhatsApp Chat with xyz.txt'


xyz <- rwa_read("WhatsApp Chat with xyz.txt") %>% 
  mutate(time = as_datetime(time),
         hr = hour(time),
         day= wday(time,label = TRUE)) %>% 
  filter(!is.na(author)) %>% 
  count(day,hr) %>% 
  ggplot(aes(x = day,y = hr,fill = n))+
  geom_tile()+
  theme_light()+
  theme(
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.title.y = element_text(face = 'bold',size = 13),
    axis.title.x = element_text(face = 'bold',size = 13),
    plot.title = element_text(face = "bold",size = 16)
  )+
  scale_y_continuous(breaks = seq(0,23,by = 1))+
  scale_fill_gradient(low = "yellow", high = "red", na.value = NA,name = "No.of Messages")+
  labs(x = 'Day of the Week',
       y = 'Hour of the Day',
       title = "Heatmap of Chats With xyz")

ggsave('leah xyz.png',xyz)

