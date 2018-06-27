### Tidyverse tuesday challenge
### Week 13 26/06/2018
### created by Kigen tembu

library(RCurl)
library(ggplot2)
library(tidyverse)
library(gridExtra)
library(gtable)
library(grid)


alcohol_df <-read.csv(text=getURL("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/week13_alcohol_global.csv"), 
                      header=T)
#beer table 
beer_df <- arrange(alcohol_df,desc(beer_servings))  %>% 
  select(country,beer_servings)%>% slice(1:10)

#spirit table
spirit_df <- arrange(alcohol_df,desc(spirit_servings))  %>% 
  select(country,spirit_servings)%>% slice(1:10)

#wine table
wine_df <- arrange(alcohol_df,desc(wine_servings))  %>% 
  select(country,wine_servings)%>% slice(1:10)


#themes

tt5 <- ttheme_default(
  core =list(bg_params =list(fill = c('grey95')),
             fg_params = list(fontface = 3)),
  colhead = list(fg_params = list(col = 'black', fontface = 4),
                 bg_params = list(fill = NA)),
  rowhead = list(fg_params = list(col = 'black', fontface = 3))
)
#tables

w <- tableGrob(wine_df,cols = c('','WINE'),rows = c('','','','','','','','','',''), theme = tt5)
w_1<- gtable_add_grob(w,
                      grobs = segmentsGrob( # draw line
                        x0 = unit(0,"npc"),
                        y0 = unit(0,"npc"),
                        x1 = unit(1,"npc"),
                        y1 = unit(0,"npc"),
                        gp = gpar(lwd = 4.0)),
                      t = 1,b = 1, l = 2, r =3
)

b <- tableGrob(beer_df,cols = c('','BEER'),  theme = tt5)
b_1<- gtable_add_grob(b,
                      grobs = segmentsGrob( # draw line
                        x0 = unit(0,"npc"),
                        y0 = unit(0,"npc"),
                        x1 = unit(1,"npc"),
                        y1 = unit(0,"npc"),
                        gp = gpar(lwd = 4.0)),
                      t = 1,b = 1, l = 2, r =3
)

s <- tableGrob(spirit_df,cols = c('','SPIRIT'),rows =  c('','','','','','','','','',''), theme = tt5)
s_1<- gtable_add_grob(s,
                      grobs = segmentsGrob( # draw line
                        x0 = unit(0,"npc"),
                        y0 = unit(0,"npc"),
                        x1 = unit(1,"npc"),
                        y1 = unit(0,"npc"),
                        gp = gpar(lwd = 4.0)),
                      t = 1,b = 1, l = 2, r =3
)

#align tables
h_aligned <- gtable_combine(b_1,s_1,w_1,along = 1)
grid.newpage()

grid.arrange(h_aligned,nrow = 1,ncol=1,
             top = textGrob('Top 10 countries by servings consumed per person, 2010',
                            gp = gpar(fontface = "bold")
                             ),
             bottom = textGrob(
               'Source: World Health Organisation',
               gp = gpar(fontface = 3, fontsize = 9),
               just = 'right'#,  x= 2, y = 5)
      
             
             
            ))






