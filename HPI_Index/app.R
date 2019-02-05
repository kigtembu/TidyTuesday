##---------------##
## Tidy Tuesday  ##
## Week 5 2019   ##
## 1/29/2019     ##

library(shiny)
library(shinydashboard)
library(tidyverse)

ui <- dashboardPage(
  dashboardHeader(title = 'Housing Price Index Dashboard'),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      box(title = 'HPI Index Per Year Per State',
          solidHeader = TRUE,
          collapsible = TRUE,
          background = 'teal',
          plotOutput('plot1')),
      
      box(
        title = 'Select Year and State:',
        solidHeader = TRUE,
        collapsible = TRUE,
        background = 'olive',
        selectInput('year_selector','Year:',c(1975:2018)),
        
        selectInput('state_selector','State:',c(state.abb))
      )
    )
  )
)

state_hpi <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-05/state_hpi.csv')
state_hpi_cleaned <- state_hpi %>% mutate(month_cleaned = case_when(
  month == 1 ~ 'Jan',
  month == 2 ~ 'Feb',
  month == 3 ~ 'Mar',
  month == 4 ~ 'Apr',
  month == 5 ~ 'May',
  month == 6 ~ 'Jun',
  month == 7 ~ 'Jul',
  month == 8 ~ 'Aug',
  month == 9 ~ 'Sep',
  month == 10 ~ 'Oct',
  month == 11 ~'Nov',
  month == 12 ~ 'Dec'
)) %>% mutate(month_cleaned = fct_reorder(month_cleaned,month))
server <- function(input,output){
  
  
  output$plot1 <- renderPlot({
    state_hpi_cleaned %>% filter(year == input$year_selector,state == input$state_selector ) %>% 
      ggplot(mapping = aes(x = month_cleaned,y = price_index,group = 1))+geom_point() + geom_line() +theme_light()+
      labs(x = 'Month', y = 'HPI Index', title = '',
           caption = 'Source:Freddie Mac\n Plot:@kigtembu')
  })
}

shinyApp(ui,server)
