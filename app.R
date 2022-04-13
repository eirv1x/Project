##opening necessary libraries
library(shiny)
library(tidyverse)
library(readxl)
library(here)
library(ggrepel)
library(ggplot2)

pwbestlocalauth <- read.csv(here("data", "pwbest_localauth_filtered.csv"))
View(pwbestlocalauth)
head(pwbestlocalauth)
pwbestlocalauth <- pwbestlocalauth %>% #renamed columns in dataframe
  rename(Geography = Title, pwb = X, '2016-17' = X.2, '2017-18' = X.6, '2018-19' = X.10, '2019-20' = X.14, '2020-21' = X.18)
pwbestlocalauth <- pwbestlocalauth %>% #selected columns of focus in dataframe
  select(Geography, pwb, '2016-17', '2017-18','2018-19', '2019-20', '2020-21')
pwblaclean <- pwbestlocalauth[-c(1,2),] #removed columns to clean dataframe
View(pwblaclean)

#creating a shiny app to display personal wellbeing scores for each geographical area in the UK for each year  
ui <- shinyUI(fluidPage(
  titlePanel("Personal Wellbeing Scores across the UK"),
  sidebarLayout(
    sidebarPanel(
      radioButtons(inputId = "year", #created a button panel for the user to select the year they wish to view
                   label = "Please select a year:",
                   choices=c("2016-17" = "2016-17", "2017-18" = "2017-18","2018-19" = "2018-19", "2019-20" = "2019-20","2020-21" = "2020-21"),
                   selected = "2016-17" #automatically set on the year 2016-2017
      )),
    
    mainPanel(
      plotOutput("scatterplot") #output should be a scatter plot
    )
  )
))

#now to instruct the server what to display 
server <- shinyServer(function(input,output){
  output$scatterplot <- renderPlot({ #ggplot used to create a scatterplot of the data depending on year picked by user
   ggplot(data=pwblaclean, mapping = aes(x= pwb, y=input$year, color = Geography)) + geom_point() + geom_smooth(alpha=0.2, method = 'lm') + labs(x = "Personal Wellbeing Measures",
                                                                                                                                                 y = "Scores", title = "Personal Wellbeing Scores") + geom_jitter() + geom_text_repel(aes(label=Geography))
  })
})

shinyApp(ui=ui,server=server)
