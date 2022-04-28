ui <- shinyUI(fluidPage(
  titlePanel("Personal Wellbeing Scores across the UK"),
  sidebarLayout(
    sidebarPanel(
      radioButtons(inputId = "year", #created a button panel for the user to select the year they wish to view
                   label = "Please select a year:",
                   choices=c("2016-17" = "2016-17", "2017-18" = "2017-18","2018-19" = "2018-19", "2019-20" = "2019-20","2020-21" = "2020-21"),
                   
                   selected = "2016-17" #automatically set on the year 2016-2017
      ),
      radioButtons(inputId="filetype", label="Select the file type to download", choices = list("png","pdf")),#buttonpanel to choose file type to download plot
      downloadButton(outputId = "down", label = "Download plot")),
    
    mainPanel(
      plotOutput("scatterplot"),
      textOutput("text"),
      textOutput("text2"),
      textOutput("text3")
    )
  )
))


#instructions for server of what to display
server <- shinyServer(function(input,output){
  output$scatterplot <- renderPlot({#ggplot used to create a scatterplot of the data depending on year picked by user
    ggplot(pwbestlocalauth, aes(x= pwb, y=get(input$year), color = Geography)) + geom_point(size=3, position=position_jitter(h=.15,w=.12)) +  #jitter was used as points overlapped
      geom_smooth(alpha=0.3, method = 'lm') + 
      #customisation of plot for better visualisation
      labs(x = "Personal Wellbeing Measures", y = "Scores", title = "Personal Wellbeing Scores") + scale_colour_manual(values=cbPalette) + theme_light() + theme(text=element_text(color="black", size=11), legend.position = c("top"), legend.direction = "horizontal", axis.title.x=element_text(vjust=-1.5),
                                                                                                                                                                 legend.justification = 0.5, legend.text=element_text(size=11,color="black"), axis.line=element_line(color="black", size=0.2), axis.line.y=element_blank(),panel.grid.major=element_line(color="gray50", size=0.5), panel.grid.major.x = element_blank())
  })
  
  #text used to display a key for the data
  output$text <- renderText({"Personal WellBeing (PWB) Scores Key:"})
  output$text2 <- renderText({"Low Anxiety=High PWB"})
  output$text3 <- renderText({"Low Happiness,Low Satisfaction, Low Worthwhile = Low PWB"})
  
  output$down <- downloadHandler( #code to enable download of specific plot
    filename = function(){
      paste("pwb",input$year,input$filetype,sep='.') #code to name file being downloaded 
    },
    content = function(file){
      if(input$filetype == "png")
        png(file)
      else
        pdf(file) 
      plot(ggplot(pwbestlocalauth, aes(x= pwb, y=get(input$year), color = Geography)) + geom_point(size=3, position=position_jitter(h=.15,w=.12)) +  geom_smooth(alpha=0.3, method = 'lm') + labs(x = "Personal Wellbeing Measures", y = "Scores", title = "Personal Wellbeing Scores") + scale_colour_manual(values=cbPalette) + theme_light() + theme(text=element_text(color="black", size=11), legend.position = c("top"), legend.direction = "horizontal", axis.title.x=element_text(vjust=-1.5),
                                                                                                                                                                                                                                                                                                                                                        legend.justification = 0.5, legend.text=element_text(size=11,color="black"), axis.line=element_line(color="black", size=0.2), axis.line.y=element_blank(),panel.grid.major=element_line(color="gray50", size=0.5), panel.grid.major.x = element_blank()))
      dev.off()
    }
  )
  
  
  
})

shinyApp(ui, server)
