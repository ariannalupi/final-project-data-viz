library(shiny)
library(tidyverse)
library(caret)
library(scales)
library(ggthemes)
library(plotly)


ui <- fluidPage(
  # Application title
  titlePanel("Youtube Data 2021-2022"),
  
  
  fluidRow(
    
    column(
      width = 2,
      selectInput(
        inputId = 'linetype',
        label = 'Line Type:',
        choices = c("solid", "dashed", "twodash"),
        selected = "solid")),
    
    
    
    column(
      width = 3,
      textInput(
        inputId = "linecolor",
        label = "Line Color:",
        value = "blue")),
    
    
    column(
      width = 5,
      sliderInput(
        "linesize",
        "Line Size:",
        min = 1,
        max = 20,
        value = 2
      ))),
  
  
  br(), 
  
  
  
  mainPanel(
    plotlyOutput("Plot1")
  ),
  
  br(),
  
  br(),
  
  
  fluidRow(
    
    column(
      width = 3,
      numericInput(
        inputId = 'dotshape',
        label = 'Dot Shape:',
        min = 0,
        max = 25,
        value = 1)),
    
    
    
    column(
      width = 4,
      textInput(
        inputId = "dotcolor",
        label = "Dot Color",
        value = "red")),
    
    
    column(
      width = 5,
      sliderInput(
        "dotsize",
        "Dot Size:",
        min = 1,
        max = 5,
        value = 2
      ))),
  
  mainPanel(
    plotlyOutput("Plot2")),
  mainPanel(
    plotlyOutput("Plot3")),
  mainPanel(
    plotlyOutput("Plot4")
    ),
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$Plot1 <- renderPlotly({
    
    
    plot1 <- ggplot(data = youtube,
                    mapping = aes(x = Views, y = Subscribers.gained)) +
      geom_line(color = input$linecolor,
                size = input$linesize,
                linetype = input$linetype) +
    theme(axis.text.x = element_blank() ,
            axis.ticks.x=element_blank()) +
      labs(title = "Views and Subcribers Gained")
    
    ggplotly(plot1)
    
  })
  
  output$Plot2 <- renderPlotly({
    
    
    
    plot2 <- ggplot(data = youtube, 
                    mapping = aes(x = Views, y = Comments.added)) +
      geom_point(color = input$dotcolor, size = input$dotsize, 
                 shape = input$dotshape) + coord_flip() +
    labs(title = "Views and Comments Added")
    
    
    
    ggplotly(plot2)
    
    
  })
  
  
  output$Plot3 <- renderPlotly({ 
    plot3 <-  SVMConfusionMatrixGGPlot1 + labs(title ="SVM Confusion Matrix")
    
    ggplotly(plot3)
    
    
  })
 
  output$Plot4 <- renderPlotly({ 
    plot4 <-  GGPlotNaiveBayes + labs(title ="Naive Bayes Confusion Matrix")

    
    ggplotly(plot4)
    
    
  }) 
  
}

# Run the application
shinyApp(ui = ui, server = server)





















