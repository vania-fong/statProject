library(ggplot2)
library(shiny)

#Read in data set 
data <- read.csv("~/Desktop/SpringStat133/Stat133Project/data/homes_apts.csv")

#Function to generate Shiny Server
#Create a reactive data set for the server to recognize and use
#Create a plot object to display in the interface
#generates scatterplot with variable selection through dropdown menus
#facet by rows and columns based on input from a dropdown widget
#jitter scatterplot given an input from a checkbox widget
#generate and plot either a linear or locally estimated slope model to fit the data
#select a variable to color the points in the scatterplot
#select a variable to shape the points in the scatterplot 
#select the size of the points in the scatterplot
function(input, output) {
  dataset <- reactive(data)
  output$plot1 <- renderPlot({
    
    p <- ggplot(dataset(), aes_string(x=input$x1, y=input$y1)) + geom_point() 
    facets <- paste(input$facet_row1, '~', input$facet_col1)
    if (facets != '. ~ .')
      p <- p + facet_grid(facets)
    if (input$jitter)
      p <- p + geom_jitter()
    if(input$regression == "Linear")
      p <- p + geom_smooth(method=lm)
    if(input$regression == "Loess")
      p <- p + geom_smooth(method=loess)
    if (input$color != 'None')
      p <- p + aes_string(color=input$color)
    if (input$shape != 'None')
      p <- p + aes_string(shape=input$shape)
    if(input$size)
      p <- p + geom_point(size=input$size)
    
    print(p)
  })
}
