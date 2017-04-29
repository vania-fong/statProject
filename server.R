library(ggplot2)
library(shiny)
apartments <- read.csv("~/Desktop/SpringStat133/Stat133Project/data/apt_cleaned.csv")
houses <- read.csv("~/Desktop/SpringStat133/Stat133Project/data/homes.csv")
function(input, output) {
  
  dataset <- reactive(houses)
  
  output$plot1 <- renderPlot({
    p <- ggplot(dataset(), aes_string(x=input$x1, y=input$y1)) + geom_point(alpha=.5) 
    
    facets <- paste(input$facet_row1, '~', input$facet_col1)
    if (facets != '. ~ .')
      p <- p + facet_grid(facets)
    
    if(input$regression == "Linear")
      p <- p + geom_smooth(method=lm)
    
    if(input$regression == "Loess")
      p <- p + geom_smooth(method=loess)
    
    print(p)
  })
}
