library(ggplot2)
apartments <- read.csv("~/Desktop/SpringStat133/Stat133Project/data/apt_cleaned.csv")
houses <- read.csv("~/Desktop/SpringStat133/Stat133Project/data/homes.csv")
dataset1 <- houses
dataset2 <- houses

shinyUI(
  fluidPage(
    titlePanel("Houseprice Variable Analysis"),
    plotOutput('plot1'),
    
    fluidRow(
      column(3, offset = 1,
             selectInput('x1', 'X Variable', names(dataset1)),
             selectInput('y1', 'Y Variable', names(dataset1), names(dataset1))
      ),
      column(4,
             selectInput('facet_row1', 'Facet Variable (Rows)',
                         c(None='.', names(dataset1[sapply(dataset1, is.factor)]))),
             selectInput('facet_col1', 'Facet Variable (Columns)',
                         c(None='.', names(dataset1[sapply(dataset1, is.factor)])))
      ),
      column(width=3,
             div(class = "option-group",
                 radioButtons("regression", "Model Type",
                              choices = c("Linear", "Loess"))))
    ))
)