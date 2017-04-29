library(ggplot2)
merged <- read.csv("~/Desktop/SpringStat133/Stat133Project/data/homes_apts.csv")
dataset <- merged
shinyUI(
  fluidPage(
    titlePanel("Renting in the Bay Area"),
    plotOutput('plot1'),
    fluidRow(
      column(3, offset = 1,
             selectInput('x1', 'X Variable', names(dataset)),
             selectInput('y1', 'Y Variable', names(dataset), names(dataset)),
             selectInput('color', 'Color', c('None', names(dataset)))),
      column(3,
             selectInput('facet_row1', 'Facet Variable (Rows)',
                         c(None='.', names(dataset[sapply(dataset, is.factor)]))),
             selectInput('facet_col1', 'Facet Variable (Columns)',
                         c(None='.', names(dataset[sapply(dataset, is.factor)]))),
             selectInput('shape', 'Shape', c('None', names(dataset)))),
      column(2,
             br(),
             checkboxInput('jitter', 'Jitter Values')),
      column(2,
             div(class = "option-group",
                 radioButtons("regression", "Model Type",
                              choices = c("Linear", "Loess")))),
      column(4,
             sliderInput("size","Point Size",min=1,max=10,value=1))
    )
    )
  )
