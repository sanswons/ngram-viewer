library(shiny)
source('utils.R')

ui <- fluidPage(
    titlePanel("N-gram Viewer"),
    textInput('query',"Graph these comma-separated phrases:", value = 'food, data'),
    actionButton("search", "Search",style = 'background-color: orange, color: white'),
    plotOutput('plot')
  )
