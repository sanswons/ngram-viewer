library(shiny)

server = function(input, output){
  data = reactive(create_plots(input$query))
  output$plot = renderPlot({
    input$search
    isolate(data())
  })
}


