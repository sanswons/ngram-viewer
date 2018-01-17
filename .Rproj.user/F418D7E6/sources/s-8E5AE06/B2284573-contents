library(shiny)
library(shinythemes)
source('utils.R')

shinyApp(
  ui <- fluidPage(theme = shinytheme('flatly'),
    
    titlePanel("N-gram Viewer"),
    hr(),
    
    sidebarLayout(
      sidebarPanel(
             textInput('query',"Graph these comma-separated phrases:", value = 'food, data'),
             actionButton("search", "Search"),
             hr(),
             uiOutput('opts'),
             actionButton('update','Add Synonym'),
             width = 3
             
      ),
      
      mainPanel(
        
        plotOutput('plot'),
        width = 9

      )
    )
    
    
  ),
  
  server = function(input, output,session){
   
      data = reactive(create_plots(input$query))
      output$opts = renderUI({
        suggestions = find_synonyms(input$query)
        selectizeInput('sugg',paste(c('Synonyms for "',extract_last_phrase(input$query),' " :'),collapse = ''),suggestions)
      })
      
      observeEvent(input$update, {
        updateTextInput(session, 'query', value = paste(c(input$query, input$sugg),collapse = ', '))
      }) 
     
      output$plot = renderPlot({
        input$search
        isolate(data())
      })
  }
  
)

