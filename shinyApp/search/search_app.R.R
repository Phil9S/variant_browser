library(shiny)
shinyApp(
  ui = fluidPage(
    wellPanel(
      fluidRow(
        column(2),
        column(width = 8, 
             textInput(inputId = "pos", label = "Chromosome Position",placeholder = "X:233541-489810", width = "100%"),
             actionButton(inputId = "but", label = "Search", width = "100%"), align="center"),
        column(2)
    )),
    tags$h4("Search Results"),
    dataTableOutput(outputId = "data")
  ),
  
  server = function(input, output){
     d <-eventReactive(input$but, {
      pos <- as.character(input$pos)
      print(pos)
      })
  output$data <- renderDataTable({data.frame(x = rep("string",20, y = rep(d(), 20)))})
  }
)