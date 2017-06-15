library(shiny)
library(DT)
##UI code
ui = fluidPage(
    wellPanel( 
      fluidRow(
          column(4, textInput(inputId = "pos", label = "Chromosome Position",placeholder = "X:233541-489810")),
          column(3, offset = 1, textInput(inputId = "gene", label = "Gene Name",placeholder = "BRCA2")),
          column(3, offset = 1, textInput(inputId = "sample", label = "Sample ID",placeholder = "BHAM-FH-200434"))
      ),
      fluidRow(
        column(2, offset = 0, actionButton(inputId = "but_pos", "Search")),
        column(2, offset = 3, actionButton(inputId = "but_gene", "Search")),
        column(2, offset = 2, actionButton(inputId = "but_samp", "Search"))
      )
    ),
    mainPanel(
      h4(textOutput(outputId = "data"))
    )
  )
  

###server functions




##Server code
server = function(input, output){
  p <- eventReactive(input$but_pos, {
    #validate input
    validate(
      need(expr = grepl("[0-9xy]{1,2}:[1-9]+[-[1-9]+]{0,1}", x = input$pos, ignore.case = TRUE),
           message = "Please enter a valid genomic coordinate"))
    print(input$pos)
        })
  

output$data <- renderText({paste("Search results for",p())})
}

##app call
shinyApp(ui = ui, server = server)