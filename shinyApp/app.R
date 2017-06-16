library(shiny)
library(DT)

##UI code
ui = fluidPage(
    wellPanel( 
      fluidRow(
          column(4, textInput(inputId = "pos", label = "Chromosome Position", placeholder = "X:233541 or X:233541-489810")),
          column(3, offset = 1, textInput(inputId = "gene", label = "Gene Name", placeholder = "BRCA2")),
          column(3, offset = 1, textInput(inputId = "sample", label = "Sample ID", placeholder = "BHAM-FH-200434"))
      ),
      fluidRow(
        column(2, offset = 0, actionButton(inputId = "but_pos", label = "Search")),
        column(2, offset = 3, actionButton(inputId = "but_gene", label = "Search")),
        column(2, offset = 2, actionButton(inputId = "but_samp", label = "Search"))
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Position", 
                 h4(textOutput(outputId = "id_cord"),
                 h4(""),
                 dataTableOutput(outputId = "table_cord"))
                 ),
        tabPanel("Gene"),
        tabPanel("Sample")
      )
    )
  )
###server functions

variant_data <- read.table(file = "../test_variant_data.txt", header = TRUE, sep = "\t")
sample_data <- read.table(file = "../test_sample_data.txt", header = TRUE, sep = "\t")

##Server code
server = function(input, output){
  ##Evaluate if the coordinate provided looks roughly like a genomic coordinate
  val_cord <- reactive({
    validate(
      need(grepl(pattern = "([XY]|[1-9]|1[0-9]|2[0-2]):[0-9]+(-[0-9]+)?", input$pos),
           message = "Please enter a valid chromosome position") 
          ## message passed if it looks weird or malformed e.g. contains letters
    )
    ##passes input value if no problem
    print(input$pos)
  })    
  data_cord <- reactive({
    ##split the inputpos into its chr start and stop components
    chr <- strsplit(val_cord(), ":",fixed = TRUE)[[1]][1]
    start1 <- strsplit(strsplit(val_cord(), ":",fixed = TRUE)[[1]][2], "-", fixed = TRUE)[[1]][1]
    stop1 <- strsplit(strsplit(val_cord(), ":",fixed = TRUE)[[1]][2], "-", fixed = TRUE)[[1]][2]
    ##if only start was provided - look for specific coord - if both start and stop - look up range of variants - start should be less than stop
    if(is.na(stop1)){
      dat <- variant_data[variant_data$Chr == chr & variant_data$Pos == start1,]
      ##report data back to function
      dat <- dat[-(16:25)]
    } else {
      dat <- variant_data[variant_data$Chr == chr 
            & as.numeric(variant_data$Pos) >= as.numeric(start1)
            & as.numeric(variant_data$Pos) <= as.numeric(stop1),]
      ##report data back to function
      dat <- dat[-(16:25)]
    }
  })
##output variant table found and the search term
output$table_cord <- renderDataTable({datatable(data_cord(),rownames = FALSE, options = list(
                                                                                pageLength = 10,
                                                                                lengthMenu = c(10, 25, 50),
                                                                                searchHighlight = TRUE
                                                                              )
    )
  }
)
output$id_cord <- renderText({paste("Search results for ", input$pos)})
}

##app call
shinyApp(ui = ui, server = server)