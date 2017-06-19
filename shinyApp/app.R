library(shiny)
library(DT)

##UI code
ui = fluidPage(
    wellPanel( 
      fluidRow(
          column(4, textInput(inputId = "pos", label = "Chromosome Position", placeholder = "X:233541 or X:233541-489810")),
          column(3, offset = 1, textInput(inputId = "gene", label = "Gene Name", placeholder = "BRCA2")),
          column(3, offset = 1, selectizeInput(inputId = "sample", label = "Sample ID", choices = NULL))
      ),
      fluidRow(
        column(2, offset = 0, actionButton(inputId = "but_pos", label = "Search")),
        column(2, offset = 3, actionButton(inputId = "but_gene", label = "Search")),
        column(2, offset = 2, actionButton(inputId = "but_sample", label = "Search"))
      )
    ),
    mainPanel(
      tabsetPanel(id = "main_panel",
        tabPanel("Position", value = "pos_panel",
                 h4(textOutput(outputId = "id_cord"),
                 br(),
                 dataTableOutput(outputId = "table_cord"))
                 ),
        tabPanel("Gene", value = "gene_panel",
                 h4(textOutput(outputId = "id_gene"),
                 h4(""),
                 dataTableOutput(outputId = "table_gene"))
                 ),
        tabPanel("Sample", value = "sample_panel",
                 h4(textOutput(outputId = "id_sample"),
                 br(),
                 fluidRow(column(width = 1, strong("Sex")),
                          column(width = 3, strong("Phenotype")),
                          column(width = 5, strong("Age at diagnonsis")),
                          column(width = 3, strong("Ethnicity (PCA)"))
                 ),
                 br(),
                 fluidRow(column(width = 1, textOutput(outputId = "sample_sex")),
                          column(width = 3, textOutput(outputId = "sample_pheno")),
                          column(width = 5, textOutput(outputId = "sample_age")),
                          column(width = 3, textOutput(outputId = "sample_ethnic"))
                 ),
                 br(),
                 h4(textOutput(outputId = "sampleid_table")),
                 dataTableOutput(outputId = "table_sample"))
                 )
      )
    )
  )
###server functions

variant_data <- read.table(file = "../test_variant_data.txt", header = TRUE, sep = "\t")
sample_data <- read.table(file = "../test_sample_data.txt", header = TRUE, sep = "\t")

##Server code
server = function(input, output, session){
####panel switching 
  observeEvent(input$but_pos, {updateTabsetPanel(session, "main_panel", selected = "pos_panel")})
  observeEvent(input$but_gene, {updateTabsetPanel(session, "main_panel", selected = "gene_panel")})
  observeEvent(input$but_sample, {updateTabsetPanel(session, "main_panel", selected = "sample_panel")})
####COORDINATE SEARCH
  ##Evaluate if the coordinate provided looks roughly like a genomic coordinate
  val_cord <- eventReactive(input$but_pos, {
    validate(
      need(grepl(pattern = "([XY]|[1-9]|1[0-9]|2[0-2]):[0-9]+(-[0-9]+)?", input$pos),
           message = "Please enter a valid chromosome position") 
          ## message passed if it looks weird or malformed e.g. contains letters
    )
    ##passes input value if no problem
    output$id_cord <- renderText({paste("Search results for ", input$pos)}) ##prints header for table with search value
    print(input$pos)
  })    
  data_cord <- reactive({
    ##split the inputpos into its chr start and stop components
    chr <- strsplit(val_cord(), ":",fixed = TRUE)[[1]][1]
    start1 <- strsplit(strsplit(val_cord(), ":",fixed = TRUE)[[1]][2], "-", fixed = TRUE)[[1]][1]
    stop1 <- strsplit(strsplit(val_cord(), ":",fixed = TRUE)[[1]][2], "-", fixed = TRUE)[[1]][2]
    ##if only start was provided - look for specific coord - if both start and stop - look up range of variants - start should be less than stop
    if(is.na(stop1)){
      datC <- variant_data[variant_data$Chr == chr & variant_data$Pos == start1,]
      ##report data back to function
      datC <- datC[-(16:25)]
    } else {
      datC <- variant_data[variant_data$Chr == chr 
            & as.numeric(variant_data$Pos) >= as.numeric(start1)
            & as.numeric(variant_data$Pos) <= as.numeric(stop1),]
      ##report data back to function
      datC <- datC[1:15]
    }
  })
####GENE SEARCH
  ##validates the input as a potential gene symbol - no weird punctuation etc
  val_gene <- eventReactive(input$but_gene, {
    
    validate(
      need(grepl(pattern = "^[A-Z0-9-]+$", input$gene),
           message = "Please enter a valid gene symbol - e.g. BRCA1 (Capitalisation is required)") 
      ## message passed if it looks weird or malformed 
    )
    ##passes input value if no problem
    output$id_gene <- renderText({paste("Search results for ", val_gene())}) ##prints header for table with search value
    print(input$gene)
  })
  ##function receives value from val_gene
  data_gene <- reactive({
    ##filter table for input gene symbol
    datG <- variant_data[grep(val_gene(),variant_data$GENE),]
    datG <- datG[1:15] #remove excess columns
    })
####SAMPLE LOOKUP
  ##Generates list of available samples to look through on search field
  updateSelectizeInput(session, 'sample', choices = sample_data$Id, server = TRUE)
  ##makes search event reactive and stores sample name as variable
  val_sample <- eventReactive(input$but_sample, {
    print(input$sample)
  })
  output$sample_sex <- renderText({paste(sample_data$SEX[sample_data$Id == val_sample()])})
  output$sample_pheno <- renderText({paste(sample_data$PHENO[sample_data$Id == val_sample()])})
  output$sample_age <- renderText({paste(sample_data$AGE[sample_data$Id == val_sample()])})
  output$sample_ethnic <- renderText({paste(sample_data$ETHNIC[sample_data$Id == val_sample()])})
  data_sample <- reactive({
    ##filter table for input gene symbol
    datS <- variant_data[variant_data[,s] == 1 | variant_data[,s] == 2 ,]
    datS <- datS[1:15] #remove excess columns
  })
  
####DATA OUTPUT SECTION
##output variant table found and the search term
output$table_cord <- renderDataTable({datatable(data_cord(),rownames = FALSE, options = list(
                                                                                pageLength = 10,
                                                                                lengthMenu = c(10, 25, 50),
                                                                                searchHighlight = TRUE
                                                                              ))})
output$table_gene <- renderDataTable({datatable(data_gene(),rownames = FALSE, options = list(
                                                                                pageLength = 10,
                                                                                lengthMenu = c(10, 25, 50),
                                                                                searchHighlight = TRUE
                                                                              ))})
output$table_sample <- renderDataTable({datatable(data_sample(),rownames = FALSE, options = list(
                                                                                pageLength = 10,
                                                                                lengthMenu = c(10, 25, 50),
                                                                                searchHighlight = TRUE
))})

output$id_sample <- renderText({paste(val_sample())}) ##prints header for table with search value
output$sampleid_table <- renderText({paste("Non-reference variants in ", val_sample())})

}

##app call
shinyApp(ui = ui, server = server)