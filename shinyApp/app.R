library(shiny)
library(DT)
library(markdown)
library(shinythemes)
library(ggplot2)

##UI code
ui = fluidPage(theme = shinytheme("flatly"),
      navbarPage(title = "MedGen Variant Browser", collapsible = TRUE,
        tabPanel(title = "Browser", icon = icon("flask"),  
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
          mainPanel(width = 12,
            tabsetPanel(id = "main_panel",
              tabPanel("Position", value = "pos_panel",
                      h4(textOutput(outputId = "id_cord"),
                      h4(""),
                      dataTableOutput(outputId = "table_cord"))
                      ),
              tabPanel("Gene", value = "gene_panel",
                      h4(textOutput(outputId = "id_gene"),
                      h4(""),
                      dataTableOutput(outputId = "table_gene"))
                      ),
              tabPanel("Sample", value = "sample_panel",
                      h4(textOutput(outputId = "id_sample")),
                      wellPanel(
                      fluidRow(column(width = 2, strong("Sex")),
                                column(width = 2, strong("Phenotype")),
                                column(width = 2, strong("Age of onset")),
                                column(width = 3, strong("Ethnicity (PCA)"))
                              ),
                      br(),
                      fluidRow(
                                column(width = 2, textOutput(outputId = "sample_sex")),
                                column(width = 2, textOutput(outputId = "sample_pheno")),
                                column(width = 2, textOutput(outputId = "sample_age")),
                                column(width = 3, textOutput(outputId = "sample_ethnic"))
                              )
                      ),
                      h4(textOutput(outputId = "sampleid_table")),
                      fluidRow(
                                column(width = 12, dataTableOutput(outputId = "table_sample"))
                              ),
                      h4("Sample summary metrics"),
                      fluidRow(
                                column(width = 4, plotOutput(outputId = "sampleP1"))
                              ),
                      br(),
                      h4("some text to space things", style="align:center;")
                      )
            )
          )
        ),
        tabPanel(title = "Other Databases", icon = icon("database")),
            navbarMenu(title = "More", icon = icon("cogs"),
              tabPanel(title = "FAQ"),
              tabPanel(title = "About",
                  includeMarkdown(path = "/Philip/Bioinformatics Workflows and Reports/markdown_site/shinyApp/www/about.Rmd")) ##path to .Rmd file - about 
                  # need to figure out the appropriate pathing - absolute path not ideal
        )
      )
    )


#UI
#############################################################################################################################################
#SERVER

###server functions
### not used in server deployment - connect on app usage
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
  updateSelectizeInput(session, 'sample', choices = sample_data$Id, server = TRUE,options = list(placeholder = 'Please enter a sample ID'))
  ##makes search event reactive and stores sample name as variable
  val_sample <- eventReactive(input$but_sample, {
    req(input$sample)
    print(input$sample)
  })
  output$sample_sex <- renderText({paste(sample_data$SEX[sample_data$Id == val_sample()])})
  output$sample_pheno <- renderText({paste(sample_data$PHENO[sample_data$Id == val_sample()])})
  output$sample_age <- renderText({paste(sample_data$AGE[sample_data$Id == val_sample()])})
  output$sample_ethnic <- renderText({paste(sample_data$ETHNIC[sample_data$Id == val_sample()])})
  data_sample <- reactive({
    ##filter table for input gene symbol
    datS <- variant_data[variant_data[,val_sample()] == 1 | variant_data[,val_sample()] == 2 ,]
    datS <- datS[1:15] #remove excess columns
  })
  plot1 <- reactive({
    ggplot(data_sample(),aes(x = CONSEQUENCE,fill = CONSEQUENCE))+
      geom_bar() +
      labs(list(title = "Frequency of Consequences", x = "", y = "")) +
      theme(panel.border = element_blank(), axis.line = element_line(colour="black"), panel.grid.major = element_line(colour = "gray90")) +
      theme(panel.grid.minor = element_blank(), plot.title = element_text(size = 14,face = "bold",colour = "gray10",margin = margin(0,0,10,0))) +
      theme(axis.title = element_text(size = 16), plot.margin = margin(10,25,0,0), axis.text.y = element_text(size=12), legend.position="none")
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
output$sampleP1 <- renderPlot(plot1())

output$id_sample <- renderText({paste(val_sample())}) ##prints header for table with search value
output$sampleid_table <- renderText({paste("Non-reference variants in ", val_sample())})

}

##app call
shinyApp(ui = ui, server = server)