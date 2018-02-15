library(shiny)
library(DT)
library(reshape2)
library(markdown)
library(shinythemes)
library(ggplot2)
library(shinycssloaders)
library(shinyWidgets)
library(formattable)
library(shinyjs)


### Advanced tips https://github.com/daattali/advanced-shiny#readme
### Links in table https://github.com/rstudio/DT/issues/58

##UI code
ui = fluidPage(theme = shinytheme("flatly"),
      useShinyjs(),
      tags$style(type="text/css", "body {padding-top: 80px;}"),
      tags$style(type='text/css', "#but_pos  { vertical-align: middle; }"),
      navbarPage(title = "MedGen Variant Browser", collapsible = TRUE,position = "fixed-top",
        ###Data Summary panel
        tabPanel(title = "Data summary", icon = icon("table"),
        h4("Data summary"),
          wellPanel(style = "padding: 10px;",
            h5(tags$em("Variant fitlering options"))
          )
        ),
        ##Browser panel
        tabPanel(title = "Browser", icon = icon("flask"),
         h4(tags$b("Variant Browser")),
         fluidRow(
           column(11,selectInput(inputId = "cohort",label = "Cohort",choices = c(cohorts,"None"),selected = "None"))
         ),
         div(id = "animation",
         conditionalPanel("input.cohort != 'None'",
          column(width = 2,
            h4("Data options"),
              tabsetPanel(id = "data_select",type = "pills",
                  tabPanel(title = "Position",value = "data_select_pos",
                   wellPanel(
                    fluidRow(
                      column(12, textInput(inputId = "pos", label = "Position(s)",placeholder = "X:2335 or X:2335-4898"))
                      #column(4, actionButton(inputId = "but_pos", label = "Search"))
                      ),
                    h5(tags$em("Fitlering options")),
                    fluidRow(
                      column(12,materialSwitch(inputId = "syn_cord_pos",label = "Include synonymous",value = FALSE,status = "success",right = TRUE)
                        )),
                    fluidRow(
                      column(12,materialSwitch(inputId = "trunc_cord_pos",label = "Truncating only",value = FALSE,status = "success",right = TRUE)
                        )),
                    fluidRow(
                      column(12,materialSwitch(inputId = "splice_cord_pos",label = "Splicing only",value = FALSE,status = "success",right = TRUE)
                        )),
                  ###### ACTIONS NOT IMPLEMENTED
                    fluidRow(
                      column(12,sliderInput(inputId = "Exac_rarity_pos",
                                      label = h6(tags$em("ExAC")),
                                                           min = 0,
                                                           max = 0.2,
                                                           value = c(0.05),
                                                           step = 0.005,
                                                           ticks = FALSE,
                                                           width = '100%'))),
                    fluidRow(
                      column(12,sliderInput(inputId = "G1K_rarity_pos",
                                                           label = h6(tags$em("1K WGS")),
                                                           min = 0,
                                                           max = 0.2,
                                                           value = c(0.05),
                                                           step = 0.005,
                                                           ticks = FALSE,
                                                           width = '100%'))),
                    fluidRow(
                      column(width = 12, actionButton(inputId = "but_pos", label = "Search",icon = icon("search"),width = "100%"))
                      )
                    )
                  ),
######################
                  tabPanel(title = "Gene",value = "data_select_gene",
                   wellPanel(
                    fluidRow(
                      column(12,selectizeInput(inputId = "gene", label = "Gene(s)", multiple = TRUE, choices = NULL))
                      #column(6, offset = 3, actionButton(inputId = "but_gene", label = "Search"))
                            ),
                    h5(tags$em("Fitlering options")),
                    fluidRow(
                      column(12,materialSwitch(inputId = "syn_cord_gene",label = "Include synonymous",value = FALSE,status = "success",right = TRUE)
                             )),
                    fluidRow(
                      column(12,materialSwitch(inputId = "trunc_cord_gene",label = "Truncating only",value = FALSE,status = "success",right = TRUE)
                             )),
                    fluidRow(
                      column(12,materialSwitch(inputId = "splice_cord_gene",label = "Splicing only",value = FALSE,status = "success",right = TRUE)
                             )),
                                 ###### ACTIONS NOT IMPLEMENTED
                    fluidRow(
                      column(12,sliderInput(inputId = "Exac_rarity_gene",
                                                         label = h6(tags$em("ExAC")),
                                                         min = 0,
                                                         max = 0.2,
                                                         value = c(0.05),
                                                         step = 0.005,
                                                         ticks = FALSE,
                                                         width = '100%'))),
                    fluidRow(
                      column(12,sliderInput(inputId = "G1K_rarity_gene",
                                                         label = h6(tags$em("1K WGS")),
                                                         min = 0,
                                                         max = 0.2,
                                                         value = c(0.05),
                                                         step = 0.005,
                                                         ticks = FALSE,
                                                         width = '100%'))),
                    fluidRow(
                      column(width = 12, actionButton(inputId = "but_gene", label = "Search",icon = icon("search"),width = "100%"))
                      )
                    )
                  ),
############################
                  tabPanel(title = "Sample",value = "data_select_sample",
                   wellPanel(
                    fluidRow(
                      column(12,selectizeInput(inputId = "sample", label = "Sample", choices = NULL))
                      #column(6, offset = 2, actionButton(inputId = "but_sample", label = "Search"))
                                 ),
                    h5(tags$em("Fitlering options")),
                    fluidRow(
                      column(12,materialSwitch(inputId = "syn_cord_sample",label = "Include synonymous",value = FALSE,status = "success",right = TRUE)
                            )),
                    fluidRow(
                      column(12,materialSwitch(inputId = "trunc_cord_sample",label = "Truncating only",value = FALSE,status = "success",right = TRUE)
                            )),
                    fluidRow(
                      column(12,materialSwitch(inputId = "splice_cord_sample",label = "Splicing only",value = FALSE,status = "success",right = TRUE)
                            )),
                    ###### ACTIONS NOT IMPLEMENTED
                    fluidRow(
                      column(12,sliderInput(inputId = "Exac_rarity_sample",
                                                         label = h6(tags$em("ExAC")),
                                                         min = 0,
                                                         max = 0.2,
                                                         value = c(0.05),
                                                         step = 0.005,
                                                         ticks = FALSE,
                                                         width = '100%'))),
                    fluidRow(
                      column(12,sliderInput(inputId = "G1K_rarity_sample",
                                                         label = h6(tags$em("1K WGS")),
                                                         min = 0,
                                                         max = 0.2,
                                                         value = c(0.05),
                                                         step = 0.005,
                                                         ticks = FALSE,
                                                         width = '100%'))),
                    fluidRow(
                      column(width = 12, actionButton(inputId = "but_sample", label = "Search",icon = icon("search"),width = "100%"))    
                    )
                  )
                )
              )
            )
           )
          ),
          column(width = 10,
            mainPanel(width = 12,
              tabsetPanel(id = "main_panel",
                tabPanel("Position", value = "pos_panel", 
                    fluidRow(h4(textOutput(outputId = "id_cord"))),
                    fluidRow(
                      column(width = 12,
                      h4(""),
                      withSpinner(dataTableOutput(outputId = "table_cord"),type = 4,color = "#95a5a6")
                            )
                        )
                    # fluidRow(
                    #   column(width = 2, offset = 10, downloadButton(outputId = "data_dl",label = "Save table as csv", icon = icon("floppy-o")))
                    # )
                  ),
                tabPanel("Gene", value = "gene_panel",
                    #h4(textOutput(outputId = "id_gene")),
                    h4(""),
                    withSpinner(dataTableOutput(outputId = "table_gene"),type = 4,color = "#95a5a6")
                    ),
                    h4(""),
                tabPanel("Sample", value = "sample_panel",
                    h4(textOutput(outputId = "id_sample")),
                    wellPanel(
                      fluidRow(
                        column(width = 2, strong("Sex")),
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
                      
                      h4("Sample summary metrics"),
                      fluidRow(
                        column(width = 4, plotOutput(outputId = "sampleP1"))
                              ),
                      h4(""),
                      h4(textOutput(outputId = "sampleid_table")),
                      fluidRow(
                        column(width = 12, withSpinner(dataTableOutput(outputId = "table_sample"),type = 4,color = "#95a5a6"))
                      ),
                      h4("")
                      )
                  )
              )
              # fluidRow(
              #   column(width = 2, offset = 10, downloadButton(outputId = "data_dl",label = "Save table as csv", icon = icon("floppy-o")))
              #   )
            )
        ),
        ###start of other databases tab
        tabPanel(title = "Other Databases", icon = icon("database")),
        ###dropdown navbar section
        navbarMenu(title = "More", icon = icon("cogs"),
              tabPanel(title = "FAQ",
                includeMarkdown(path = "./www/faq.Rmd")),
              tabPanel(title = "About",
                  includeMarkdown(path = "./www/about.Rmd")) ##path to .Rmd file - about 
                  # need to figure out the appropriate pathing - absolute path not ideal
        )
      )
    )


#UI
#############################################################################################################################################
#SERVER

###server functions
### not used in server deployment - connect on app usage
variant_data <- read.table(file = "test_variant_data.txt", header = TRUE, sep = "\t")
sample_data <- read.table(file = "test_sample_data.txt", header = TRUE, sep = "\t")
cohorts <- c("RCC","PCC","1958")

##Server code
server = function(input, output, session){

### cohort selection
observe({
    toggle(id = "animation", anim = TRUE, animType = "fade",
           time = 0.5, condition = input$cohort != "None")
})
hide(id = "main_panel")

####panel switching & hiding
  observeEvent(input$but_pos, {updateTabsetPanel(session, "main_panel", selected = "pos_panel")})
  observeEvent(input$but_gene, {updateTabsetPanel(session, "main_panel", selected = "gene_panel")})
  observeEvent(input$but_sample, {updateTabsetPanel(session, "main_panel", selected = "sample_panel")})
####COORDINATE SEARCH
  #Evaluate if the coordinate provided looks roughly like a genomic coordinate
  val_cord <- eventReactive(input$but_pos, {
    validate(
      need(grepl(pattern = "([XY]|[1-9]|1[0-9]|2[0-2]):[0-9]+(-[0-9]+)?", input$pos),
           message = "Please enter a valid chromosome position")
          ## message passed if it looks weird or malformed e.g. contains letters
    )
    ##passes input value if no problem
    output$id_cord <- renderText({paste("Search results for ", input$pos)}) ##prints header for table with search value
    return(input$pos)
  })
  data_cord <- reactive({
    ##split the inputpos into its chr start and stop components
    chr <- strsplit(val_cord(), ":",fixed = TRUE)[[1]][1]
    start1 <- strsplit(strsplit(val_cord(), ":",fixed = TRUE)[[1]][2], "-", fixed = TRUE)[[1]][1]
    stop1 <- strsplit(strsplit(val_cord(), ":",fixed = TRUE)[[1]][2], "-", fixed = TRUE)[[1]][2]
    ##if only start was provided - look for specific coord - if both start and stop - look up range of variants - start should be less than stop
    if(is.na(stop1)){
      datC <- variant_data[variant_data$Chr == chr & variant_data$Pos == start1,]
      ##report data back to function2:1-9999999999999999
    } else {
      datC <- variant_data[variant_data$Chr == chr 
            & as.numeric(variant_data$Pos) >= as.numeric(start1)
            & as.numeric(variant_data$Pos) <= as.numeric(stop1),]
      
    }
    ##report data back to function
    return(datC)
  })
  ##check box filtering for cord search - add more if lines if more checkboxes used
  data2_cord <- reactive({
     datC2 <- data_cord()
     if(input$syn_cord_pos == FALSE){
       datC2 <- datC2[datC2$CONSEQUENCE != "synonymous",]
     }
     if(input$trunc_cord_pos == TRUE){
       datC2 <- datC2[datC2$CONSEQUENCE == "stop gain" 
                      | datC2$CONSEQUENCE == "frameshift_deletion"
                      | datC2$CONSEQUENCE == "frameshift_insertion"
                      | datC2$CONSEQUENCE == "stop gain",]
     }
     if(input$splice_cord_pos == TRUE){
       datC2 <- datC2[datC2$TYPE == "splicing",]
     }
    ##report table to render function
     datC2 <- datC2[1:15]
     return(datC2)
  })
  
####GENE SEARCH
  ##validates the input as a potential gene symbol - no weird punctuation etc
  updateSelectizeInput(session, 'gene', choices = variant_data$GENE, server = TRUE,options = list(placeholder = 'Gene Symbol'))
  val_gene <- eventReactive(input$but_gene, {
    
    validate(
      need(grepl(pattern = "^[A-Z0-9-]+$", input$gene),
           message = "Please enter a valid gene symbol - e.g. BRCA1 (Capitalisation is required)") 
      ## message passed if it looks weird or malformed 
    )
    ##passes input value if no problem
    return(input$gene)
  })
  #output$id_gene <- renderText({paste("Search results for ", val_gene())}) ##prints header for table with search value
  ##function receives value from val_gene
  data_gene <- reactive({
    ##filter table for input gene symbol
    datG <- variant_data[grep(val_gene(),variant_data$GENE),]
    #remove excess columns
    })
  ##functional filtering based on check boxes
  data2_gene <- reactive({
    datG2 <- data_gene()
    if(input$syn_cord_gene == FALSE){
      datG2 <- datG2[datG2$CONSEQUENCE != "synonymous",]
    }
    if(input$trunc_cord_gene == TRUE){
      datG2 <- datG2[datG2$CONSEQUENCE == "stop gain" 
                     | datG2$CONSEQUENCE == "frameshift_deletion" 
                     | datG2$CONSEQUENCE == "frameshift_insertion"
                     | datG2$CONSEQUENCE == "stop gain",]
    }
    if(input$splice_cord_gene == TRUE){
      datG2 <- datG2[datG2$TYPE == "splicing",]
    }
    ##report table to render function
    datG2 <- datG2[1:15]
  })
####SAMPLE LOOKUP
  ##Generates list of available samples to look through on search field
  updateSelectizeInput(session, 'sample', choices = sample_data$Id, server = TRUE,options = list(placeholder = 'Sample ID'))
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
  })
  data2_sample <- reactive({
    datS2 <- data_sample()
    if(input$syn_cord_sample == FALSE){
      datS2 <- datS2[datS2$CONSEQUENCE != "synonymous",]
    }
    if(input$trunc_cord_sample == TRUE){
      datS2 <- datS2[datS2$CONSEQUENCE == "stop gain" 
                     | datS2$CONSEQUENCE == "frameshift_deletion" 
                     | datS2$CONSEQUENCE == "frameshift_insertion"
                     | datS2$CONSEQUENCE == "stop gain",]
    }
    if(input$splice_cord_sample == TRUE){
      datS2 <- datS2[datS2$TYPE == "splicing",]
    }
    ##report table to render function
    datS2 <- datS2[1:15] #remove excess columns
  })
  plot1 <- reactive({
    ggplot(data_sample(),aes(x = CONSEQUENCE,fill = CONSEQUENCE))+
      geom_bar() +
      labs(list(title = "Frequency of Consequences", x = "", y = "")) +
      scale_x_discrete(labels=c("frameshift_deletion" = "FS_del", "stop gain" = "stop gain",
                                "frameshift_insertion" = "FS_ins", "nonsynonymous" = "nonsyn", "nonframeshift_insertion" = "nFS_ins",
                                "synonymous" = "syn", "stop loss" = "stop loss", "nonframeshift_deletion" = "nFS_del"))+
      theme(panel.background = element_rect(fill = 'white'), axis.text.x=element_text(size = 10)) +
      theme(panel.border = element_blank(), axis.line = element_line(colour="black"), panel.grid.major = element_line(colour = "gray90")) +
      theme(panel.grid.minor = element_blank(), plot.title = element_text(size = 14,face = "bold",colour = "gray10",margin = margin(0,0,10,0))) +
      theme(axis.title = element_text(size = 16), plot.margin = margin(10,25,0,0), axis.text.y = element_text(size=12), legend.position="none")
  })
  
####DATA OUTPUT SECTION
##output variant table found and the search term
output$table_cord <- renderDataTable({as.datatable(formattable(data2_cord(), list(CADD = color_tile("white", "orange"),
                                                                                  CONSEQUENCE = formatter("span",
                                                                                  style = x ~ ifelse(x == "nonsynonymous", 
                                                                                  style(color = "green", font.weight = "bold"), NA)))
                                                                ),
                                                                class = 'row-border compact table-hover',
                                                                rownames = FALSE, options = list(pageLength = 10,
                                                                lengthMenu = c(10, 25, 50),
                                                                searchHighlight = TRUE))})
# output$table_cord <- renderDataTable({datatable(  data2_cord(),rownames = FALSE, options = list(
#                                                                                 pageLength = 10,
#                                                                                 lengthMenu = c(10, 25, 50),
#                                                                                 searchHighlight = TRUE
#                                                                               ))})
output$table_gene <- renderDataTable({datatable(data2_gene(),rownames = FALSE, options = list(
                                                                                pageLength = 10,
                                                                                lengthMenu = c(10, 25, 50),
                                                                                searchHighlight = TRUE
                                                                              ))})
output$table_sample <- renderDataTable({datatable(data2_sample(),rownames = FALSE, options = list(
                                                                                pageLength = 10,
                                                                                lengthMenu = c(10, 25, 50),
                                                                                searchHighlight = TRUE
                                                                              ))})

###Plot rendering
output$sampleP1 <- renderPlot(plot1())

###Text rendering
output$id_sample <- renderText({paste(val_sample())}) ##prints header for table with search value
output$sampleid_table <- renderText({paste("Non-reference variants in ", val_sample())})

### review save mechanisms
###file download function
# output$data_dl <- downloadHandler(
#   filename = function(){
#     #paste("MedGenVB_data_",Sys.Date(),".csv", sep = "")
#     if (input$main_panel == "pos_panel"){
#     paste("position_data_",Sys.Date(),".csv", sep = "")
#   } else if (input$main_panel == "gene_panel"){
#     paste("gene_data_",Sys.Date(),".csv", sep = "")
#   } else if (input$main_panel == "sample_panel"){
#     paste("sample_data_",Sys.Date(),".csv", sep = "")
#     }
#   },
#   content = function(file) {
#     if (input$main_panel == "pos_panel"){
#       write.csv(x = data2_cord(), file, quote = FALSE, row.names = FALSE)
#     } else if (input$main_panel == "gene_panel"){
#       write.csv(x = data2_gene(), file, quote = FALSE, row.names = FALSE)
#     } else if (input$main_panel == "sample_panel"){
#       write.csv(x = data2_sample(), file, quote = FALSE, row.names = FALSE)
#     }
#   })
##end server lines
}

##app call
shinyApp(ui = ui, server = server)
