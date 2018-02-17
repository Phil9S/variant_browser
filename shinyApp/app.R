library(devtools)

library(shiny)
library(shinythemes)
library(shinycssloaders)
library(shinyWidgets)
library(shinyjs)
library(shinyTree)
library(shinyFiles)
library(DT)
library(formattable)
library(shinyalert)

library(markdown)
library(reshape2)
library(ggplot2)
library(plotly)


## Advanced tips https://github.com/daattali/advanced-shiny#readme
## Links in table https://github.com/rstudio/DT/issues/58

#### UI START ####

ui = fluidPage(theme = shinytheme("flatly"),
      useShinyjs(),
      useShinyalert(),
      tags$style(type="text/css", "body {padding-top: 80px;}"),
      navbarPage(title = "MedGen Variant Browser", collapsible = TRUE,position = "fixed-top",
#### Data Summary panel ####
        tabPanel(title = "Data summary", icon = icon("table"),
        h4(tags$b("Data summary")),
        fluidRow(
          column(1,h5(tags$b("Cohorts"))),
          column(1,h5(tags$b("Population"))),
          column(1,h5(tags$b("Sex")))
        ),
        fluidRow(
          column(1,withSpinner(plotlyOutput(outputId = "summary_p1"),type = 4,color = "#95a5a6")),
          column(1,withSpinner(plotlyOutput(outputId = "summary_p2"),type = 4,color = "#95a5a6")),
          column(1,withSpinner(plotlyOutput(outputId = "summary_p3"),type = 4,color = "#95a5a6"))
          )
        ),
#### Data selection panel ####
        tabPanel(title = "Browser", icon = icon("flask"),
         h4(tags$b("Variant Browser")),
         fluidRow(
           column(2,selectInput(inputId = "cohort",label = "Cohort",choices = c("RCC","PCC","WECARE","None"),selected = "None"))
         ),
         fluidRow(
           column(2,actionButton(inputId = "reset",label = "",icon = icon("undo"),width = "100%",style='padding:2px;'))
         ),
         tags$hr(),
         div(id = "animation",
         conditionalPanel("input.cohort != 'None'",
          column(width = 2,
            h4("Data options"),
              tabsetPanel(id = "data_select",type = "tabs",
                  tabPanel(title = "Position",value = "data_select_pos",
                   wellPanel(
                    fluidRow(
                      column(12, textInput(inputId = "pos", label = "Position(s)",placeholder = "X:2335 or X:2335-4898"))
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
                    fluidRow(
                      column(12,sliderInput(inputId = "Exac_rarity_pos",
                                      label = h6(tags$em("ExAC")),
                                                           min = 0,
                                                           max = 0.05,
                                                           value = c(0.05),
                                                           step = 0.005,
                                                           ticks = FALSE,
                                                           width = '100%'))),
                    fluidRow(
                      column(12,sliderInput(inputId = "X1000G_rarity_pos",
                                                           label = h6(tags$em("1K WGS")),
                                                           min = 0,
                                                           max = 0.05,
                                                           value = c(0.05),
                                                           step = 0.005,
                                                           ticks = FALSE,
                                                           width = '100%'))),
                    fluidRow(
                      column(width = 12, actionButton(inputId = "but_pos", label = "Search",icon = icon("search"),width = "100%"))
                      )
                    )
                  ),
                  tabPanel(title = "Gene",value = "data_select_gene",
                   wellPanel(
                    fluidRow(
                      column(12,textInput(inputId = "gene", label = "Gene(s)",placeholder = "APC,BRCA1,TTN"))
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
                    fluidRow(
                      column(12,sliderInput(inputId = "Exac_rarity_gene",
                                                         label = h6(tags$em("ExAC")),
                                                         min = 0,
                                                         max = 0.05,
                                                         value = c(0.05),
                                                         step = 0.005,
                                                         ticks = FALSE,
                                                         width = '100%'))),
                    fluidRow(
                      column(12,sliderInput(inputId = "X1000G_rarity_gene",
                                                         label = h6(tags$em("1K WGS")),
                                                         min = 0,
                                                         max = 0.05,
                                                         value = c(0.05),
                                                         step = 0.005,
                                                         ticks = FALSE,
                                                         width = '100%'))),
                    fluidRow(
                      column(width = 12, actionButton(inputId = "but_gene", label = "Search",icon = icon("search"),width = "100%"))
                      )
                    )
                  ),

                  tabPanel(title = "Sample",value = "data_select_sample",
                   wellPanel(
                    fluidRow(
                      column(12,selectizeInput(inputId = "sample", label = "Sample", choices = NULL))
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
                    fluidRow(
                      column(12,sliderInput(inputId = "Exac_rarity_sample",
                                                         label = h6(tags$em("ExAC")),
                                                         min = 0,
                                                         max = 0.05,
                                                         value = c(0.05),
                                                         step = 0.005,
                                                         ticks = FALSE,
                                                         width = '100%'))),
                    fluidRow(
                      column(12,sliderInput(inputId = "X1000G_rarity_sample",
                                                         label = h6(tags$em("1K WGS")),
                                                         min = 0,
                                                         max = 0.05,
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
#### main panel ####         
          column(width = 10,
            mainPanel(width = 12,
              tabsetPanel(id = "main_panel",
                tabPanel(title = "",value = "blank_panel"),
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
                    h4(textOutput(outputId = "id_gene")),
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
#### Other databases tab ####
        tabPanel(title = "Other Databases", icon = icon("database"),
                 h4("MedGen Database repository"),
                 fluidRow(
                   column(2,h5("Select a file:")),
                   column(1,shinyFilesButton('files', label='File select', title='Please select a file',multiple = FALSE))
                   )
        ),
#### Dropdown navbar section ####
        navbarMenu(title = "More", icon = icon("cogs"),
              tabPanel(title = "FAQ",
                includeMarkdown(path = "./www/faq.Rmd")),
              tabPanel(title = "About",
                  includeMarkdown(path = "./www/about.Rmd")) ##path to .Rmd file - about 
        )
      )
    )

######## UI END ########


#### SERVER START ####

## external non-session server functions
variant_data <- read.table(file = "test_variant_data.txt", header = TRUE, sep = "\t")
sample_data <- read.table(file = "test_sample_data.txt", header = TRUE, sep = "\t")
cohorts <- c("RCC","PCC","1958")

## Server code
server = function(input, output, session){
#### cohort selection & resetting ####
observe({
    toggle(id = "animation", anim = TRUE, animType = "fade",
           time = 0.5, condition = input$cohort != "None")
})
  
observe({
    if (input$cohort != "None") {
      shinyjs::disable("cohort")
      shinyjs::enable("reset")
    } else {
      shinyjs::disable("reset")
      shinyjs::enable("cohort")
    }
})

observeEvent(input$reset, {
  shinyjs::reset("cohort")
  updateTabsetPanel(session, "main_panel", selected = "blank_panel")
  
})
hide(id = "main_panel")

#### Panel switching & hiding ####
  observeEvent(input$but_pos, {updateTabsetPanel(session, "main_panel", selected = "pos_panel")})
  observeEvent(input$but_gene, {updateTabsetPanel(session, "main_panel", selected = "gene_panel")})
  observeEvent(input$but_sample, {updateTabsetPanel(session, "main_panel", selected = "sample_panel")})
  
#### COORDINATE SEARCH ####
  #Evaluate if the coordinate provided looks roughly like a genomic coordinate
  val_cord <- eventReactive(input$but_pos, {
    validate(
      need(grepl(pattern = "([XY]|[1-9]|1[0-9]|2[0-2]):[0-9]+(-[0-9]+)?", input$pos),
           message = "Please enter a valid chromosome position")
          ## message passed if it looks weird or malformed e.g. contains letters
    )
    ##passes input value if no problem
    ##prints header for table with search value
    return(input$pos)
  })
  data_cord <- eventReactive(input$but_pos, {
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
    
    datC <- datC[datC$X1000G <= input$X1000G_rarity_pos & datC$EXAC <= input$Exac_rarity_pos,]
    
    if(input$syn_cord_pos == FALSE){
      datC <- datC[datC$CONSEQUENCE != "synonymous",]
    }
    if(input$trunc_cord_pos == TRUE){
      datC <- datC[datC$CONSEQUENCE == "stop gain" 
                     | datC$CONSEQUENCE == "frameshift_deletion"
                     | datC$CONSEQUENCE == "frameshift_insertion"
                     | datC$CONSEQUENCE == "stop gain",]
    }
    if(input$splice_cord_pos == TRUE){
      datC <- datC[datC$TYPE == "splicing",]
    }
    ##report table to render function
    datC <- datC[1:15]
    ##report data back to function
    return(datC)
  })

#### GENE SEARCH ####
  ##validates the input as a potential gene symbol - no weird punctuation etc
  label_gene <- eventReactive(input$but_gene, {
    gene_list <- unique(unlist(strsplit(input$gene,",",fixed = TRUE)))
    gene_list <- toupper(gene_list)
    if(length(gene_list) >= 2){
      gene_label <- paste("Gene search for",as.character(length(gene_list)),"genes",sep = " ")
    } else {
      gene_label <- paste("Gene search for",as.character(gene_list),sep = " ")
    }
  })
  
  val_gene <- eventReactive(input$but_gene, {
      gene_list <- unique(unlist(strsplit(input$gene,",",fixed = TRUE)))
      gene_list <- toupper(gene_list)
      
      if(length(gene_list) >= 2){
        no_list <- gene_list[!gene_list %in% variant_data$GENE]
        if(length(no_list) > 0){
          shinyalert(
            title = "Gene symbols missing:",
            text = print(no_list),
            closeOnEsc = TRUE,
            closeOnClickOutside = TRUE,
            html = FALSE,
            type = "warning",
            showConfirmButton = TRUE,
            showCancelButton = FALSE,
            confirmButtonText = "OK",
            confirmButtonCol = "#AEDEF4",
            timer = 0,
            imageUrl = "",
            animation = TRUE
          )}
        gene_list <- gene_list[gene_list %in% variant_data$GENE]
        datG <- variant_data[variant_data$GENE %in% gene_list,]
      } else {
        datG <- variant_data[grep(gene_list,variant_data$GENE),]
      }
      
      datG <- datG[datG$X1000G <= input$X1000G_rarity_gene & datG$EXAC <= input$Exac_rarity_gene,]
      
      if(input$syn_cord_gene == FALSE){
        datG <- datG[datG$CONSEQUENCE != "synonymous",]
      }
      if(input$trunc_cord_gene == TRUE){
        datG <- datG[datG$CONSEQUENCE == "stop gain"
                       | datG$CONSEQUENCE == "frameshift_deletion"
                       | datG$CONSEQUENCE == "frameshift_insertion"
                       | datG$CONSEQUENCE == "stop gain",]
      }
      if(input$splice_cord_gene == TRUE){
        datG <- datG[datG$TYPE == "splicing",]
      }
      ##report table to render function
      datG <- datG[1:15]
      return(datG)
  })


#### SAMPLE LOOKUP ####
  ##Generates list of available samples to look through on search field
  updateSelectizeInput(session, 'sample', choices = sample_data$Id, server = TRUE,options = list(placeholder = 'Sample ID'))
  ##makes search event reactive and stores sample name as variable
  val_sample <- eventReactive(input$but_sample, {
    req(input$sample)
    return(input$sample)
  })
  output$sample_sex <- renderText({paste(sample_data$SEX[sample_data$Id == val_sample()])})
  output$sample_pheno <- renderText({paste(sample_data$PHENO[sample_data$Id == val_sample()])})
  output$sample_age <- renderText({paste(sample_data$AGE[sample_data$Id == val_sample()])})
  output$sample_ethnic <- renderText({paste(sample_data$ETHNIC[sample_data$Id == val_sample()])})
  
  data_sample <- eventReactive(input$but_sample, {
    ##filter table for input gene symbol
    datS <- variant_data[variant_data[,val_sample()] == 1 | variant_data[,val_sample()] == 2 ,]
    
    datS <- datS[datS$X1000G <= input$X1000G_rarity_sample & datS$EXAC <= input$Exac_rarity_sample,]
    
    if(input$syn_cord_sample == FALSE){
      datS <- datS[datS$CONSEQUENCE != "synonymous",]
    }
    if(input$trunc_cord_sample == TRUE){
      datS <- datS[datS$CONSEQUENCE == "stop gain" 
                     | datS$CONSEQUENCE == "frameshift_deletion" 
                     | datS$CONSEQUENCE == "frameshift_insertion"
                     | datS$CONSEQUENCE == "stop gain",]
    }
    if(input$splice_cord_sample == TRUE){
      datS <- datS[datS$TYPE == "splicing",]
    }
    datS <- datS[1:15] #remove excess columns
    ##report table to render function
    return(datS)
  })

#### DATA OUTPUT SECTION ####
  
## Data summary outputs
  
##Variant table output
output$table_cord <- renderDataTable({as.datatable(formattable(data_cord(), list(CADD = color_tile("white", "orange"),
                                                                                  CONSEQUENCE = formatter("span",
                                                                                  style = x ~ style(color=ifelse(x == "nonsynonymous","orange",
                                                                                                          ifelse(x == "synonymous","green","red")))),
                                                                class = 'row-border compact table-hover')),
                                                                rownames = FALSE,
                                                                options = list(pageLength = 10,
                                                                lengthMenu = c(10, 20, 50, 100),
                                                                searchHighlight = TRUE))})

output$table_gene <- renderDataTable({datatable(val_gene(),rownames = FALSE, options = list(
                                                                                pageLength = 10,
                                                                                lengthMenu = c(10, 20, 50, 100),
                                                                                searchHighlight = TRUE
                                                                              ))})

output$table_sample <- renderDataTable({datatable(data_sample(),rownames = FALSE, options = list(
                                                                                pageLength = 10,
                                                                                lengthMenu = c(10, 20, 50, 100),
                                                                                searchHighlight = TRUE
                                                                              ))})

##### Plot rendering ####
sample_plot1 <- reactive({
  ggplot(data_sample(),aes(x = CONSEQUENCE,fill = CONSEQUENCE))+
    geom_bar() +
    labs(list(title = "Frequency of Consequences", x = "", y = "")) +
    scale_x_discrete(labels=c("frameshift_deletion" = "FS_del", "stop gain" = "stop gain",
                              "frameshift_insertion" = "FS_ins", "nonsynonymous" = "nonsyn", "nonframeshift_insertion" = "nFS_ins",
                              "synonymous" = "syn", "stop loss" = "stop loss", "nonframeshift_deletion" = "nFS_del"),expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    theme(panel.background = element_rect(fill = 'white'), axis.text.x=element_text(size = 10)) +
    theme(panel.border = element_blank(), axis.line = element_line(colour="black"), panel.grid.major = element_line(colour = "gray90")) +
    theme(panel.grid.minor = element_blank(), plot.title = element_text(size = 14,face = "bold",colour = "gray10",margin = margin(0,0,10,0))) +
    theme(axis.title = element_text(size = 16), plot.margin = margin(10,25,0,0), axis.text.y = element_text(size=12), legend.position="none")
})
output$sampleP1 <- renderPlot(sample_plot1())

summary_pheno <- reactive({
  plot_ly(as.data.frame(table(sample$PHENO)), labels = ~Var1, values = ~Freq,
             textposition = 'inside',
             textinfo = 'label',
             insidetextfont = list(color = '#FFFFFF'),
             hoverinfo = 'text',
             text = ~Freq) %>% add_pie(hole = 0.6) %>%
  layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>% config(displayModeBar = F)
})
output$summary_p1 <- renderPlotly(summary_pheno())

summary_ethnic <- reactive({
  plot_ly(as.data.frame(table(sample$ETHNIC)), labels = ~Var1, values = ~Freq,
          textposition = 'inside',
          textinfo = 'label',
          insidetextfont = list(color = '#FFFFFF'),
          hoverinfo = 'text',
          text = ~Freq) %>% add_pie(hole = 0.6) %>%
    layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>% config(displayModeBar = F)
})
output$summary_p2 <- renderPlotly(summary_ethnic())

summary_sex <- reactive({
  plot_ly(as.data.frame(table(sample$SEX)), labels = ~Var1, values = ~Freq,
          textposition = 'inside',
          textinfo = 'label',
          insidetextfont = list(color = '#FFFFFF'),
          hoverinfo = 'text',
          text = ~Freq) %>% add_pie(hole = 0.6) %>%
    layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>% config(displayModeBar = F)
})
output$summary_p3 <- renderPlotly(summary_sex())

#### Text rendering ####
output$id_cord <- renderText({paste("Search results for ", val_cord())})
output$id_sample <- renderText({paste(val_sample())}) ##prints header for table with search value
output$id_gene <- renderText({label_gene()}) 
output$sampleid_table <- renderText({paste("Non-reference variants in ", val_sample())})

#### File Browser render ####
shinyFileChoose(input, 'files', root=c(root='.'), filetypes=c('', 'txt'))

#### Save mechanism ####
## review save mechanisms
##file download function
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

#### Server END ####
}
## App call
shinyApp(ui = ui, server = server)
