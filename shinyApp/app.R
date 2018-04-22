library(devtools)

library(jsonlite)
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(shinyWidgets)
library(shinyjs)
library(DT)
library(shinyalert)
library(htmltools)
library(bsplus)

library(stringr)
library(markdown)

## TO FIX! 

## Fix intronic filtering
## Session hang on empty tables
## gene fields with comma seperated entries
## remove cosmic fields - likely not useful
## colour schemes!!
## site info row names out of order
## add aggregated frequencies to gene page
## Data undefined on donut plots - custom tooltips
## Warning in gene_info$Gene.name == unique(val_gene()$GENE) :longer object length is not a multiple of shorter object length - GENE SEARCH ERROR
## fix gene description printing on single gene page
## HET OR HOM CALL FOR SAMPLE CALLS - Add column?
## add additional rarites for exac and 1kg
## shorten nonframeshifting / frameshifting -> nFS / FS
## support for comma-sv in clinvar/cosmic table on site panel - add drop down?
## Fix all NA issue with AA column by replacing NA with "N/A"
## Fix ".,." characters in annotation columns in data generation

## Links in table https://github.com/rstudio/DT/issues/58

#### External data ####
#variant_data <- read.table(file = "test_variant_data.txt", header = TRUE, sep = "\t")
load("www/2018-04-11-db_cohort_data.RData")
variant_data <- cohort_list
rm(cohort_list)
sample_data <- read.table(file = "www/2018-04-11-db_sample_data.txt", header = TRUE, sep = "\t")
gene_data <- read.table(file = "www/gene_ids_reference.tsv", header = TRUE, sep = "\t",stringsAsFactors = F)
gene_info <- read.table(file = "www/gene_info_formatted.tsv", header = TRUE, sep = "\t",quote = "",stringsAsFactors = F)

#### external non-session server functions and varibales ####

column_names <- c("Id","CHR","POS","rsID","REF","ALT","QUAL","Func","GENE","CONSEQ","X1000G_all",
                  "ExAC_ALL","ExAC_NFE","HET_val","HOM_val","HET_rate","HOM_rate","MISS_rate",
                  "INT_freq","AggAF_Trunc","AggAF_nsyn","SIFT","POLYP","POLYP_VAR","LRT",
                  "MUT_TASTER","MUT_ASSESSOR","FATHMM","PROVEAN","CADD","TRANSCRIPT","EXON","DNA",
                  "AA","CLINVAR","DISEASE","COSMIC_ID","COSMIC_COUNTS")

main_table <- c("CHR","POS","rsID","REF","ALT","GENE","CONSEQ","TRANSCRIPT","EXON","DNA",
                "AA","X1000G_all","ExAC_ALL","INT_freq","MISS_rate","SIFT","POLYP","CADD")

sitreinfo_cols <- c("rsID","REF","ALT","QUAL","GENE","Func","CONSEQ")

siteconseq_cols <- c("SIFT","POLYP","POLYP_VAR","LRT","MUT_TASTER","MUT_ASSESSOR","FATHMM","PROVEAN","CADD")

siteclinc_cols <- c("CLINVAR","DISEASE")

siterarity_cols <- c("INT_freq","X1000_all","ExAC_ALL","ExAC_NFE")

mut_types <- c("T>C","T>A","T>G","C>T","C>G","C>A","G>A","G>T","G>C","A>C","A>T","A>G")

colfuncCADD <- colorRampPalette(c("chartreuse4","goldenrod2","orangered3"))

gene_links <- function(gene){
  refs <- c("http://www.genecards.org/cgi-bin/carddisp.pl?gene=",
            "http://www.ensembl.org/id/",
            "https://www.genenames.org/cgi-bin/gene_symbol_report?hgnc_id=",
            "https://www.ncbi.nlm.nih.gov/gene/",
            "http://www.uniprot.org/uniprot/",
            "https://www.omim.org/entry/")
  L <- vector()  
  for(i in 1:6){
    if(is.na(gene_data[gene_data$Gene.name == gene,][,i])){
      L <- append(L,"n/a") 
    } else {
      val <- gene_data[gene_data$Gene.name == gene,][,i]
      L <- append(L,paste('<a href=\"',refs[i],val,'\" target="_blank">',val,'</a>',sep = ""))
    }
  }
  L <- unlist(L)
  
  return(L)
}

## test variable for site panel
testVAR <- variant_data[[3]][454,]
names(testVAR)[1:38] <- column_names

#### JSON PREP ####

## Cohort JSON
sample_cohortJSON <- as.data.frame(table(sample_data$COHORT))
colnames(sample_cohortJSON) <- c("name","total")
sample_cohortJSON <- sample_cohortJSON[order(sample_cohortJSON$total,decreasing = T),]
var_cohort_json <- toJSON(sample_cohortJSON)

## Age JSON
sample_ageJSON <- as.data.frame(table(cut(sample_data$AGE,seq.int(1,100,10),include.lowest = T)))
sample_ageJSON$Var1 <- gsub(pattern = "(\\[|\\()(.*)\\,(.*)\\]","\\2-\\3",sample_ageJSON$Var1)
sample_ageJSON <- sample_ageJSON[sample_ageJSON$Freq != 0,]
colnames(sample_ageJSON) <- c("name","total")
sample_ageJSON <- sample_ageJSON[order(sample_ageJSON$total,decreasing = T),]
var_age_json <- toJSON(sample_ageJSON)

## Ethno JSON
sample_ethnoJSON <- as.data.frame(table(sample_data$ETHNO))
colnames(sample_ethnoJSON) <- c("name","total")
sample_ethnoJSON <- sample_ethnoJSON[order(sample_ethnoJSON$total,decreasing = T),]
var_ethno_json <- toJSON(sample_ethnoJSON)

## Sex JSON
sample_sexJSON <- as.data.frame(table(sample_data$SEX))
colnames(sample_sexJSON) <- c("name","total")
var_sex_json <- toJSON(sample_sexJSON)

## Pheno JSON
sample_phenoJSON <- as.data.frame(table(sample_data$PHENOTYPE))
colnames(sample_phenoJSON) <- c("pheno","total")
sample_phenoJSON <- sample_phenoJSON[order(sample_phenoJSON$total,decreasing = T),]
var_pheno_json <- toJSON(sample_phenoJSON)

## Main page plot JSON
var_counts <- data.frame(samples=vector(),vars=vector(),cohort=vector())
for(i in 2:length(variant_data)){
var_counts_sample <- apply(variant_data[[i]][39:ncol(variant_data[[i]])],2,function(x) length(x[x == 1 | x == 2]))
var_counts <- rbind.data.frame(var_counts,data.frame(samples=c(names(var_counts_sample)),
                                                  variants=c(as.numeric(var_counts_sample)),
                                                  cohort=rep_len(names(variant_data[i]),length.out = length(var_counts_sample)),stringsAsFactors = F))
}
var_counts <- var_counts[order(var_counts$variants,decreasing = T),]
var_main_json <- toJSON(var_counts)

#### UI START ####
ui = fluidPage(theme = shinytheme("flatly"),
#### required calls to packages and external files ####          
      useShinyjs(),
      useShinyalert(),
      
      tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "https://cdnjs.cloudflare.com/ajax/libs/c3/0.4.10/c3.min.css"),
      tags$style(type="text/css", "body {padding-top: 80px;}"),
      tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/d3/3.5.16/d3.min.js"),
      tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/c3/0.4.10/c3.min.js"),
      tags$script(src="plots.js"),
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
      ),

      navbarPage(title = "MedGenDP", collapsible = TRUE,position = "fixed-top",
#### Data Summary panel ####
        tabPanel(title = "Data summary", icon = icon("table"),
        fluidRow(
          column(5,h3(tags$b("Medical Genetics Data Portal")),style='margin-bottom: 5px;'),
          column(2,offset = 5,h5(tags$em("Version 0.3"),style='text-align: right; margin-top: 25px;'))
        ),
        hr(),
        fluidRow(
          column(2,style = 'margin-top: 42.5px; padding-left: 30px',
                 fluidRow(h4(tags$b("Data summary"),style = "text-align: left")
                 ),
                 fluidRow(
                   column(6,style = "text-align: center",h4("Samples"),tags$b(formatC(length(unique(sample_data$SAMPLE_ID)),format="d", big.mark=","))),
                   column(6,style = "text-align: center",h4("Populations"),tags$b(formatC(length(unique(sample_data$ETHNO)),format="d", big.mark=",")))
                 ),
                 fluidRow(
                   column(6,style = "text-align: center",h4("Cohorts"),tags$b(formatC(length(variant_data),format="d", big.mark=","))),
                   column(6,style = "text-align: center",h4("Variants"),tags$b(formatC(sum(sapply(variant_data,nrow)),format="d", big.mark=",")))
                 ),
                 fluidRow(
                   column(6,style = "text-align: center",h4("Phenotypes"),tags$b(formatC(length(unique(sample_data$PHENOTYPE)),format="d", big.mark=","))),
                   column(6,style = "text-align: center",h4("Genes"),tags$b(formatC(length(unique(unlist(sapply(variant_data,function(x) unique(x$Gene.refGene))))),format="d", big.mark=",")))
                 )
                ),
          column(width = 10,
              column(3,tags$div(id="plotCohort")),
              column(3,tags$div(id="plotAge")),
              column(3,tags$div(id="plotEthno")),
              column(3,tags$div(id="plotSex"))
                 )
        ),
        hr(),
        fluidRow(
        column(width = 4,
        h4(tags$b("Phenotype Summary"))
               ),
        column(width = 8,
        h4(tags$b("Information"))
              )
        ),
        fluidRow(
          column(12,
                 column(4,style = "height: 400px",tags$div(id="plotPheno")),
                 column(8,
                        bs_accordion(id = "splash_info") %>%
                          bs_set_opts(panel_type = "default", use_heading_link = TRUE) %>%
                          bs_append(title = "Features", content = tags$div(HTML("
                  <p>The MedGen Data Portal (MGDP) is an intergrated platform for the viewing and querying of data filtered from the MedGen Exome Cohorts.</p>
                  <p>Provided tools:</P>
                  <ul>
                  <li>Variant browser</li>
                  <li>Cohort Comparison - Not Implemented</li>
                  <li>Additional databases - Not Implemented</li>
                  </ul>
                  <p>The Variant browser allows for interrogation of variants present individual cohorts. Data can be searched by:</P>
                  <ul>
                  <li>Chromosome position / rsID</li>
                  <li>Gene / Gene lists</li>
                  <li>Sample</li>
                  </ul>
                  "))) %>%
                          bs_append(title = "Data generation", content = tags$div(HTML("
                  <p>Data present in the MGVB is derivied from unfiltered multi-sample VCF files</p>
                  <p>Variants are filtered according to the following metrics:</P>
                  <ul>
                  <li>Read depth: ##</li>
                  <li>Genotype quality: ##</li>
                  <li>QUAL: ##</li>
                  <li>Global MAF (ExAC & 1K genomes): ##</li>
                  <li>Missingness: ##</li>
                  </ul>
                  <p>Variants were annotated with Annovar</P>
                  <p>See the 'About' page for more information on scripts and methodology</p>
                  ")))
                        )
                 )
          )
        ),
#### Data selection panel ####
      tabPanel(title = "Browser", icon = icon("flask"),
       h4(tags$b("Variant Browser")),
        column(width = 2,
          fluidRow(
            column(12,selectInput(inputId = "cohort",label = "Cohort",choices = c(names(variant_data),"None"),selected = "None"))
          ),
          fluidRow(
           column(12,actionButton(inputId = "reset",label = "",icon = icon("undo"),width = "100%",style='padding:2px;'))
          ),
          fluidRow(
           column(12,actionButton(inputId = "site_test",label = "Test")) 
          ),
         tags$hr(),
         div(id = "animation",
         conditionalPanel("input.cohort != 'None'",
          column(width = 12,
            h4("Data options"),
            tabsetPanel(id = "data_select",type = "tabs",
                  tabPanel(title = "Position",value = "data_select_pos",
                   wellPanel(
                    fluidRow(
                      column(12, textInput(inputId = "pos", label = "Position(s)",placeholder = "X:2335 or X:2335-4898"))
                      ),
                    h5(tags$em("Fitlering options")),
                    fluidRow(
                      column(12,materialSwitch(inputId = "syn_cord_pos",label = "Include synonymous",value = FALSE,status = "primary",right = TRUE)
                        )),
                    fluidRow(
                      column(12,materialSwitch(inputId = "intron_cord_pos",label = "Include intronic",value = FALSE,status = "primary",right = TRUE)
                         )),
                    fluidRow(
                      column(12,materialSwitch(inputId = "trunc_cord_pos",label = "Truncating only",value = FALSE,status = "primary",right = TRUE)
                        )),
                    fluidRow(
                      column(12,materialSwitch(inputId = "splice_cord_pos",label = "Splicing only",value = FALSE,status = "primary",right = TRUE)
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
                    fluidRow(
                      column(12,prettyCheckbox(inputId = "gene_wildcard",
                                               label = "Exact match", 
                                               value = FALSE,
                                               animation = "smooth",
                                               status = "primary",
                                               shape = "curve",
                                               icon = icon("check"))
                      )),
                    h5(tags$em("Fitlering options")),
                    fluidRow(
                      column(12,materialSwitch(inputId = "syn_cord_gene",label = "Include synonymous",value = FALSE,status = "primary",right = TRUE)
                             )),
                    fluidRow(
                      column(12,materialSwitch(inputId = "intron_cord_gene",label = "Include intronic",value = FALSE,status = "primary",right = TRUE)
                             )),
                    fluidRow(
                      column(12,materialSwitch(inputId = "trunc_cord_gene",label = "Truncating only",value = FALSE,status = "primary",right = TRUE)
                             )),
                    fluidRow(
                      column(12,materialSwitch(inputId = "splice_cord_gene",label = "Splicing only",value = FALSE,status = "primary",right = TRUE)
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
                      column(12,materialSwitch(inputId = "syn_cord_sample",label = "Include synonymous",value = FALSE,status = "primary",right = TRUE)
                            )),
                    fluidRow(
                      column(12,materialSwitch(inputId = "intron_cord_sample",label = "Include intronic",value = FALSE,status = "primary",right = TRUE)
                            )),
                    fluidRow(
                      column(12,materialSwitch(inputId = "trunc_cord_sample",label = "Truncating only",value = FALSE,status = "primary",right = TRUE)
                            )),
                    fluidRow(
                      column(12,materialSwitch(inputId = "splice_cord_sample",label = "Splicing only",value = FALSE,status = "primary",right = TRUE)
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
                      div(withSpinner(dataTableOutput(outputId = "table_cord"),type = 4,color = "#95a5a6"),style = "align: center; font-size: 80%; width: 100%")
                            )
                        )
                    # fluidRow(
                    #   column(width = 2, offset = 10, downloadButton(outputId = "data_dl",label = "Save table as csv", icon = icon("floppy-o")))
                    # )
                  ),
                tabPanel("Gene", value = "gene_panel",
                    h4(textOutput(outputId = "id_gene")),
                    h4(""),
                    div(withSpinner(dataTableOutput(outputId = "table_gene"),type = 4,color = "#95a5a6"),style = "font-size: 80%; width: 100%")
                    ),
                tabPanel("", value = "single_gene_panel",
                         h2(textOutput(outputId = "single_gene_panel_title")),
                         tags$hr(),
                         textOutput(outputId = "single_gene_panel_desc"),
                         h4(""),
                         fluidRow(
                         column(4,
                                h4("Gene information"),
                                dataTableOutput(outputId = "single_gene_panel_info")),
                         column(4,offset = 4,
                                h4("External databases"),
                                dataTableOutput(outputId = "single_gene_panel_ext"))
                         ),
                         h4(""),
                         tags$hr(),
                         fluidRow(
                         div(withSpinner(dataTableOutput(outputId = "table_single_gene"),type = 4,color = "#95a5a6"),style = "font-size: 80%; width: 100%")
                         )
                ),
                tabPanel("Site", value = "site_panel",
                        h2(textOutput(outputId = "site_title")),
                        tags$hr(),
                        fluidRow(
                          column(4,
                            fluidRow(
                              column(12,
                                h4("Site information"),
                                dataTableOutput(outputId = "site_info")
                                )
                            ),
                            fluidRow(
                              column(12,
                               h4(""),
                               selectInput(inputId = "site_transcript_select", multiple = FALSE, label = "Affected transcripts", choices = NULL)
                              )
                            ),
                            fluidRow(
                              column(12,
                              dataTableOutput(outputId = "site_transcript_table")
                              )
                            )
                          ),
                          column(4,
                              h4("PLOT PLACEHOLDER")
                            ),
                          column(4,
                              h4("PLOT PLACEHOLDER")
                            )
                        ),
                        h4(""),
                        tags$hr(),
                        fluidRow(
                          column(12,
                                 h4("Damage prediction"),
                              wellPanel(
                                 dataTableOutput(outputId = "site_conseq")
                              )
                            )
                        ),
                        tags$hr(),
                        column(8,
                          fluidRow(
                            column(12,
                                   h4("Genotype information")
                            )
                          ),
                          wellPanel(
                          fluidRow(
                            column(6,style = "margin-top: 35.5px",
                                     column(8,
                                            fluidRow(tags$b("Heterozygous Calls:")),
                                            fluidRow(tags$b("Homozygous Calls:")),
                                            fluidRow(tags$b("Missing rate Calls:"))
                                     ),
                                     column(4,
                                            fluidRow(tags$em(textOutput(outputId = "site_hetv"))),
                                            fluidRow(tags$em(textOutput(outputId = "site_homv"))),
                                            fluidRow(tags$em(textOutput(outputId = "site_miss")))
                                     )
                            ),
                            column(6,
                              fluidRow(
                                column(8,
                                       selectInput(inputId = "site_sample_select", multiple = FALSE, label = "Affected Samples", choices = NULL)
                                      )
                                    ),
                              fluidRow(
                                column(12,
                                  div(dataTableOutput(outputId = "site_sample_table"),style = "font-size: 85%; width: 100%")
                                      )
                                    )
                            )
                          )
                          ),
                          tags$hr(),
                          fluidRow(
                            column(12,
                                   h4("Clinvar & Cosmic")
                            )
                          ),
                          wellPanel(
                          fluidRow(
                            column(12,
                                   dataTableOutput(outputId = "site_clinc")
                                   )
                          )
                         )
                       ),
                       column(4,
                        fluidRow(
                         column(12,
                                h4("Allele frequency")
                                )
                         ),
                         fluidRow(
                           column(12,
                                  dataTableOutput(outputId = "site_rarity_table")
                                 )
                         )
                       )
                ),
                tabPanel("Sample", value = "sample_panel",
                    h4(textOutput(outputId = "id_sample")),
                    wellPanel(
                      fluidRow(
                        column(width = 2, strong("Sex")),
                        column(width = 2, strong("Phenotype")),
                        column(width = 2, strong("Age of onset")),
                        column(width = 3, strong("Ethnicity (PCA)")),
                        column(width = 2, strong("Vital status"))
                              ),
                      br(),
                      fluidRow(
                        column(width = 2, textOutput(outputId = "sample_sex")),
                        column(width = 2, textOutput(outputId = "sample_pheno")),
                        column(width = 2, textOutput(outputId = "sample_age")),
                        column(width = 3, textOutput(outputId = "sample_ethno")),
                        column(width = 3, textOutput(outputId = "sample_MORT"))
                              )
                      ),
                      
                      h4("Sample summary metrics"),
                      fluidRow(
                        column(12,
                        column(width = 4, tags$div(id="plotSampleVars")),
                        column(width = 4, tags$div(id="plotSampleMut")),
                        column(width = 4, tags$div(id="plotSampleRare"))
                              )
                        ),
                      tags$hr(),
                      h4(""),
                      h4(textOutput(outputId = "sampleid_table")),
                      fluidRow(
                        div(withSpinner(dataTableOutput(outputId = "table_sample"),type = 4,color = "#95a5a6"),style = "font-size: 80%; width: 100%")
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
                 fluidRow()
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

#### UI END
#### SERVER START #### 
server = function(input, output, session){

  
## Site panel features - move once done
updateSelectInput(session,
                  inputId = "site_sample_select",
                  choices = as.character(sample_data$SAMPLE_ID[sample_data$DATA_ID %in% names(testVAR[39:ncol(testVAR)][which(testVAR[39:ncol(testVAR)] == 1 | testVAR[39:ncol(testVAR)] == 2)])])
                  )
  
observe({
  output$site_sample_table <- renderDataTable({datatable(as.data.frame(sample_data[sample_data$SAMPLE_ID == input$site_sample_select,][c(1,4:8)]),
    class = 'compact',
    selection = 'none',
    rownames = FALSE,
    escape = FALSE,
    options = list(dom = 't',ordering=F)
      )
    })
})

## Transcript table
updateSelectInput(session, inputId = "site_transcript_select", choices = unlist(str_split(testVAR$TRANSCRIPT,",")))
transcripts <- testVAR[names(testVAR) %in% c("TRANSCRIPT","EXON","DNA","AA")]

if(length(str_split(transcripts$TRANSCRIPT,pattern = ",",simplify = T)) > 1){
  transcripts <- as.data.frame(apply(transcripts,2,function(x) str_split(x,pattern = ",",simplify = T)))
}
  
observe({
  output$site_transcript_table <- renderDataTable({datatable(transcripts[transcripts$TRANSCRIPT == input$site_transcript_select,],
                                                         class = 'compact',
                                                         selection = 'none',
                                                         rownames = FALSE,
                                                         escape = FALSE,
                                                         options = list(dom = 't',ordering=F)
                                            )
                                            })
})

observe({
  index_AF <- names(testVAR[names(testVAR) %in% siterarity_cols])
  site_AF <- t(testVAR[names(testVAR) %in% siterarity_cols])
  site_AF <- data.frame(index_AF,site_AF)
  colnames(site_AF) <- c("Dataset","AF")
  output$site_rarity_table <- renderDataTable({datatable(site_AF,
                                                             selection = 'none',
                                                             rownames = FALSE,
                                                             escape = FALSE,
                                                             options = list(dom = 't',ordering=F)
                                                      )
                                                    })
})
    
#### Json main page - cohort graphs ####
  
session$sendCustomMessage("cohortjson",var_cohort_json)
session$sendCustomMessage("agejson",var_age_json) 
session$sendCustomMessage("ethnojson",var_ethno_json)
session$sendCustomMessage("pheno_barjson",var_pheno_json)
session$sendCustomMessage("sexjson",var_sex_json)

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

observe({
  if(input$trunc_cord_pos == TRUE){
    updateMaterialSwitch(session,"syn_cord_pos",value = FALSE)
    updateMaterialSwitch(session,"intron_cord_pos",value = FALSE)
    updateMaterialSwitch(session,"splice_cord_pos",value = FALSE)
  }
})
observe({
  if(input$syn_cord_pos == TRUE | input$intron_cord_pos == TRUE){
    updateMaterialSwitch(session,"trunc_cord_pos",value = FALSE)
    updateMaterialSwitch(session,"splice_cord_pos",value = FALSE)
  }
})
observe({  
  if(input$splice_cord_pos == TRUE){
    updateMaterialSwitch(session,"syn_cord_pos",value = FALSE)
    updateMaterialSwitch(session,"intron_cord_pos",value = FALSE)
    updateMaterialSwitch(session,"trunc_cord_pos",value = FALSE)
  }
})

observe({  
  if(input$trunc_cord_sample == TRUE){
    updateMaterialSwitch(session,"syn_cord_sample",value = FALSE)
    updateMaterialSwitch(session,"intron_cord_sample",value = FALSE)
    updateMaterialSwitch(session,"splice_cord_sample",value = FALSE)
  }
})
observe({
  if(input$syn_cord_sample == TRUE | input$intron_cord_sample == TRUE){
    updateMaterialSwitch(session,"trunc_cord_sample",value = FALSE)
    updateMaterialSwitch(session,"splice_cord_sample",value = FALSE)
  }
})
observe({
  if(input$splice_cord_sample == TRUE){
    updateMaterialSwitch(session,"syn_cord_sample",value = FALSE)
    updateMaterialSwitch(session,"intron_cord_sample",value = FALSE)
    updateMaterialSwitch(session,"trunc_cord_sample",value = FALSE)
  }
})
observe({
  if(input$trunc_cord_gene == TRUE){
    updateMaterialSwitch(session,"syn_cord_gene",value = FALSE)
    updateMaterialSwitch(session,"intron_cord_gene",value = FALSE)
    updateMaterialSwitch(session,"splice_cord_gene",value = FALSE)
  }
})
observe({
  if(input$syn_cord_gene == TRUE | input$intron_cord_gene == TRUE){
    updateMaterialSwitch(session,"trunc_cord_gene",value = FALSE)
    updateMaterialSwitch(session,"splice_cord_gene",value = FALSE)
  }
})
observe({
  if(input$splice_cord_gene == TRUE){
    updateMaterialSwitch(session,"syn_cord_gene",value = FALSE)
    updateMaterialSwitch(session,"intron_cord_gene",value = FALSE)
    updateMaterialSwitch(session,"trunc_cord_gene",value = FALSE)
  }
})


observeEvent(input$reset, {
  shinyjs::reset("cohort")
  updateTabsetPanel(session, "main_panel", selected = "blank_panel")
})

hide(id = "main_panel")

#### Panel switching & hiding ####

  observeEvent(input$but_pos, {if(input$main_panel != "pos_panel"){updateTabsetPanel(session, "main_panel", selected = "pos_panel")}})
  observeEvent(input$but_gene, {if(input$main_panel != "gene_panel"){updateTabsetPanel(session, "main_panel", selected = "gene_panel")}})
  observeEvent(input$but_sample, {if(input$main_panel != "sample_panel"){updateTabsetPanel(session, "main_panel", selected = "sample_panel")}})
  observeEvent(input$site_test, {if(input$main_panel != "site_panel"){updateTabsetPanel(session, "main_panel", selected = "site_panel")}})

#### COORDINATE SEARCH ####
  #addrsID search
  #Evaluate if the coordinate provided looks roughly like a genomic coordinate
  val_cord <- eventReactive(input$but_pos, {
    shiny::validate(
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
    datC <- variant_data[[which(names(variant_data) == input$cohort)]]
    
    if(is.na(stop1)){
      datC <- datC[datC$CHROM == chr & datC$POS == start1,]
      datC <- datC[1:15]
      return(datC)
      ##report data back to function2:1-9999999999999999
    } else {
      datC <- datC[datC$CHROM == chr 
            & as.numeric(datC$POS) >= as.numeric(start1)
            & as.numeric(datC$POS) <= as.numeric(stop1),]
    
    
      datC <- datC[datC$X1000g2015aug_all <= input$X1000G_rarity_pos & datC$ExAC_ALL <= input$Exac_rarity_pos,]
    ########### INTRONIC   
      if(input$intron_cord_pos == FALSE){
      datC <- datC[!is.na(datC$ExonicFunc.refGene),]
      }
    
      if(input$syn_cord_pos == FALSE){
        datC <- datC[which(datC$ExonicFunc.refGene != "synonymous"),]
      }
      if(input$trunc_cord_pos == TRUE){
        datC <- datC[datC$ExonicFunc.refGene == "stopgain" 
                     | datC$ExonicFunc.refGene == "frameshift deletion"
                     | datC$ExonicFunc.refGene == "frameshift insertion"
                     | datC$ExonicFunc.refGene == "stoploss",]
      }
      if(input$splice_cord_pos == TRUE){
        datC <- datC[datC$ExonicFunc.refGene == "splicing",]
      }
    ##report table to render function
      names(datC)[1:38] <- column_names
      datC <- datC[colnames(datC) %in% main_table]
      datC <- datC[main_table]
      ## REMOVE AFTER FIXED IN PREPROCESS SCRIPT
      datC$AA[is.na(datC$AA)] <- "NA"
    ##report data back to function
      return(datC)
    }
  })

#### GENE SEARCH ####
  ## Add non-wildcard
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
      datG <- variant_data[[which(names(variant_data) == input$cohort)]]
      if(length(gene_list) >= 2){
        no_list <- gene_list[!gene_list %in% datG$Gene.refGene]
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
            )
            updateTextInput(session,"gene",value = gene_list[!gene_list %in% no_list])
          }
        gene_list <- gene_list[gene_list %in% datG$Gene.refGene]
        datG <- datG[datG$Gene.refGene %in% gene_list,]
        updatePrettyCheckbox(session,inputId = "gene_wildcard",value = FALSE)
      } else if(input$gene_wildcard == FALSE) {
        datG <- datG[grep(gene_list,datG$Gene.refGene),]
      } else {
        datG <- datG[datG$Gene.refGene == gene_list,]
        if(input$main_panel != "single_gene_panel"){updateTabsetPanel(session, "main_panel", selected = "single_gene_panel")}
      }
      
      datG <- datG[datG$X1000g2015aug_all <= input$X1000G_rarity_gene & datG$ExAC_ALL <= input$Exac_rarity_gene,]
      ########### INTRONIC   
      if(input$intron_cord_gene == FALSE){
        datG <- datG[!is.na(datG$ExonicFunc.refGene),]
      }
      
      if(input$syn_cord_gene == FALSE){
        datG <- datG[datG$ExonicFunc.refGene != "synonymous",]
      }
      if(input$trunc_cord_gene == TRUE){
        datG <- datG[datG$ExonicFunc.refGene == "stopgain" 
                     | datG$ExonicFunc.refGene == "frameshift deletion"
                     | datG$ExonicFunc.refGene == "frameshift insertion"
                     | datG$ExonicFunc.refGene == "stoploss",]
      }
      if(input$splice_cord_gene == TRUE){
        datG <- datG[datG$ExonicFunc.refGene == "splicing",]
      }
      ##report table to render function
      names(datG)[1:38] <- column_names
      datG <- datG[colnames(datG) %in% main_table]
      datG <- datG[main_table]
      ## REMOVE AFTER FIXED IN PREPROCESS SCRIPT
      datG$AA[is.na(datG$AA)] <- "NA"
      return(datG)
  })


#### SAMPLE LOOKUP ####
  ##Generates list of available samples to look through on search field
  observe({
    if (input$cohort != "None") {
    updateSelectizeInput(session, 'sample', 
                         choices = sample_data$SAMPLE_ID[sample_data$COHORT == input$cohort], 
                         server = TRUE,
                         options = list(placeholder = 'Sample ID'))
    }
  })
  ##makes search event reactive and stores sample name as variable
  val_sample <- eventReactive(input$but_sample, {
    req(input$sample)
    return(as.character(sample_data$DATA_ID[sample_data$SAMPLE_ID == input$sample]))
  })
  output$sample_sex <- renderText({paste(sample_data$SEX[sample_data$DATA_ID == val_sample()])})
  output$sample_pheno <- renderText({paste(sample_data$PHENO[sample_data$DATA_ID == val_sample()])})
  output$sample_age <- renderText({paste(sample_data$AGE[sample_data$DATA_ID == val_sample()])})
  output$sample_ethno <- renderText({paste(sample_data$ETHNO[sample_data$DATA_ID == val_sample()])})
  output$sample_MORT <- renderText({paste(sample_data$MORT_STATUS[sample_data$DATA_ID == val_sample()])})
  
  data_sample <- eventReactive(input$but_sample, {
    datS <- variant_data[[which(names(variant_data) == input$cohort)]]
    ##filter table for input sample
    datS <- datS[datS[,val_sample()] == 1 | datS[,val_sample()] == 2 ,]
    
    datS <- datS[datS$X1000g2015aug_all <= input$X1000G_rarity_sample & datS$ExAC_ALL <= input$Exac_rarity_sample,]
    ########### INTRONIC   
    if(input$intron_cord_sample == FALSE){
      datS <- datS[!is.na(datS$ExonicFunc.refGene),]
    }
    
    if(input$syn_cord_sample == FALSE){
      datS <- datS[datS$ExonicFunc.refGene != "synonymous",]
    }
    if(input$trunc_cord_sample == TRUE){
      datS <- datS[datS$ExonicFunc.refGene == "stopgain" 
                   | datS$ExonicFunc.refGene == "frameshift deletion"
                   | datS$ExonicFunc.refGene == "frameshift insertion"
                   | datS$ExonicFunc.refGene == "stoploss",]
    }
    if(input$splice_cord_sample == TRUE){
      datS <- datS[datS$ExonicFunc.refGene == "splicing",]
    }
  
    ##report table to render function
    names(datS)[1:38] <- column_names
    datS <- datS[colnames(datS) %in% main_table]
    datS <- datS[main_table]
    ## REMOVE AFTER FIXED IN PREPROCESS SCRIPT
    datS$AA[is.na(datS$AA)] <- "NA"
    ##report table to render function
    return(datS)
  })

#### Data table outputs ####
output$table_cord <- renderDataTable({datatable(data_cord(),
  rownames = FALSE,
    options = list(
    pageLength = 10,
    lengthMenu = c(10, 20, 50, 100),
    searchHighlight = TRUE,
    columnDefs = list(
     list(
      targets = c(7:10),
      render = JS(
        "function(data, type, row, meta) {",
        "return type === 'display' && data.indexOf(',') > 0 ?",
        "'<span title=\"' + data + '\">' + data.substr(0, data.indexOf(',')) + '...</span>' : data;",
        "}")),
     list(
      targets = c(3,4),
      render = JS(
        "function(data, type, row, meta) {",
        "return type === 'display' && data.length > 5 ?",
        "'<span title=\"' + data + '\">' + data.substr(0, 5) + '...</span>' : data;",
        "}"))
      )
    )
  ) %>% 
  formatStyle('CADD',
              color = styleInterval(seq.int(1,max(data_cord()$CADD,na.rm = TRUE),1),colfuncCADD(max(seq.int(1,max(data_cord()$CADD,na.rm = TRUE)+1,1)))),
              fontWeight = 'bold'
  ) %>% 
  formatStyle(
      'SIFT',
      color = styleEqual(sort(unique(data_cord()$SIFT)),colfuncCADD(length(sort(unique(data_cord()$SIFT))))),
      fontWeight = 'bold'
  ) %>% 
  formatStyle(
      'POLYP',
      color = styleEqual(sort(unique(data_cord()$POLYP)),colfuncCADD(length(sort(unique(data_cord()$POLYP))))),
      fontWeight = 'bold'
  )
  })

output$table_gene <- renderDataTable({datatable(val_gene(),
                                                rownames = FALSE,
                                                options = list(
                                                  pageLength = 10,
                                                  lengthMenu = c(10, 20, 50, 100),
                                                  searchHighlight = TRUE,
                                                  columnDefs = list(
                                                    list(
                                                      targets = c(7:10),
                                                      render = JS(
                                                        "function(data, type, row, meta) {",
                                                        "return type === 'display' && data.indexOf(',') > 0 ?",
                                                        "'<span title=\"' + data + '\">' + data.substr(0, data.indexOf(',')) + '...</span>' : data;",
                                                        "}")),
                                                    list(
                                                      targets = c(3,4),
                                                      render = JS(
                                                        "function(data, type, row, meta) {",
                                                        "return type === 'display' && data.length > 5 ?",
                                                        "'<span title=\"' + data + '\">' + data.substr(0, 5) + '...</span>' : data;",
                                                        "}"))
    )
  )
) %>% 
    formatStyle('CADD',
                color = styleInterval(seq.int(1,max(val_gene()$CADD,na.rm = TRUE),1),colfuncCADD(max(seq.int(1,max(val_gene()$CADD,na.rm = TRUE)+1,1)))),
                fontWeight = 'bold'
    ) %>% 
    formatStyle(
      'SIFT',
      color = styleEqual(sort(unique(val_gene()$SIFT)),colfuncCADD(length(sort(unique(val_gene()$SIFT))))),
      fontWeight = 'bold'
    ) %>% 
    formatStyle(
      'POLYP',
      color = styleEqual(sort(unique(val_gene()$POLYP)),colfuncCADD(length(sort(unique(val_gene()$POLYP))))),
      fontWeight = 'bold'
    )
})                                               

output$table_single_gene <- renderDataTable({datatable(val_gene(),
                                                       rownames = FALSE,
                                                       options = list(
                                                         pageLength = 10,
                                                         lengthMenu = c(10, 20, 50, 100),
                                                         searchHighlight = TRUE,
                                                         columnDefs = list(
                                                           list(
                                                             targets = c(7:10),
                                                             render = JS(
                                                               "function(data, type, row, meta) {",
                                                               "return type === 'display' && data.indexOf(',') > 0 ?",
                                                               "'<span title=\"' + data + '\">' + data.substr(0, data.indexOf(',')) + '...</span>' : data;",
                                                               "}")),
                                                           list(
                                                             targets = c(3,4),
                                                             render = JS(
                                                               "function(data, type, row, meta) {",
                                                               "return type === 'display' && data.length > 5 ?",
                                                               "'<span title=\"' + data + '\">' + data.substr(0, 5) + '...</span>' : data;",
                                                               "}"))
                                                         )
                                                       )
) %>% 
    formatStyle('CADD',
                color = styleInterval(seq.int(1,max(val_gene()$CADD,na.rm = TRUE),1),colfuncCADD(max(seq.int(1,max(val_gene()$CADD,na.rm = TRUE)+1,1)))),
                fontWeight = 'bold'
    ) %>% 
    formatStyle(
      'SIFT',
      color = styleEqual(sort(unique(val_gene()$SIFT)),colfuncCADD(length(sort(unique(val_gene()$SIFT))))),
      fontWeight = 'bold'
    ) %>% 
    formatStyle(
      'POLYP',
      color = styleEqual(sort(unique(val_gene()$POLYP)),colfuncCADD(length(sort(unique(val_gene()$POLYP))))),
      fontWeight = 'bold'
    )
})                                                     

output$table_sample <- renderDataTable({datatable(data_sample(),
                                                  rownames = FALSE,
                                                  options = list(
                                                    pageLength = 10,
                                                    lengthMenu = c(10, 20, 50, 100),
                                                    searchHighlight = TRUE,
                                                    columnDefs = list(
                                                      list(
                                                        targets = c(7:10),
                                                        render = JS(
                                                          "function(data, type, row, meta) {",
                                                          "return type === 'display' && data.indexOf(',') > 0 ?",
                                                          "'<span title=\"' + data + '\">' + data.substr(0, data.indexOf(',')) + '...</span>' : data;",
                                                          "}")),
                                                      list(
                                                        targets = c(3,4),
                                                        render = JS(
                                                          "function(data, type, row, meta) {",
                                                          "return type === 'display' && data.length > 5 ?",
                                                          "'<span title=\"' + data + '\">' + data.substr(0, 5) + '...</span>' : data;",
                                                          "}"))
                                                    )
                                                  )
) %>% 
    formatStyle('CADD',
                color = styleInterval(seq.int(1,max(data_sample()$CADD,na.rm = TRUE),1),colfuncCADD(max(seq.int(1,max(data_sample()$CADD,na.rm = TRUE)+1,1)))),
                fontWeight = 'bold'
    ) %>% 
    formatStyle(
      'SIFT',
      color = styleEqual(sort(unique(data_sample()$SIFT)),colfuncCADD(length(sort(unique(data_sample()$SIFT))))),
      fontWeight = 'bold'
    ) %>% 
    formatStyle(
      'POLYP',
      color = styleEqual(sort(unique(data_sample()$POLYP)),colfuncCADD(length(sort(unique(data_sample()$POLYP))))),
      fontWeight = 'bold'
    )
}) 

output$single_gene_panel_ext <- renderDataTable({datatable(data.frame(Database=c("Genecards","Ensembl","HGNC","NCBI","Uniprot","OMIM"),
                                                                         Link=gene_links(unique(val_gene()$GENE))),
                                                              class = 'compact',
                                                              colnames = c("",""),
                                                              selection = 'none',
                                                              rownames = FALSE,
                                                              escape = FALSE,
                                                              options = list(dom = 't',ordering=F,
                                                                             initComplete = JS(
                                                                               "function(settings, json) {",
                                                                               "$(this.api().table().header()).css({'color': '#fff'});",
                                                                               "}")
                                                                             )
                                                              ) %>% 
                                                                    formatStyle(
                                                                      'Database',
                                                                      fontWeight = 'bold'
                                                                    )
                                                                  })

output$single_gene_panel_info <- renderDataTable({datatable(data.frame(Database=c("Chromosome","Start (bp)","End (bp)","Cytoband","Strand","Gene type"),
                                                                       Info=as.character(gene_info[gene_info$Gene.name == unique(val_gene()$GENE),][c(4,5,6,2,3,7)])),
                                                            class = 'compact',
                                                            colnames = c("",""),
                                                            selection = 'none',
                                                            rownames = FALSE,
                                                            escape = FALSE,
                                                            options = list(dom = 't',ordering=F,
                                                                           initComplete = JS(
                                                                             "function(settings, json) {",
                                                                             "$(this.api().table().header()).css({'color': '#fff'});",
                                                                             "}")
                                                            )
                                                            ) %>% 
                                                                formatStyle(
                                                                  'Database',
                                                                  fontWeight = 'bold'
                                                                )
                                                            })

output$site_info <- renderDataTable({datatable(data.frame(Names=names(testVAR[names(testVAR) %in% sitreinfo_cols]),values=as.character(testVAR[sitreinfo_cols])),
                                      class = 'compact',
                                      colnames = c("",""),
                                      selection = 'none',
                                      rownames = FALSE,
                                      escape = FALSE,
                                      options = list(dom = 't',ordering=F,
                                                     initComplete = JS(
                                                       "function(settings, json) {",
                                                       "$(this.api().table().header()).css({'color': '#fff'});",
                                                       "}")
                                      )
                                    ) %>% formatStyle(
                                        'Names',
                                        fontWeight = 'bold'
                                      )
                                    })

output$site_conseq <- renderDataTable({datatable(testVAR[siteconseq_cols],
                                               class = 'compact',
                                               selection = 'none',
                                               rownames = FALSE,
                                               escape = FALSE,
                                               options = list(dom = 't',ordering=F)
                                            ) 
                                          })

output$site_clinc <- renderDataTable({datatable(testVAR[siteclinc_cols],
                                                 class = 'compact',
                                                 selection = 'none',
                                                 rownames = FALSE,
                                                 escape = FALSE,
                                                 options = list(dom = 't',ordering=F)
                                            ) 
                                          })
                                                 

#### Plot rendering ####

## SamplePage - Variant plots
observe({
  SampleVarJSON <- as.data.frame(table(data_sample()$CONSEQ))
  colnames(SampleVarJSON) <- c("conseq","total")
  SampleVarJSON <- toJSON(SampleVarJSON)
  session$sendCustomMessage("SampleVarjson",SampleVarJSON)
})

observe({
  sampleMut_plotJSON <- as.data.frame(table(paste(data_sample()$REF,data_sample()$ALT,sep = ">")))
  sampleMut_plotJSON <- sampleMut_plotJSON[sampleMut_plotJSON$Var1 %in% mut_types,]
  colnames(sampleMut_plotJSON) <- c("mutation","total")
  sampleMut_plotJSON <- toJSON(sampleMut_plotJSON)
  session$sendCustomMessage("SampleMutjson",sampleMut_plotJSON)
})

observe({
  K <- as.data.frame(table(cut(data_sample()$X1000G_all,seq.int(0,0.05,0.01),include.lowest = T)))
  K <- c('1KG',as.numeric(K$Freq))
  E <- as.data.frame(table(cut(data_sample()$ExAC_ALL,seq.int(0,0.05,0.01),include.lowest = T)))
  E <- c('ExAC',as.numeric(E$Freq))
  I <- as.data.frame(table(cut(data_sample()$INT_freq,seq.int(0,0.05,0.01),include.lowest = T)))
  I <- c('Internal',as.numeric(I$Freq))
  sampleRare_plotJSON <- toJSON(rbind(K,E,I))
  
  session$sendCustomMessage("SampleRarejson",sampleRare_plotJSON)
})

#### Text rendering #### 

output$id_cord <- renderText({paste("Search results for ", val_cord())})
output$id_sample <- renderText({paste(sample_data$SAMPLE_ID[sample_data$DATA_ID == val_sample()])}) ##prints header for table with search value
output$id_gene <- renderText({label_gene()}) 
output$sampleid_table <- renderText({paste("Non-reference samples in ",sample_data$SAMPLE_ID[sample_data$DATA_ID == val_sample()])})
output$single_gene_panel_title <- renderText({unique(val_gene()$GENE)})
output$single_gene_panel_desc <- renderText({as.character(gene_info$Gene.description[gene_info$Gene.name == unique(val_gene()$GENE)])})
output$site_title <- renderText({paste(paste(gsub(pattern = "chr",x = testVAR[2],replacement = ""),":",testVAR[3],sep = ""),testVAR[5], "/", testVAR[6])})
output$site_hetv <- renderText({paste(testVAR$HET_val," (",round(testVAR$HET_rate,digits = 1),"%)",sep = "")})
output$site_homv <- renderText({paste(testVAR$HOM_val," (",round(testVAR$HOM_rate,digits = 1),"%)",sep = "")})
output$site_miss <- renderText({paste(testVAR$MISS_rate,"%")})

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

#### Server END
}
## App call
shinyApp(ui = ui, server = server)