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
library(reshape2)
library(plotly)
library(stringr)
library(markdown)

## SHINY APP - VARIANT BROWSER - ALPHA 1.3

## TO FIX!

## Plotly graph - selection of some sample points results in no info - ERROR:length of 'dimnames' [2] not equal to array extent
## External database links fail if table length = 0
## Gene-specific page and gene search page alternate when searching for the same, reflitering or searching for a new gene
## Session hang on empty tables
## gene fields with comma seperated entries
## colour schemes!!
## site info row names out of order
## Data undefined on donut plots - custom tooltips
## Warning in gene_info$Gene.name == unique(val_gene()$GENE) :longer object length is not a multiple of shorter object length - GENE SEARCH ERROR
## fix gene description printing on single gene page
## HET OR HOM CALL FOR SAMPLE CALLS - Add column?
## single gene panel reliant on genelinks() data containing values after filtering
## support for comma-sv in clinvar/cosmic table on site panel - add drop down?
## missing/empty value handling for plots - if no fs for exmaple, then data is misaligned in json
## Restore CADD to be numeric value with NA not N/a
## Filtering and including intronic makes the sample-AF plot render below 0
## Transcript table renders as empty if data is missing?
## Table header tooltips
## Transcript returns duplicated columns if empty on site-specific page
## Combine chr and pos in table output for ease of CopyPaste
## Intronic / synonymous filtering

## Links in table https://github.com/rstudio/DT/issues/58

#### External data ####
#variant_data <- read.table(file = "test_variant_data.txt", header = TRUE, sep = "\t")
load("www/db_cohortdata_SUBSET.RData")
variant_data <- cohort_list
rm(cohort_list)
sample_data <- read.table(file = "www/sample_data.final.info", header = TRUE, sep = "\t")
gene_data <- read.table(file = "www/gene_ids_reference.tsv", header = TRUE, sep = "\t",stringsAsFactors = F)
gene_info <- read.table(file = "www/gene_info_formatted.tsv", header = TRUE, sep = "\t",quote = "",stringsAsFactors = F)
gtex_data <- unique(read.table(file = "www/GTEx_gene_data.txt", header = TRUE, sep = "\t",stringsAsFactors = F))
gtex_data <- gtex_data[!duplicated(gtex_data$Description),]

load("www/db_admixture.RData")
load("www/db_pca.RData")

#### external non-session server functions and varibales ####
# 
# column_names <- c("Id","CHR","POS","rsID","REF","ALT","QUAL","Func","GENE","CONSEQ","X1000G_all",
#                   "ExAC_ALL","ExAC_NFE","HET_val","HOM_val","HET_rate","HOM_rate","MISS_rate",
#                   "INT_freq","AggAF_Trunc","AggAF_nsyn","SIFT","POLYP","POLYP_VAR","LRT",
#                   "MUT_TASTER","MUT_ASSESSOR","FATHMM","PROVEAN","CADD","TRANSCRIPT","EXON","DNA",
#                   "AA","CLINVAR","DISEASE","COSMIC_ID","COSMIC_COUNTS")

main_table <- c("CHR","POS","rsID","REF","ALT","GENE","CONSEQ","TRANSCRIPT","EXON","DNA",
                "AA","1Kg_all","ExAC_ALL","INT_freq","MISS_rate","SIFT","POLYP","CADD","MEAN_RD")

full_table <- c("CHR","POS","rsID","REF","ALT","QUAL","FILTER","ABHet","ABHom","AN","BaseQRankSum",
             "ExcessHet","HaplotypeScore","InbreedingCoeff","MQ","MQRankSum","ReadPosRankSum","FUNC",
             "GENE","CONSEQ","1Kg_all","ExAC_ALL","ExAC_AFR","ExAC_AMR","ExAC_EAS","ExAC_FIN",
             "ExAC_NFE","ExAC_OTH","ExAC_SAS","SIFT","POLYP","POLYP_VAR","LRT","MUT_TASTER","MUT_ASSESSOR",
             "FATHMM","PROVEAN","VEST3","METASVM","METALR","MCAP","CADD","DANN","FATHMMMKL","GENOCANYON",
             "INT_FITCONS","GERP","PHYLOP100","PHYLOP20","PHAST100","PHAST20","SIPHY29","INTERPRO",
             "COSMIC","HET_val","HOM_val","HET_rate","HOM_rate","MISS_rate","INT_freq","MEAN_RD",
             "TRANSCRIPT","EXON","DNA","AA","CLINVAR","DISEASE")

sitreinfo_cols <- c("rsID","REF","ALT","FILTER","FUNC","GENE","CONSEQ")

siteconseq_cols <- c("SIFT","POLYP","POLYP_VAR","LRT","MUT_TASTER","MUT_ASSESSOR","FATHMM",
                     "PROVEAN","CADD","VEST3","METASVM","METALR","MCAP","DANN","FATHMMMKL",
                     "GENOCANYON","INT_FITCONS","GERP","PHYLOP100","PHYLOP20","PHAST100",
                     "PHAST20","SIPHY29")

site_quals <- c("QUAL","ABHet","ABHom","BaseQRankSum","ExcessHet","HaplotypeScore",
                "InbreedingCoeff","MQ","MQRankSum","ReadPosRankSum","MEAN_RD")

siteclinc_cols <- c("CLINVAR","DISEASE")

siterarity_cols <- c("INT_freq","1Kg_all","ExAC_ALL","ExAC_NFE","ExAC_FIN","ExAC_AFR",
                     "ExAC_AMR","ExAC_EAS","ExAC_OTH","ExAC_SAS")

mut_types <- c("A>C","A>G","A>T","C>A","C>G","C>T","G>A","G>C","G>T","T>A","T>C","T>G")

transitions <- c("A>G","C>T","G>A","T>C")

transversions <- c("A>C","A>T","C>A","G>C","G>T","C>G","T>A","T>G")

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
for(i in 1:length(variant_data)){
var_counts_sample <- apply(variant_data[[i]][69:ncol(variant_data[[i]])],2,function(x) length(x[x == 1 | x == 2]))
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

      navbarPage(title = "Medical Genetics", collapsible = TRUE,position = "fixed-top",
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
                   column(6,style = "text-align: center",h4("Genes"),tags$b(formatC(length(unique(unlist(sapply(variant_data,function(x) unique(x$GENE))))),format="d", big.mark=",")))
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
                 column(4,style = "height: 350px",tags$div(id="plotPheno")),
                 column(8,
                        bs_accordion(id = "splash_info") %>%
                          bs_set_opts(panel_type = "default", use_heading_link = TRUE) %>%
                          bs_append(title = "Features", content = tags$div(HTML("
                  <p>Provided tools:</P>
                  <ul>
                  <li>Variant browser</li>
                  <li>Cohort Comparison</li>
                  <li>Population analysis - Not Implemented</li>
                  </ul>
                  <p>The Variant browser allows for interrogation of variants present individual cohorts. Data can be searched by:</P>
                  <ul>
                  <li>Chromosome position / rsID</li>
                  <li>Gene / Gene lists</li>
                  <li>Sample</li>
                  </ul>
                  "))) %>%
                          bs_append(title = "Data generation", content = tags$div(HTML("
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
         tags$hr(),
         div(id = "animation",
         conditionalPanel("input.cohort != 'None'",
          column(width = 12,
            h4("Data options"),
            tabsetPanel(id = "data_select",type = "tabs",
                  tabPanel(title = "Position",value = "data_select_pos",
                   wellPanel(
                    fluidRow(
                      column(12, textInput(inputId = "pos", label = "Position",placeholder = "Position or rsID"))
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
                         ),
                         h4("GTEx tissue expression data"),
                         tags$hr(),
                         fluidRow(tags$div(id="plotGtexData")),
                         h4(""),
                         tags$hr()
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
                          column(7,offset = 1,
                              fluidRow(column(8,h4("Quality metric summary")),
                                       column(3,style='margin-top:-17px',
                                                     selectInput(inputId = "qual_met",multiple = FALSE, label = "", selected = "QUAL", choices = site_quals)),
                                       column(width = 1,h4(""))
                              ),
                              fluidRow(column(width = 11,tags$div(id="plotSiteQual")),
                                       column(width = 1,h4(""))
                              )
                          )
                        ),
                        h4(""),
                        fluidRow(
                          column(12,
                                 h4("Damage prediction"),
                                 tags$div(id="plotSiteDamage",style = "height:200px;")
                            )
                        ),
                        column(8,
                          fluidRow(
                            column(12,
                                   h4("Genotype information")
                            )
                          ),
                          wellPanel(
                          fluidRow(
                            column(6,
                                     column(4,
                                            fluidRow(tags$b("Heterozygous Calls:")),
                                            fluidRow(tags$b("Homozygous Calls:")),
                                            fluidRow(tags$b("Missing rate Calls:"))
                                     ),
                                     column(4,offset = 4,
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
                                    )
                            )
                          ),
                          fluidRow(
                                  div(dataTableOutput(outputId = "site_sample_table"),style = "font-size: 85%; width: 100%")
                                    )
                          ),
                          tags$hr(),
                          fluidRow(
                            column(12,
                                   h4("Clinvar")
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
                                 div(dataTableOutput(outputId = "site_rarity_table"),style = "font-size: 70%; width: 100%")
                                 )
                         ),
                        fluidRow(h4(""))
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
#### Comparison tab ####
        tabPanel(title = "Comparison", icon = icon("random"),
         column(2,
                fluidRow(
                  column(12,h4("Select comparison type:"),
                         selectInput(inputId = "comp_analysis_type",label = NULL,choices = c("By Gene","By Position"),multiple = F,selected = "By Gene"),
                         h6(tags$em(""), style = "text-align: center; margin: 0px;")
                  )
                ),
                tags$hr(),
                div(id = "animation_comp_gene",conditionalPanel(condition = "input.comp_analysis_type == 'By Gene'",
                          fluidRow(
                             column(12,h4("Select Gene:"),selectizeInput(inputId = "compare_gene",label = NULL,choices = NULL),
                                       actionButton(inputId = "comp_gene_submit",label = "Search",icon = icon("search"),width = "100%")
                             )
                          )
                )),
                div(id = "animation_comp_pos",conditionalPanel(condition = "input.comp_analysis_type == 'By Position'",
                          fluidRow(
                             column(12,h4("Enter rsID or genomic position:"),textInput(inputId = "compare_pos",label = NULL,placeholder = "Position or rsID"),
                                       actionButton(inputId = "comp_pos_submit",label = "Search",icon = icon("search"),width = "100%")
                             )
                           )
                ))
         ),
         column(10,
                h6("Cohort statistics:"),
                column(width = 4, tags$div(id="plotCohortmut")),
                column(width = 4, tags$div(id="plotCohortTstv")),
                column(width = 4, tags$div(id="plotCohortConseq"))
         ),
         fluidRow(tags$hr()),
         tabsetPanel(id = "comp_tabs",
                     tabPanel("blank",value = "comp_blank_panel"),
                     tabPanel(title = "gene",value = "comp_gene_panel",
                              fluidRow(
                                column(12,
                                       h4("Search Results:")
                                )
                              ),
                              column(12,
                                     fluidRow(
                                       column(width = 12,withSpinner(uiOutput("comp_gene"),type = 4,color = "#95a5a6"),style = "font-size: 80%; width: 100%;")
                                     )
                              ),
                              fluidRow(
                                column(12,h4("")
                                )
                              ),
                              fluidRow(
                                column(4,h4("")),column(1,offset = 7,downloadButton(outputId = "comp_gene_dl",label = "Download"))
                              ),
                              fluidRow(
                                column(12,h4("")
                                )
                              )
                     ),
                     tabPanel(title = "pos", value = "comp_pos_panel",
                              fluidRow(
                                column(12,
                                       h4("Search Results:")
                                )
                              ),
                              column(12,
                                     fluidRow(
                                       column(width = 12,withSpinner(uiOutput("comp_pos"),type = 4,color = "#95a5a6"),style = "font-size: 80%; width: 100%;")
                                     )
                              ),
                              fluidRow(
                                column(12,h4("")
                                )
                              ),
                              fluidRow(
                                column(4,h4("")),column(1,offset = 7,downloadButton(outputId = "comp_pos_dl",label = "Download"))
                              ),
                              fluidRow(
                                column(12,h4("")
                                )
                              )    
                     )
          )    
        ),
#### Populations tab ####
        tabPanel(title = "Populations", icon = icon("globe"),
              fluidRow(
                 selectInput(inputId = "pop_select",label = "Select population",choices = names(variant_data),multiple = F)
              ),
              tags$hr(),
              fluidRow(
              column(2,h4("PCA analysis"))
              ),
              fluidRow(
              column(6,withSpinner(plotlyOutput("pca",height = "420px"),type = 4,color = "#95a5a6")),
              column(2,selectInput(inputId = "pca",
                                    label = "Component",
                                    choices = c("PC1 vs. PC2",
                                                "PC2 vs. PC3",
                                                "PC3 vs. PC4",
                                                "PC4 vs. PC5"),
                                    multiple = F,
                                    selected = "PC1 vs. PC2"),
                       tags$hr(),
                       tableOutput("pca_click"), style = "display: inline; align: center;")
              ),
              fluidRow(column(12,tags$hr())),
              h4("Admixture analysis"),
              fluidRow(
              tags$div(id="plotadmixJSON")
              )
        ),
#### Dropdown navbar section ####
        navbarMenu(title = "More", icon = icon("cogs"),
              tabPanel(title = "About",
              fluidRow(
                column(8,offset = 2,
                  h4(tags$b("The Medical Genetics Data Portal (MGDP) is a platform for the viewing and querying of genetic data sets generated from internal cohorts"))
                , style = "text-align: center;")
              ),
              fluidRow(
                column(8,offset = 2,tags$img(src = "./images/flow_chart.png", width = "100%", height = "100%"))
              ),
              h3("Data filtering"),
              h3("Supporting scripts"),
              code("./db_prep.sh --i /data/vcfs --anno annovar/file/path/anno --out"),
              h3("Data preparation"),
              h3("Software / Library requirements"),
              fluidRow(
                column(4,h4("Software dependecies")),column(4,h4("Web app libraries"))
              ),
              fluidRow(
                column(4,
                  tags$li(tags$a(href="https://vcftools.github.io/index.html","VCFtools")),
                  tags$li(tags$a(href="https://software.broadinstitute.org/gatk/","GATK")),
                  tags$li(tags$a(href="http://annovar.openbioinformatics.org/en/latest/","ANNOVAR")),
                  tags$li(tags$a(href="https://samtools.github.io/bcftools/bcftools.html","BCFtools")),
                  tags$li(tags$a(href="http://www.htslib.org/doc/tabix.html","Tabix")),
                  tags$li(tags$a(href="https://www.genetics.ucla.edu/software/admixture/","Admixture"))
                ),
                column(4,
                  tags$li(tags$a(href="","devtools")),
                  tags$li(tags$a(href="","shiny")),
                  tags$li(tags$a(href="","shinythemes")),
                  tags$li(tags$a(href="","jsonlite")),
                  tags$li(tags$a(href="","shinycssloaders")),
                  tags$li(tags$a(href="","shinywidgets")),
                  tags$li(tags$a(href="","shinyjs"))
                ),
                column(4,
                  tags$li(tags$a(href="","DT")),
                  tags$li(tags$a(href="","shinyalert")),
                  tags$li(tags$a(href="","htmltools")),
                  tags$li(tags$a(href="","bsplus")),
                  tags$li(tags$a(href="","plotly")),
                  tags$li(tags$a(href="","ggplot2")),
                  tags$li(tags$a(href="","reshape2")),
                  tags$li(tags$a(href="","stringr")),
                  tags$li(tags$a(href="","markdown"))
                )
              ),
              h5("The source code can be found",tags$a(href='https://github.com/Phil9S/variant_browser','here'))
              ),
              tabPanel(title = "FAQ",
                  fluidRow(h3("FAQ"))
              ),
              tabPanel(title = "Development",
                  h4("Known issues, bugs, & future features"),
                  column(12,
                         bs_accordion(id = "dev_info") %>%
                         bs_set_opts(panel_type = "primary", use_heading_link = TRUE) %>%
                         bs_append(title = "App issues",
                            content = tags$div(HTML("
                                      <ul>
                                      <li>External database links fail on single gene lookup if search return length = 0</li>
                                      <li>Plotly graph - selection of some sample points results in no info - ERROR:length of 'dimnames' [2] not equal to array extent</li>
                                      <li>Gene & Pos searching alternates between only rendering table and full page when same, reflitering or searching for a new gene</li>
                                      <li>Session occasionally hangs on empty table return - might be resolved?</li>
                                      <li>Gene fields with comma seperated entries may extend tables outside page bounderies if too long</li>
                                      <li>Site info row names out of order</li>
                                      <li>Tooltips for donught plots state data undefined as source</li>
                                      <li>Missing/empty value handling for plots - if no fs for exmaple, then data is misaligned in json - change to sum() not table()</li>
                                      <li>Filtering using 'including intronic' occasionally makes the sample-AF plot render below 0</li>
                                      <li>Transcript returns duplicated columns if empty or missing values on site-specific page</li>
                                      <li>Memory utilistion and minor memory leak on repeated loading</li>
                                      </ul>
                                      "))) %>%
                         bs_append(title = "Pre-processing issues",
                            content = tags$div(HTML("
                                      <ul>
                                      <li>1k genomes is incorrectly named as 'G1KG'</li>
                                      <li>Restore CADD to be numeric value with NA not N/a</li>
                                      <li>Add population data support by intergrating PCA and Admixture data</li>
                                      </ul>
                                      "))) %>%
                         bs_append(title = "Features in development",
                            content = tags$div(HTML("
                                      <ul>
                                      <li>Colour schemes inprovements</li>
                                      <li>No placeholders</li>
                                      <li>Links between tables - e.g. clickable rsIDs to site-specific page</li>
                                      <li>Statistical testing metrics - fishers/Burden/etc</li>
                                      <li>Data download buttons for all pages</li>
                                      <li>Tooltips and help icons for search fields, filters and table headers</li>
                                      <li>Oncoprint like plotting for sample sets</li>
                                      <li>Additional metrics for comparison page</li>
                                      <li>HET/HOM call flag for site-specifc page & sample page data table</li>
                                      <li>Support for comma-sv in clinvar table on site panel with multiple entries</li>
                                      </ul>
                                      ")))        
                         )
                  
              )
        )
      )
    )

#### UI END
#### SERVER START #### 
server = function(input, output, session){

#### TEMP MODEL

shinyalert(title = "<h4>MGDP - Alpha (v1.3)</h4>",
           type = "warning",
           text = "<h5>-----------------------------------------------------------</h5>
                   <h5><b>- </b>Web app is still in development</h5>
                   <h5><b>- </b>There will be weird bugs and errors</h5>
                   <h5><b>- </b>It will <b>definitely</b> break</h5>
                    <h5><b>- </b>Check 'Development' tab for known issues</b></h5>
                   <h5>-----------------------------------------------------------</h5>
                   <h5><b>Please inform me when, how, and what breaks!</b></h5>",
           html = T,
           closeOnEsc = T,
           closeOnClickOutside = T,
           confirmButtonText = "OK",
           confirmButtonCol = "#3182bd",
           animation = T,
           timer = 15000
           )
  
#### Json main page - cohort graphs ####
  
session$sendCustomMessage("cohortjson",var_cohort_json)
session$sendCustomMessage("agejson",var_age_json) 
session$sendCustomMessage("ethnojson",var_ethno_json)
session$sendCustomMessage("pheno_barjson",var_pheno_json)
session$sendCustomMessage("sexjson",var_sex_json)

#### Animations
observe({
    toggle(id = "animation", anim = TRUE, animType = "fade",
           time = 0.3, condition = input$cohort != "None")
})

observe({
  toggle(id = "animation_comp_gene", anim = TRUE, animType = "fade",
         time = 0.3, condition = input$comp_analysis_type == "By Gene")
})

observe({
  toggle(id = "animation_comp_pos", anim = TRUE, animType = "fade",
         time = 0.3, condition = input$comp_analysis_type == "By Position")
})

#### cohort selection & resetting ####
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
  session$reload()
  # shinyjs::reset("cohort")
  # updateTabsetPanel(session, "main_panel", selected = "blank_panel")
  # updateSelectInput(session, inputId = "site_sample_select", choices = NULL)
  # data_cord <- NULL
})

hide(id = "main_panel")
hide(id = "comp_tabs")

#### Panel switching & hiding ####

  observeEvent(input$but_pos, {if(input$main_panel != "pos_panel"){updateTabsetPanel(session, "main_panel", selected = "pos_panel")}})
  observeEvent(input$but_gene, {if(input$main_panel != "gene_panel"){updateTabsetPanel(session, "main_panel", selected = "gene_panel")}})
  observeEvent(input$but_sample, {if(input$main_panel != "sample_panel"){updateTabsetPanel(session, "main_panel", selected = "sample_panel")}})

#### COORDINATE SEARCH ####
  #addrsID search
  #Evaluate if the coordinate provided looks roughly like a genomic coordinate
  cord_label <- eventReactive(input$but_pos,{
    return(input$pos)
  })
  
  val_cord <- eventReactive(input$but_pos, {
    shiny::validate(
      need(grepl(pattern = "(^chr([XY]|[1-9]|1[0-9]|2[0-2])|([XY]|^[1-9]|^1[0-9]|^2[0-2])):[0-9]+(-[0-9]+)?|(^rs[0-9]+)", input$pos),
           message = "Please enter a valid chromosome position or rsID")
          ## message passed if it looks weird or malformed e.g. contains letters
    )
    ##passes input value if no problem
    return(input$pos)
  })
  data_cord <- eventReactive(input$but_pos, {
      
      datC <- variant_data[[which(names(variant_data) == input$cohort)]]
      
      if(grepl("^rs", val_cord()) == TRUE){
          datC <- datC[datC$rsID == val_cord(),]
      } else {
          ##split the inputpos into its chr start and stop components
          chr <- strsplit(val_cord(), ":",fixed = TRUE)[[1]][1]
          start1 <- strsplit(strsplit(val_cord(), ":",fixed = TRUE)[[1]][2], "-", fixed = TRUE)[[1]][1]
          stop1 <- strsplit(strsplit(val_cord(), ":",fixed = TRUE)[[1]][2], "-", fixed = TRUE)[[1]][2]
          ##if only start was provided - look for specific coord - if both start and stop - look up range of variants - start should be less than stop
          
          if(is.na(stop1)){
            datC <- datC[datC$CHR == chr & datC$POS == start1,]
            ##report data back to function2:1-9999999999999999
          } else {
            datC <- datC[datC$CHR == chr 
                  & as.numeric(datC$POS) >= as.numeric(start1)
                  & as.numeric(datC$POS) <= as.numeric(stop1),]
          }
      }
      datC <- datC[datC$`1Kg_all` <= input$X1000G_rarity_pos & datC$ExAC_ALL <= input$Exac_rarity_pos,]
      if(nrow(datC) == 1){
        ## Panel switching to site panel
        updateTabsetPanel(session, "main_panel", selected = "site_panel")
        updateSelectInput(session, inputId = "site_transcript_select", choices = unlist(str_split(datC$TRANSCRIPT,",")))
        # datC <- datC[colnames(datC) %in% full_table]
        # datC <- datC[full_table]
        return(datC)
      } else {
        
        ########### INTRONIC   
        if(input$intron_cord_pos == FALSE){
        datC <- datC[which(datC$CONSEQ != "intronic"),]
        }
      
        if(input$syn_cord_pos == FALSE){
          datC <- datC[which(datC$CONSEQ != "synonymous"),]
        }
        if(input$trunc_cord_pos == TRUE){
          datC <- datC[datC$CONSEQ == "stopgain" 
                       | datC$CONSEQ == "frameshift deletion"
                       | datC$CONSEQ == "frameshift insertion"
                       | datC$CONSEQ == "stoploss",]
        }
        if(input$splice_cord_pos == TRUE){
          datC <- datC[datC$CONSEQ == "splicing",]
        }
      ##report table to render function
      datC <- datC[colnames(datC) %in% main_table]
      datC <- datC[main_table]
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
  
  geneIn <- eventReactive(input$but_gene, {
      gene_list <- unique(unlist(strsplit(input$gene,",",fixed = TRUE)))
      gene_list <- toupper(gene_list)
      return(gene_list)
  })
  
  val_gene <- eventReactive(input$but_gene, {
      gene_list <- geneIn()
      datG <- variant_data[[which(names(variant_data) == input$cohort)]]
      if(length(gene_list) >= 2){
        no_list <- gene_list[!gene_list %in% datG$GENE]
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
        gene_list <- gene_list[gene_list %in% datG$GENE]
        datG <- datG[datG$GENE %in% gene_list,]
      } else {
        datG <- datG[datG$GENE == gene_list,]
        if(input$main_panel != "single_gene_panel"){updateTabsetPanel(session, "main_panel", selected = "single_gene_panel")}
      }
      
      datG <- datG[datG$`1Kg_all` <= input$X1000G_rarity_gene & datG$ExAC_ALL <= input$Exac_rarity_gene,]
      ########### INTRONIC   
      if(input$intron_cord_gene == FALSE){
        datG <- datG[which(datG$CONSEQ != "intronic"),]
      }
      
      if(input$syn_cord_gene == FALSE){
        datG <- datG[datG$CONSEQ != "synonymous",]
      }
      if(input$trunc_cord_gene == TRUE){
        datG <- datG[datG$CONSEQ == "stopgain" 
                     | datG$CONSEQ == "frameshift deletion"
                     | datG$CONSEQ == "frameshift insertion"
                     | datG$CONSEQ == "stoploss",]
      }
      if(input$splice_cord_gene == TRUE){
        datG <- datG[datG$CONSEQ == "splicing",]
      }
      ##report table to render function
      datG <- datG[colnames(datG) %in% main_table]
      datG <- datG[main_table]
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
    
    datS <- datS[datS$`1Kg_all` <= input$X1000G_rarity_sample & datS$ExAC_ALL <= input$Exac_rarity_sample,]
    ########### INTRONIC   
    if(input$intron_cord_sample == FALSE){
      datS <- datS[which(datS$CONSEQ != "intronic"),]
    }
    
    if(input$syn_cord_sample == FALSE){
      datS <- datS[datS$CONSEQ != "synonymous",]
    }
    if(input$trunc_cord_sample == TRUE){
      datS <- datS[datS$CONSEQ == "stopgain" 
                   | datS$CONSEQ == "frameshift deletion"
                   | datS$CONSEQ == "frameshift insertion"
                   | datS$CONSEQ == "stoploss",]
    }
    if(input$splice_cord_sample == TRUE){
      datS <- datS[datS$CONSEQ == "splicing",]
    }
  
    ##report table to render function
    datS <- datS[colnames(datS) %in% main_table]
    datS <- datS[main_table]
    ##report table to render function
    return(datS)
  })

#### Data table outputs ####
  
output$table_cord <- renderDataTable({
  if(nrow(data_cord()) == 0){
    datatable(data_cord(),rownames = FALSE,
              options = list(
                pageLength = 10,
                lengthMenu = c(10, 20, 50, 100),
                searchHighlight = TRUE))
  } else {
    datatable(data_cord(),rownames = FALSE,
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
    # formatStyle('CADD',
    #             color = styleInterval(seq.int(1,max(data_cord()$CADD,na.rm = TRUE),1),colfuncCADD(max(seq.int(1,max(data_cord()$CADD,na.rm = TRUE)+1,1)))),
    #             fontWeight = 'bold'
    # ) %>% 
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
  }
})

output$table_gene <- renderDataTable({
  if(nrow(val_gene()) == 0){
    datatable(val_gene(),rownames = FALSE,
              options = list(pageLength = 10,
                             lengthMenu = c(10, 20, 50, 100),
                             searchHighlight = TRUE))
  } else {
    datatable(val_gene(),rownames = FALSE,
              options = list(pageLength = 10,
                             lengthMenu = c(10, 20, 50, 100),
                             searchHighlight = TRUE,
                             columnDefs = list(list(
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
    # formatStyle('CADD',
    #             color = styleInterval(seq.int(1,max(val_gene()$CADD,na.rm = TRUE),1),colfuncCADD(max(seq.int(1,max(val_gene()$CADD,na.rm = TRUE)+1,1)))),
    #             fontWeight = 'bold'
    # ) %>% 
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
  }  
})                                               

output$table_sample <- renderDataTable({
  if(nrow(data_sample()) == 0){
    datatable(data_sample(),rownames = FALSE,
              options = list(pageLength = 10,
                             lengthMenu = c(10, 20, 50, 100),
                             searchHighlight = TRUE))
  } else {
    datatable(data_sample(),
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
      # formatStyle('CADD',
      #             color = styleInterval(seq.int(1,max(data_sample()$CADD,na.rm = TRUE),1),colfuncCADD(max(seq.int(1,max(data_sample()$CADD,na.rm = TRUE)+1,1)))),
      #             fontWeight = 'bold'
      # ) %>% 
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
  }
}) 

#### Single gene page ####
output$table_single_gene <- renderDataTable({
  if(nrow(val_gene()) == 0){
    datatable(val_gene(),rownames = FALSE,
              options = list(pageLength = 10,
                             lengthMenu = c(10, 20, 50, 100),
                             searchHighlight = TRUE))
  } else {
    
    datatable(val_gene(),
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
                  color = styleEqual(sort(unique(val_gene()$CADD)),colfuncCADD(length(sort(unique(val_gene()$CADD))))),
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
  }  
})
##input$gene not effective fix - results in auto updating on runtime
output$single_gene_panel_ext <- renderDataTable({
    datatable(data.frame(Database=c("Genecards","Ensembl","HGNC","NCBI","Uniprot","OMIM"),
                                                                         Link=gene_links(geneIn())),
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
                                                                       Info=as.character(gene_info[gene_info$Gene.name == geneIn(),][c(4,5,6,2,3,7)])),
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

#### Site specific page rendering ####
output$site_sample_table <- renderDataTable({
  updateSelectInput(session,
                    inputId = "site_sample_select",
                    choices = as.character(sample_data$SAMPLE_ID[sample_data$DATA_ID %in% names(data_cord()[69:ncol(data_cord())][which(data_cord()[69:ncol(data_cord())] == 1 | data_cord()[69:ncol(data_cord())] == 2)])])
  )
  datatable(as.data.frame(sample_data[sample_data$SAMPLE_ID == input$site_sample_select,][c(3,1,2,6:8)]),
            class = 'compact',
            selection = 'none',
            rownames = FALSE,
            escape = FALSE,
            options = list(dom = 't',ordering=F)
  )
})

output$site_transcript_table <- renderDataTable({
  ## Transcript table
  transcripts <- data_cord()[names(data_cord()) %in% c("TRANSCRIPT","EXON","DNA","AA")]
  
  if(length(str_split(transcripts$TRANSCRIPT,pattern = ",",simplify = T)) > 1){
    transcripts <- as.data.frame(apply(transcripts,2,function(x) str_split(x,pattern = ",",simplify = T)))
  }
  datatable(transcripts[transcripts$TRANSCRIPT == input$site_transcript_select,],
            class = 'compact',
            selection = 'none',
            rownames = FALSE,
            escape = FALSE,
            options = list(dom = 't',ordering=F)
  )
})

output$site_rarity_table <- renderDataTable({
  index_AF <- names(data_cord()[names(data_cord()) %in% siterarity_cols])
  site_AF <- t(data_cord()[names(data_cord()) %in% siterarity_cols])
  site_AF <- data.frame(index_AF,site_AF)
  colnames(site_AF) <- c("Dataset","AF")
  datatable(site_AF,
            selection = 'none',
            rownames = FALSE,
            escape = FALSE,
            options = list(dom = 't',ordering=F)
  )
})

output$site_title <- renderText({paste(paste(gsub(pattern = "chr",x = data_cord()[2],replacement = ""),":",data_cord()[3],sep = ""),data_cord()[5], "/", data_cord()[6]," - ",data_cord()[4])})
output$site_hetv <- renderText({paste(data_cord()$HET_val," (",data_cord()$HET_rate,"%)",sep = "")})
output$site_homv <- renderText({paste(data_cord()$HOM_val," (",data_cord()$HOM_rate,"%)",sep = "")})
output$site_miss <- renderText({paste(data_cord()$MISS_rate,"%")})

output$site_info <- renderDataTable({
  datatable(data.frame(Names=names(data_cord()[names(data_cord()) %in% sitreinfo_cols]),values=as.character(data_cord()[which(names(data_cord()) %in% sitreinfo_cols)])),
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

observe({
  if(input$main_panel == "site_panel"){
      ## pathogenicity plotting - per site
      sitedmg_melt <- t(reshape2::melt(data_cord()[which(names(data_cord()) %in% siteconseq_cols)]))
      siteDamageJSON <- cbind(c("Consequence","value"),sitedmg_melt)
      rownames(siteDamageJSON) <- NULL
      siteDamageJSON <- toJSON(siteDamageJSON, dataframe = 'values')
      session$sendCustomMessage("sitedmgjson",siteDamageJSON)
  }
})

observe({
  if(input$main_panel == "site_panel"){      
      # Quality metric plotting
      qual_hist <- variant_data[[which(names(variant_data) == input$cohort)]][site_quals]
      qual_hist <- qual_hist[,which(colnames(qual_hist) == input$qual_met)]
      qual_hist <- hist(qual_hist,breaks = seq.int(from = ifelse(min(qual_hist,na.rm = T) < 0,min(qual_hist,na.rm = T),0),to = max(qual_hist,na.rm = T),length.out = 51),plot = F)
      qual_hist <- cbind(c("Bin","Count"),rbind(paste(signif(qual_hist$breaks[-length(qual_hist$breaks)],digits = 2)),qual_hist$counts))
      
      qual_line <- data_cord()[,which(names(data_cord()) == input$qual_met)]
      siteQualJSON <- toJSON(list(qual_hist,qual_line), dataframe = 'values')
      
      session$sendCustomMessage("sitequaljson",siteQualJSON)
  }
})      

output$site_clinc <- renderDataTable({
  if(is.na(data_cord()$CLINVAR)){
    datatable(data.frame(CLINVAR=c("no data"),DISEASE=c("no data")),
              class = 'compact',
              selection = 'none',
              rownames = FALSE,
              escape = FALSE,
              options = list(dom = 't',ordering=F)
    )
  } else {
        datatable(data_cord()[which(names(data_cord()) %in% siteclinc_cols)],
                  class = 'compact',
                  selection = 'none',
                  rownames = FALSE,
                  escape = FALSE,
                  options = list(dom = 't',ordering=F)
        )
  }
})

#})                                                 

#### Plot rendering ####

## SamplePage - Variant plots
observe({
  if(nrow(data_sample()) == 0){
    SampleVarJSON <- data.frame(conseq=c("frameshift deletion","frameshift insertion","nonframeshift deletion","nonframeshift insertion","nonsynonymous","splicing","stopgain","stoploss","synonymous","unknown"),total=rep.int(0,10))
    SampleVarJSON <- toJSON(SampleVarJSON)
  } else {
    SampleVarJSON <- as.data.frame(table(data_sample()$CONSEQ))
    colnames(SampleVarJSON) <- c("conseq","total")
    SampleVarJSON <- toJSON(SampleVarJSON)
  }
  session$sendCustomMessage("SampleVarjson",SampleVarJSON)
})

observe({
  if(nrow(data_sample()) == 0){
    sampleMut_plotJSON <- data.frame(mutation=mut_types,total=rep(0,12))
    sampleMut_plotJSON <- toJSON(sampleMut_plotJSON)
  } else {
    sampleMut_plotJSON <- as.data.frame(table(paste(data_sample()$REF,data_sample()$ALT,sep = ">")))
    sampleMut_plotJSON <- sampleMut_plotJSON[sampleMut_plotJSON$Var1 %in% mut_types,]
    colnames(sampleMut_plotJSON) <- c("mutation","total")
    sampleMut_plotJSON <- toJSON(sampleMut_plotJSON)
  }
  session$sendCustomMessage("SampleMutjson",sampleMut_plotJSON)
})

observe({
  if(nrow(data_sample()) == 0){
    K <- c('1KG',rep.int(0,5))
    E <- c('ExAC',rep.int(0,5))
    I <- c('Internal',rep.int(0,5))
    sampleRare_plotJSON <- toJSON(rbind(K,E,I))
  } else {
    K <- as.data.frame(table(cut(data_sample()$`1Kg_all`,seq.int(0,0.05,0.01),include.lowest = T)))
    K <- c('1KG',as.numeric(K$Freq))
    E <- as.data.frame(table(cut(data_sample()$ExAC_ALL,seq.int(0,0.05,0.01),include.lowest = T)))
    E <- c('ExAC',as.numeric(E$Freq))
    I <- as.data.frame(table(cut(data_sample()$INT_freq,seq.int(0,0.05,0.01),include.lowest = T)))
    I <- c('Internal',as.numeric(I$Freq))
    sampleRare_plotJSON <- toJSON(rbind(K,E,I))
  }
  session$sendCustomMessage("SampleRarejson",sampleRare_plotJSON)
})
## Single gene page GTEx
observe({
    gtex_filt <- gtex_data[gtex_data$Description == geneIn(),]
    gtex_filt <- gtex_filt[,-1]
    gtex_filt <- rbind(colnames(gtex_filt),gtex_filt[1,])
    rownames(gtex_filt) <- NULL
    colnames(gtex_filt) <- NULL
    gtex_filt <- cbind(c("Tissue","TPM"),gtex_filt)
    gtex_filt[1,-1] <- gsub(pattern = "\\.\\..",replacement = " ",gtex_filt[1,-1])
    gtex_filt[1,-1] <- gsub(pattern = "Brain ",replacement = "",gtex_filt[1,-1])
    gtex_filt[1,-1] <- gsub(pattern = "Cells ",replacement = "",gtex_filt[1,-1])
    gtex_filt[1,-1] <- gsub(pattern = "\\.",replacement = " ",gtex_filt[1,-1])
    gtex_JSON <- toJSON(gtex_filt, dataframe = 'values')
    session$sendCustomMessage("GtexPlot",gtex_JSON)
})
#### Text rendering #### 

output$id_cord <- renderText({paste("Search results for ", cord_label())})
output$id_sample <- renderText({paste(sample_data$SAMPLE_ID[sample_data$DATA_ID == val_sample()])}) ##prints header for table with search value
output$id_gene <- renderText({label_gene()}) 
output$sampleid_table <- renderText({paste("Non-reference samples in ",sample_data$SAMPLE_ID[sample_data$DATA_ID == val_sample()])})

output$single_gene_panel_title <- renderText({geneIn()})
output$single_gene_panel_desc <- renderText({as.character(gene_info$Gene.description[gene_info$Gene.name == geneIn()])})

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

#### Comparison server functions ####

  ##Tab switch
  observeEvent(input$comp_gene_submit, {if(input$comp_tabs != "comp_gene_panel"){updateTabsetPanel(session, "comp_tabs", selected = "comp_gene_panel")}})
  observeEvent(input$comp_pos_submit, {if(input$comp_tabs != "comp_pos_panel"){updateTabsetPanel(session, "comp_tabs", selected = "comp_pos_panel")}})
  
  ## Gene comparison 
  max_table = length(variant_data)
  
  sub_data_gene <- eventReactive(input$comp_gene_submit,{
    subset <- lapply(variant_data, function(x){
      sub <- x[x$GENE == input$compare_gene,]
      sub <- sub[colnames(sub) %in% main_table]
      sub <- sub[main_table]
      sub <- sub[!colnames(sub) %in% siteconseq_cols]
      return(sub)
    })
    return(subset)
  })
  
  observe({
    updateSelectizeInput(session, 'compare_gene', 
                         choices = sort(unique(unlist(lapply(variant_data,function(x) unique(x$GENE))))), 
                         server = TRUE,
                         options = list(placeholder = 'Gene ID'))
    
    output$comp_gene <- renderUI({
      table_output_list <- lapply(1:max_table, function(i) {
        tablename <- paste("tablename", i, sep = "")
        dataTableOutput(tablename)
      })
      title_output_list <- lapply(1:max_table, function(i) {
        tabletitles <- paste("titlename",i,sep = "")
        h4(textOutput(tabletitles))
      })
      elements <- do.call(tagList, c(table_output_list,title_output_list))
      elements[c(rbind(seq.int(1+max_table,max_table*2,1),seq.int(1,max_table,1)))]
    })
    
    for (i in 1:max_table) {
      local({
        my_i <- i
        tablename <- paste("tablename", my_i, sep = "")
        output[[tablename]] <- renderDataTable({
          datatable(sub_data_gene()[[my_i]],rownames = FALSE,
                    options = list(
                      dom = 'tp',
                      pageLength = 5,
                      lengthMenu = c(10, 20, 50, 100),
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
                      ),
                      searchHighlight = TRUE))
        })
      })
    }
    
    for (i in 1:max_table) {
      local({
        my_i <- i
        titlename <- paste("titlename", my_i, sep = "")
        output[[titlename]] <- renderText({
          names(sub_data_gene())[my_i]
          
        })
      })
    }
  })
  
  ## Position comparison
  
  val_sub_cord <- eventReactive(input$comp_pos_submit, {
    shiny::validate(
      need(grepl(pattern = "(^chr([XY]|[1-9]|1[0-9]|2[0-2])|([XY]|^[1-9]|^1[0-9]|^2[0-2])):[0-9]+|(^rs[0-9]+)", input$compare_pos),
           message = "Please enter a valid chromosome position")
      ## message passed if it looks weird or malformed e.g. contains letters
    )
    ##passes input value if no problem
    return(input$compare_pos)
  })
  
  sub_data_pos <- eventReactive(input$comp_pos_submit,{
    subset <- lapply(variant_data, function(x){
      
      if(grepl("^rs", val_sub_cord()) == TRUE){
        id <- val_sub_cord()
        sub <- x[which(x$rsID == id),]
      } else {
        ##split the inputpos into its chr start and stop components
        chr <- strsplit(val_sub_cord(), ":",fixed = TRUE)[[1]][1]
        start1 <- strsplit(val_sub_cord(), ":",fixed = TRUE)[[1]][2]
        ##if only start was provided - look for specific coord - if both start and stop - look up range of variants - start should be less than stop
        sub <- x[x$CHR == chr & x$POS == start1,]
        ##report data back to function2:1-9999999999999999
      }
      sub <- sub[colnames(sub) %in% main_table]
      sub <- sub[main_table]
      sub <- sub[!colnames(sub) %in% siteconseq_cols]
      return(sub)
    })
    return(subset)
  })
  
  observe({
    
    output$comp_pos <- renderUI({
      table_output_list <- lapply(1:max_table, function(i) {
        tablename <- paste("tablenameP", i, sep = "")
        dataTableOutput(tablename)
      })
      title_output_list <- lapply(1:max_table, function(i) {
        tabletitles <- paste("titlenameP",i,sep = "")
        h4(textOutput(tabletitles))
      })
      elements <- do.call(tagList, c(table_output_list,title_output_list))
      elements[c(rbind(seq.int(1+max_table,max_table*2,1),seq.int(1,max_table,1)))]
    })
    
    for (i in 1:max_table) {
      local({
        my_i <- i
        tablename <- paste("tablenameP", my_i, sep = "")
        output[[tablename]] <- renderDataTable({
          datatable(sub_data_pos()[[my_i]],rownames = FALSE,
                    options = list(
                      dom = 'tp',
                      pageLength = 5,
                      lengthMenu = c(10, 20, 50, 100),
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
                      ),
                      searchHighlight = TRUE))
        })
      })
    }
    
    for (i in 1:max_table) {
      local({
        my_i <- i
        titlename <- paste("titlenameP", my_i, sep = "")
        output[[titlename]] <- renderText({
          names(sub_data_pos())[my_i]
          
        })
      })
    }
  })
  
  ##Saving comparison results
  output$comp_gene_dl <- downloadHandler(filename = function(){
              paste("mgvb_gene_comparison_",Sys.Date(),"_",input$compare_gene,".txt",sep = "")
    },
    content = function(file){
      data <- do.call(rbind,lapply(seq_len(length(sub_data_gene())),function(x){
        cbind(Cohort=rep(names(sub_data_gene())[x],times=nrow(sub_data_gene()[[x]])),sub_data_gene()[[x]])
      }))
      write.table(data,file,sep = "\t",quote = F,col.names = T,row.names = F)
    })
  
  output$comp_pos_dl <- downloadHandler(filename = function(){
    paste("mgvb_pos_comparison_",Sys.Date(),".txt",sep = "")
  },
  content = function(file){
    data <- do.call(rbind,lapply(seq_len(length(sub_data_pos())),function(x){
      cbind(Cohort=rep(names(sub_data_pos())[x],times=nrow(sub_data_pos()[[x]])),sub_data_pos()[[x]])
    }))
    write.table(data,file,sep = "\t",quote = F,col.names = T,row.names = F)
  })

#### Cohort summary plots ####
  observe({
    cohort_mutations_JSON <- do.call(rbind,lapply(variant_data,function(x){
      j <- as.data.frame(table(paste(x$REF,x$ALT,sep = ">")))
      j <- j[j$Var1 %in% mut_types,]
      j <- t(signif(j[2]/length(names(x[69:ncol(x)])),digits = 2))
    }))
    cohort_mutations_JSON <- as.data.frame(cohort_mutations_JSON,row.names = length(cohort_mutations_JSON))
    cohort_mutations_JSON <- rbind(as.character(mut_types),cohort_mutations_JSON)
    cohort_mutations_JSON <- cbind(c("x",names(variant_data)),cohort_mutations_JSON)
    cohort_mutations_JSON <- toJSON(cohort_mutations_JSON,dataframe = 'values')
    
    session$sendCustomMessage("CohortMutjson",cohort_mutations_JSON)
  })
  
  observe({
    cohort_tstv_JSON <- do.call(rbind,lapply(variant_data, function(x){
      tstv <- numeric()
      for(i in seq.int(30,120,10)){
        a <- as.data.frame(table(paste(x$REF[x$QUAL >= i],x$ALT[x$QUAL >= i],sep = ">")))
        a <- a[a$Var1 %in% mut_types,]
        tstv <- append(tstv,signif(sum(a$Freq[a$Var1 %in% transitions]) / sum(a$Freq[a$Var1 %in% transversions]),digits = 3))
      }
      return(tstv)
    }))
    cohort_tstv_JSON <- as.data.frame(cohort_tstv_JSON,row.names = length(cohort_tstv_JSON))
    cohort_tstv_JSON <- rbind(seq.int(30,120,10),cohort_tstv_JSON)
    cohort_tstv_JSON <- cbind(c("x",names(variant_data)),cohort_tstv_JSON)
    cohort_tstv_JSON <- toJSON(cohort_tstv_JSON,dataframe = 'values')

    session$sendCustomMessage("CohortTsTvjson",cohort_tstv_JSON)
  })
  
  observe({
    cohort_conseq_JSON <- do.call(rbind,lapply(variant_data, function(x){
      int <- signif(length(x$CONSEQ[x$CONSEQ == "intronic"])/ ncol(x[69:ncol(x)]),digits = 2)
      syn <- signif(length(x$CONSEQ[x$CONSEQ == "synonymous"])/ ncol(x[69:ncol(x)]),digits = 2)
      nsyn <- signif(length(x$CONSEQ[x$CONSEQ == "nonsynonymous"])/ ncol(x[69:ncol(x)]),digits = 2)
      trunc <- signif(length(x$CONSEQ[x$CONSEQ %in% c("stopgain","FS deletion","FS insertion")])/ ncol(x[69:ncol(x)]),digits = 2)
      splc <- signif(length(x$CONSEQ[x$CONSEQ == "splicing"])/ ncol(x[69:ncol(x)]),digits = 2)
      c(splc,trunc,nsyn,syn,int)
    }))
    cohort_conseq_JSON <- as.data.frame(cohort_conseq_JSON,row.names = length(cohort_conseq_JSON))
    cohort_conseq_JSON <- rbind(c("Splicing","Truncations","Nonsynonymous","Synonymous","Intronic"),cohort_conseq_JSON)
    cohort_conseq_JSON <- cbind(c("x",names(variant_data)),cohort_conseq_JSON)
    cohort_conseq_JSON <- toJSON(cohort_conseq_JSON,dataframe = 'values')
    
    session$sendCustomMessage("CohortConseqjson",cohort_conseq_JSON)
  })
#### Population functions ####
  observe({
    admixDAT <- as.data.frame(admixture[names(admixture) == input$pop_select],row.names = NULL,stringsAsFactors = F)
    rownames(admixDAT) <- NULL
    colnames(admixDAT) <- NULL
    admixJSON <- toJSON(admixDAT,dataframe = 'values')
    session$sendCustomMessage("admixJSON",admixJSON)
  })
  
  plotly_out <- reactive({
        pca <- as.data.frame(pca_data[names(pca_data) == input$pop_select])
        colnames(pca) <- c("sample","PC1","PC2","PC3","PC4","PC5","Super_population")
        pca[,c(2:6)] <- apply(pca[,c(2:6)],2,function(x) (x-min(x))/(max(x)-min(x))+0.05)
        pca$Super_population <- factor(pca$Super_population,levels = c("Sample","SAS","EUR","EAS","AFR","AMR"))
   
        if(input$pca == "PC1 vs. PC2"){
            pca <- pca[colnames(pca) %in% c("sample","PC1","PC2","Super_population")]
        } else if(input$pca == "PC2 vs. PC3"){
            pca <- pca[colnames(pca) %in% c("sample","PC2","PC3","Super_population")]
        } else if(input$pca == "PC3 vs. PC4"){
            pca <- pca[colnames(pca) %in% c("sample","PC3","PC4","Super_population")]
        } else if(input$pca == "PC4 vs. PC5"){
            pca <- pca[colnames(pca) %in% c("sample","PC4","PC5","Super_population")]
        }

    P <- plot_ly(data = pca[pca$Super_population == "SAS",],
                 type = "scatter",
                 mode = "markers",
                 name = "SAS",
                 opacity = 0.4,
                 text = pca$sample[pca$Super_population == "SAS"],
                 hoverinfo = "text",
                 x = pca[pca$Super_population == "SAS",][,2],
                 y = pca[pca$Super_population == "SAS",][,3]) %>%
      add_trace(data = pca[pca$Super_population == "EUR",],
                type = "scatter",
                mode = "markers",
                name = "EUR",
                opacity = 0.4,
                text = pca$sample[pca$Super_population == "EUR"],
                hoverinfo = "text",
                x = pca[pca$Super_population == "EUR",][,2],
                y = pca[pca$Super_population == "EUR",][,3]) %>%
      add_trace(data = pca[pca$Super_population == "EAS",],
                type = "scatter",
                mode = "markers",
                name = "EAS",
                opacity = 0.4,
                text = pca$sample[pca$Super_population == "EAS"],
                hoverinfo = "text",
                x = pca[pca$Super_population == "EAS",][,2],
                y = pca[pca$Super_population == "EAS",][,3]) %>%
      add_trace(data = pca[pca$Super_population == "AFR",],
                type = "scatter",
                mode = "markers",
                name = "AFR",
                opacity = 0.4,
                text = pca$sample[pca$Super_population == "AFR"],
                hoverinfo = "text",
                x = pca[pca$Super_population == "AFR",][,2],
                y = pca[pca$Super_population == "AFR",][,3]) %>%
      add_trace(data = pca[pca$Super_population == "AMR",],
                type = "scatter",
                mode = "markers",
                name = "AMR",
                opacity = 0.4,
                text = pca$sample[pca$Super_population == "AMR"],
                hoverinfo = "text",
                x = pca[pca$Super_population == "AMR",][,2],
                y = pca[pca$Super_population == "AMR",][,3]) %>%
      add_trace(data = pca[pca$Super_population == "Sample",],
                type = "scatter",
                mode = "markers",
                name = "Sample",
                opacity = 1,
                text = pca$sample[pca$Super_population == "Sample"],
                hoverinfo = "text",
                x = pca[pca$Super_population == "Sample",][,2],
                y = pca[pca$Super_population == "Sample",][,3]) %>%
      layout(legend = list(orientation = 'h',xanchor = "center",x = 0.5)) %>%
      config(displaylogo = FALSE, collaborate = FALSE, cloud = FALSE,
             modeBarButtonsToRemove = list(
               'lasso2d',
               'hoverClosestCartesian',
               'hoverCompareCartesian',
               'toggleSpikelines',
               'zoom2d',
               'pan2d',
               'select2d',
               'autoScale2d',
               'lasso2d'
             ))
    return(P)
    })

  output$pca <- renderPlotly(plotly_out())
  
  output$pca_click <- renderTable({
    s <- event_data("plotly_click")
    x <- s$x
    y <- s$y
    if (length(s) == 0) {
      
      return(sprintf("Select a point in the PCA to display sample information"))
    } else {
      if(input$pca == "PC1 vs. PC2"){
        pca <- pca[colnames(pca) %in% c("sample","PC1","PC2","Super_population")]
      } else if(input$pca == "PC2 vs. PC3"){
        pca <- pca[colnames(pca) %in% c("sample","PC2","PC3","Super_population")]
      } else if(input$pca == "PC3 vs. PC4"){
        pca <- pca[colnames(pca) %in% c("sample","PC3","PC4","Super_population")]
      } else if(input$pca == "PC4 vs. PC5"){
        pca <- pca[colnames(pca) %in% c("sample","PC4","PC5","Super_population")]
      }

      sample <- pca$sample[round(pca[,2],digits = 6) == round(x,digits = 6) & round(pca[,3],digits = 6) == round(y,digits = 6)]
      sample_col <- t(sample_data[sample_data$SAMPLE_ID == sample,][c(1,4:ncol(sample_data))])
      colnames(sample_col) <- sample
      sample_table <- cbind(Information=c("Cohort","Age","Population","Phenotype","Sex","Status"),sample_col)
    }
  })
#### Server END
}
## App call
shinyApp(ui = ui, server = server)

# p <- ggplot() +
#         geom_point(data = pca,aes(x = pca[,2], y = pca[,3],col=Super_population),stroke = 0,alpha = 0.2,size = 1) +
#         theme(axis.line = element_line(colour = "grey15",size = 0.5),panel.grid.major = element_line(colour = "grey90")) +
#         theme(panel.grid.minor = element_line(colour = "grey90"),panel.background = element_rect(fill=NA)) +
#         theme(axis.text = element_blank(),axis.ticks = element_blank(),legend.key = element_blank(),legend.key.size = ) +
#         scale_x_continuous(expand = c(0,2)) + scale_y_continuous(expand = c(0,2)) +
#         scale_color_manual(name = "Population",values=c("grey15", "#e31a1c", "#1f78b4", "#33a02c", "#6a3d9a", "#ff7f00")) +
#         labs(x = colnames(pca[2]),y = colnames(pca[3])) +
#         guides(colour = guide_legend(override.aes = list(size=2,alpha = 1))) +
#       annotate("point",col = "grey15",size = 1,
#                pca[,2][pca$Super_population == "Sample"],
#                pca[,3][pca$Super_population == "Sample"])
# 
# ggplotly(p)