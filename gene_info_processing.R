library(dplyr)
library(stringr)

data <- read.table("www/gene_info.txt",sep = "\t",header = T,stringsAsFactors = F,na.strings = "",quote = "")
data.col <- data.frame(data %>% group_by(Gene.name) %>% summarise_all(funs(paste(unique(.),collapse = ","))),stringsAsFactors = F)

data.col <- data.col[-c(7,9)]

data.col <- data.col[data.col$Karyotype.band != "NA",]
data.col <- as.data.frame(apply(data.col,2,function(x) gsub(",NA","",x)),stringsAsFactors = F)
data.col <- as.data.frame(apply(data.col,2,function(x) gsub("NA,","",x)),stringsAsFactors = F)

data.col$Gene.type[data.col$Gene.type == "IG_C_gene"] <- "protein_coding"
data.col$Gene.type[data.col$Gene.type == "unprocessed_pseudogene,protein_coding"] <- "protein_coding"
data.col$Gene.type[data.col$Gene.type == "unprocessed_pseudogene,polymorphic_pseudogene"] <- "unprocessed_pseudogene"
data.col$Gene.type[data.col$Gene.type == "unitary_pseudogene,transcribed_unitary_pseudogene"] <- "processed_pseudogene"
data.col$Gene.type[data.col$Gene.type == "transcribed_unprocessed_pseudogene,unprocessed_pseudogene,protein_coding"] <- "unprocessed_pseudogene"
data.col$Gene.type[data.col$Gene.type == "transcribed_unprocessed_pseudogene,transcribed_processed_pseudogene"] <- "processed_pseudogene"
data.col$Gene.type[data.col$Gene.type == "transcribed_unprocessed_pseudogene,lincRNA"] <- "lincRNA"
data.col$Gene.type[data.col$Gene.type == "transcribed_unitary_pseudogene,transcribed_unprocessed_pseudogene"] <- "unprocessed_pseudogene"
data.col <- data.col[data.col$Gene.type != "TEC",]
data.col <- data.col[!grepl("^RF0",data.col$Gene.name),]
data.col <- data.col[data.col$Gene.type != "sense_intronic",]
data.col$Gene.type[data.col$Gene.type == "transcribed_unprocessed_pseudogene"] <- "unprocessed_pseudogene"
data.col$Gene.type[data.col$Gene.type == "transcribed_processed_pseudogene"] <- "processed_pseudogene"

data.col <- data.col[!grepl("^AC0",data.col$Gene.name),]
data.col <- data.col[!grepl("^AC1",data.col$Gene.name),]
data.col <- data.col[!grepl("^AC2",data.col$Gene.name),]
data.col <- data.col[!grepl("^AF0",data.col$Gene.name),]
data.col <- data.col[!grepl("^AF1",data.col$Gene.name),]
data.col <- data.col[!grepl("^AF2",data.col$Gene.name),]
data.col <- data.col[!grepl("^AL1",data.col$Gene.name),]
data.col <- data.col[!grepl("^AL0",data.col$Gene.name),]
data.col <- data.col[!grepl("^AL3",data.col$Gene.name),]
data.col <- data.col[!grepl("^AL4",data.col$Gene.name),]
data.col <- data.col[!grepl("^AL5",data.col$Gene.name),]
data.col <- data.col[!grepl("^AL6",data.col$Gene.name),]
data.col <- data.col[!grepl("^AL7",data.col$Gene.name),]
data.col <- data.col[!grepl("^AL8",data.col$Gene.name),]
data.col <- data.col[!grepl("^AL9",data.col$Gene.name),]
data.col <- data.col[!grepl("^AP0",data.col$Gene.name),]
data.col <- data.col[!grepl("^BX",data.col$Gene.name),]
data.col <- data.col[!grepl("^Z6",data.col$Gene.name),]
data.col <- data.col[!grepl("^Z7",data.col$Gene.name),]
data.col <- data.col[!grepl("^Z8",data.col$Gene.name),]
data.col <- data.col[!grepl("^Z9",data.col$Gene.name),]
data.col <- data.col[!grepl("^CR3",data.col$Gene.name),]
data.col <- data.col[!grepl("^CR5",data.col$Gene.name),]
data.col <- data.col[!grepl("^CR7",data.col$Gene.name),]
data.col <- data.col[!grepl("^CR8",data.col$Gene.name),]
data.col <- data.col[!grepl("^CR9",data.col$Gene.name),]
data.col <- data.col[!grepl("^AD0",data.col$Gene.name),]
data.col <- data.col[!grepl("^FO3",data.col$Gene.name),]
data.col <- data.col[!grepl("^CU1",data.col$Gene.name),]
data.col <- data.col[!grepl("^CU4",data.col$Gene.name),]
data.col <- data.col[!grepl("^CU6",data.col$Gene.name),]
data.col <- data.col[!grepl("^AB0",data.col$Gene.name),]
data.col <- data.col[!grepl("^ABBA",data.col$Gene.name),]
data.col <- data.col[!grepl("^AF3",data.col$Gene.name),]
data.col <- data.col[!grepl("^AJ0",data.col$Gene.name),]
data.col <- data.col[!grepl("^AJ2",data.col$Gene.name),]
data.col <- data.col[!grepl("^AMYH0",data.col$Gene.name),]
data.col <- data.col[!grepl("^FO0",data.col$Gene.name),]
data.col <- data.col[!grepl("^FO5",data.col$Gene.name),]
data.col <- data.col[!grepl("^FO6",data.col$Gene.name),]
data.col <- data.col[!grepl("^FO7",data.col$Gene.name),]
data.col <- data.col[!grepl("^FP2",data.col$Gene.name),]
data.col <- data.col[!grepl("^FP3",data.col$Gene.name),]
data.col <- data.col[!grepl("^FP4",data.col$Gene.name),]
data.col <- data.col[!grepl("^FP5",data.col$Gene.name),]
data.col <- data.col[!grepl("^FP6",data.col$Gene.name),]
data.col <- data.col[!grepl("^FP7",data.col$Gene.name),]
data.col <- data.col[!grepl("^FP8",data.col$Gene.name),]
data.col <- data.col[!grepl("^KC8",data.col$Gene.name),]
data.col <- data.col[!grepl("^KF4",data.col$Gene.name),]
data.col <- data.col[!grepl("^U4",data.col$Gene.name),]
data.col <- data.col[!grepl("^U5",data.col$Gene.name),]
data.col <- data.col[!grepl("^U6",data.col$Gene.name),]
data.col <- data.col[!grepl("^U7",data.col$Gene.name),]
data.col <- data.col[!grepl("^U8",data.col$Gene.name),]
data.col <- data.col[!grepl("^U9",data.col$Gene.name),]
data.col <- data.col[!grepl("^L340",data.col$Gene.name),]
data.col <- data.col[!grepl("^L290",data.col$Gene.name),]

data.col$Gene.type[data.col$Gene.type == "IG_V_pseudogene"] <- "processed_pseudogene"
data.col$Gene.type[data.col$Gene.type == "IG_V_gene"] <- "protein_coding"
data.col$Gene.type[data.col$Gene.type == "TR_V_gene"] <- "protein_coding"
data.col$Gene.type[data.col$Gene.type == "TR_J_gene"] <- "protein_coding"
data.col$Gene.type[data.col$Gene.type == "IG_D_gene"] <- "protein_coding"
data.col$Gene.type[data.col$Gene.type == "IG_J_gene"] <- "protein_coding"
data.col$Gene.type[data.col$Gene.type == "transcribed_unitary_pseudogene"] <- "processed_pseudogene"
data.col$Gene.type[data.col$Gene.type == "unitary_pseudogene"] <- "processed_pseudogene"
data.col$Gene.type[data.col$Gene.type == "TR_V_pseudogene"] <- "processed_pseudogene"
data.col$Gene.type[data.col$Gene.type == "polymorphic_pseudogene"] <- "processed_pseudogene"
data.col$Gene.type[data.col$Gene.type == "processed_transcript"] <- "antisense"
data.col$Gene.type[data.col$Gene.type == "processed_transcript,antisense"] <- "antisense"
data.col$Gene.type[data.col$Gene.type == "protein_coding,processed_transcript"] <- "protein_coding"
data.col$Gene.type[data.col$Gene.type == "transcribed_processed_pseudogene,unprocessed_pseudogene,protein_coding"] <- "processed_pseudogene"
data.col$Gene.type[data.col$Gene.type == "transcribed_processed_pseudogene,unprocessed_pseudogene"] <- "processed_pseudogene"
data.col$Gene.type[data.col$Gene.type == "sense_overlapping"] <- "misc_RNA"
data.col$Gene.type[data.col$Gene.type == "bidirectional_promoter_lncRNA"] <- "misc_RNA"
data.col$Gene.type[data.col$Gene.type == "IG_C_pseudogene"] <- "processed_pseudogene"
data.col$Gene.type[data.col$Gene.type == "processed_transcript,lincRNA"] <- "lincRNA"
data.col$Gene.type[data.col$Gene.type == "protein_coding,polymorphic_pseudogene"] <- "protein_coding"
data.col$Gene.type[data.col$Gene.type == "TR_C_gene"] <- "protein_coding"
data.col$Gene.type[data.col$Gene.type == "transcribed_unprocessed_pseudogene,unprocessed_pseudogene"] <- "unprocessed_pseudogene"
data.col$Gene.type[data.col$Gene.type == "unprocessed_pseudogene,transcribed_unprocessed_pseudogene"] <- "unprocessed_pseudogene"
data.col$Gene.type[data.col$Gene.type == "processed_transcript,protein_coding"] <- "protein_coding"
data.col$Gene.type[data.col$Gene.type == "ribozyme"] <- "processed_pseudogene"
data.col$Gene.type[data.col$Gene.type == "TR_D_gene"] <- "protein_coding"
data.col$Gene.type[data.col$Gene.type == "3prime_overlapping_ncRNA"] <- "misc_RNA"
data.col$Gene.type[data.col$Gene.type == "TR_J_pseudogene"] <- "processed_pseudogene"
data.col$Gene.type[data.col$Gene.type == "IG_J_pseudogene"] <- "processed_pseudogene"
data.col$Gene.type[data.col$Gene.type == "lincRprocessed_transcript"] <- "lincRNA"
data.col$Gene.type[data.col$Gene.type == "polymorphic_pseudogene,protein_coding"] <- "protein_coding"
data.col$Gene.type[data.col$Gene.type == "transcribed_processed_pseudogene,processed_pseudogene"] <- "processed_pseudogene"
data.col$Gene.type[data.col$Gene.type == "vaultRNA"] <- "misc_RNA"
data.col$Gene.type[data.col$Gene.type == "scRNA"] <- "misc_RNA"
data.col$Gene.type[data.col$Gene.type == "ribozyme,lincRNA"] <- "lincRNA"
data.col$Gene.type[data.col$Gene.type == "protein_coding,polymorphic_pseudogene,unprocessed_pseudogene"] <- "protein_coding"
data.col$Gene.type[data.col$Gene.type == "protein_coding,lincRNA"] <- "lincRNA"
data.col$Gene.type[data.col$Gene.type == "processed_transcript,transcribed_processed_pseudogene"] <- "processed_pseudogene"
data.col$Gene.type[data.col$Gene.type == "processed_transcript,sense_overlapping"] <- "misc_RNA"
data.col$Gene.type[data.col$Gene.type == "processed_transcript,sense_intronic"] <- "misc_RNA"
data.col$Gene.type[data.col$Gene.type == "processed_pseudogene,transcribed_processed_pseudogene"] <- "processed_pseudogene"
data.col$Gene.type[data.col$Gene.type == "processed_pseudogene,lincRprotein_coding"] <- "protein_coding"
data.col$Gene.type[data.col$Gene.type == "macro_lncRNA"] <- "misc_RNA"
data.col$Gene.type[data.col$Gene.type == "lincRsense_intronic"] <- "lincRNA"
data.col$Gene.type[data.col$Gene.type == "lincRprotein_coding,unprocessed_pseudogene"] <- "protein_coding"
data.col$Gene.type[data.col$Gene.type == "antisense,processed_transcript,transcribed_unitary_pseudogene"] <- "processed_pseudogene"
data.col$Gene.type[data.col$Gene.type == "antisense,processed_transcript"] <- "antisense"
data.col$Gene.type[data.col$Gene.type == "unprocessed_pseudogene,processed_pseudogene"] <- "processed_pseudogene"
data.col$Gene.type[data.col$Gene.type == "protein_coding,transcribed_unprocessed_pseudogene"] <- "protein_coding"
data.col$Gene.type[data.col$Gene.type == "non_coding"] <- "misc_RNA"
data.col$Gene.type[data.col$Gene.type == "lincRprotein_coding"] <- "misc_RNA"
data.col$Gene.type[data.col$Gene.type == "lincRantisense"] <- "lincRNA"
data.col$Gene.type[data.col$Gene.type == "antisense,protein_coding"] <- "protein_coding"
data.col$Gene.type[data.col$Gene.type == "transcribed_unprocessed_pseudogene,processed_transcript"] <- "unprocessed_pseudogene"
data.col$Gene.type[data.col$Gene.type == "protein_coding,unprocessed_pseudogene"] <- "protein_coding"

data.col$Gene.type[data.col$Gene.type == "processed_pseudogene"] <- "pseudogene"
data.col$Gene.type[data.col$Gene.type == "unprocessed_pseudogene"] <- "pseudogene"
data.col$Gene.type[data.col$Gene.type == "lincRNA"] <- "non-protein_coding_RNA"
data.col$Gene.type[data.col$Gene.type == "miRNA"] <- "non-protein_coding_RNA"
data.col$Gene.type[data.col$Gene.type == "snRNA"] <- "non-protein_coding_RNA"
data.col$Gene.type[data.col$Gene.type == "antisense"] <- "non-protein_coding_RNA"
data.col$Gene.type[data.col$Gene.type == "misc_RNA"] <- "non-protein_coding_RNA"
data.col$Gene.type[data.col$Gene.type == "Mt_rRNA"] <- "non-protein_coding_RNA"
data.col$Gene.type[data.col$Gene.type == "Mt_tRNA"] <- "non-protein_coding_RNA"
data.col$Gene.type[data.col$Gene.type == "scaRNA"] <- "non-protein_coding_RNA"
data.col$Gene.type[data.col$Gene.type == "snoRNA"] <- "non-protein_coding_RNA"
data.col$Gene.type[data.col$Gene.type == "rRNA"] <- "non-protein_coding_RNA"

data.col$Gene.description <- gsub(" \\[.*\\]","",data.col$Gene.description)

data.col[row.names(data.col) == 34241,] <- c("LINC01297","q11.2",-1,14,19344578,19384587,"non-protein_coding_RNA","long intergenic non-protein coding RNA 1297")
data.col[row.names(data.col) == 30090,] <- c("H2BFS","q22.3",1,21,43565189,43565648,"protein_coding","H2B histone family member S")

data.bind <- data.col

chr <- vector()
start <- vector()
stop <- vector()

for(i in 1:nrow(data.col)){
  if(length(which(str_split(data.col$Chromosome.scaffold.name[i],pattern = ",",simplify = T) %in% c(1:22,"X","Y","MT"))) < 1){
  chr <- append(chr,data.col$Chromosome.scaffold.name[i])
  start <- append(start,data.col$Gene.start..bp.[i])
  stop <- append(stop,data.col$Gene.end..bp.[i])  
  } else {
  chr_index <- which(str_split(data.col$Chromosome.scaffold.name[i],pattern = ",",simplify = T) %in% c(1:22,"X","Y","MT"))
  chr <- append(chr,paste(str_split(data.col$Chromosome.scaffold.name[i],pattern = ",",simplify = T)[chr_index],collapse = ","))
  
    if(length(str_split(data.col$Gene.start..bp.[i],pattern = ",",simplify = T)[chr_index]) < chr_index){
      start <- append(start,data.col$Gene.start..bp.[i])
    } else {
      start <- append(start,paste(str_split(data.col$Gene.start..bp.[i],pattern = ",",simplify = T)[chr_index],collapse = ","))
    }
  
    if(length(str_split(data.col$Gene.start..bp.[i],pattern = ",",simplify = T)[chr_index]) < chr_index){
      stop <- append(stop,data.col$Gene.end..bp.[i])
    } else {
      stop <- append(stop,paste(str_split(data.col$Gene.end..bp.[i],pattern = ",",simplify = T)[chr_index],collapse = ","))
    }
  }
}

data.bind$Chromosome.scaffold.name <- chr
data.bind$Gene.start..bp. <- start
data.bind$Gene.end..bp. <- stop

data.min <- data.bind

start <- vector()
stop <- vector()

for(i in 1:nrow(data.bind)){
  if(length(str_split(data.col$Gene.start..bp.[i],pattern = ",",simplify = T)) > 1){
    start <- append(start,min(str_split(data.col$Gene.start..bp.[i],pattern = ",",simplify = T)))
  } else {
    start <- append(start,data.col$Gene.start..bp.[i])
  }
  if(length(str_split(data.col$Gene.end..bp.[i],pattern = ",",simplify = T)) > 1){
    stop <- append(stop,min(str_split(data.col$Gene.end..bp.[i],pattern = ",",simplify = T)))
  } else {
    stop <- append(stop,data.col$Gene.end..bp.[i])
  }
}

data.min$Gene.start..bp. <- start
data.min$Gene.end..bp. <- stop

data.min$Chromosome.scaffold.name[data.min$Gene.name == "C4B_2"] <- 6
data.min$Chromosome.scaffold.name[data.min$Gene.name == "CCL3L3"] <- 17
data.min$Chromosome.scaffold.name[data.min$Gene.name == "FAM83H"] <- 8
data.min$Chromosome.scaffold.name[data.min$Gene.name == "FAM83H-AS1"] <- 8
data.min$Chromosome.scaffold.name[data.min$Gene.name == "FAM8A5P"] <- 6
data.min$Chromosome.scaffold.name[data.min$Gene.name == "GSTT1"] <- 22
data.min$Chromosome.scaffold.name[data.min$Gene.name == "GTF2H2C_2"] <- 5
data.min$Chromosome.scaffold.name[data.min$Gene.name == "HLA-DRB2"] <- 6
data.min$Chromosome.scaffold.name[data.min$Gene.name == "HLA-DRB3"] <- 6
data.min$Chromosome.scaffold.name[data.min$Gene.name == "HLA-DRB4"] <- 6
data.min$Chromosome.scaffold.name[data.min$Gene.name == "HLA-DRB7"] <- 6
data.min$Chromosome.scaffold.name[data.min$Gene.name == "HLA-DRB8"] <- 6
data.min$Chromosome.scaffold.name[data.min$Gene.name == "IGHVII-31-1"] <- 14
data.min$Chromosome.scaffold.name[data.min$Gene.name == "KIR2DL2"] <- 19
data.min$Chromosome.scaffold.name[data.min$Gene.name == "KIR2DL5A"] <- 19
data.min$Chromosome.scaffold.name[data.min$Gene.name == "KIR2DL5B"] <- 19
data.min$Chromosome.scaffold.name[data.min$Gene.name == "KIR2DS1"] <- 19
data.min$Chromosome.scaffold.name[data.min$Gene.name == "KIR2DS2"] <- 19
data.min$Chromosome.scaffold.name[data.min$Gene.name == "KIR2DS3"] <- 19
data.min$Chromosome.scaffold.name[data.min$Gene.name == "KIR2DS5"] <- 19
data.min$Chromosome.scaffold.name[data.min$Gene.name == "KIR3DS1"] <- 19
data.min$Chromosome.scaffold.name[data.min$Gene.name == "LILRA3"] <- 19
data.min$Chromosome.scaffold.name[data.min$Gene.name == "PRAMEF22"] <- 1
data.min$Chromosome.scaffold.name[data.min$Gene.name == "PRSS3P2"] <- 7
data.min$Chromosome.scaffold.name[data.min$Gene.name == "RNU1-116P"] <- 6
data.min$Chromosome.scaffold.name[data.min$Gene.name == "RNU1-79P"] <- 6
data.min$Chromosome.scaffold.name[data.min$Gene.name == "RNU6-443P"] <- 1
data.min$Chromosome.scaffold.name[data.min$Gene.name == "TAS2R45"] <- 12
data.min$Chromosome.scaffold.name[data.min$Gene.name == "OR8U8"] <- 11
data.min$Chromosome.scaffold.name[data.min$Gene.name == "OR9G9"] <- 11
data.min$Chromosome.scaffold.name[data.min$Gene.name == "FAM231C"] <- 1
data.min$Chromosome.scaffold.name[data.min$Gene.name == "MAFIP"] <- 14

data.min$Karyotype.band[data.min$Karyotype.band == "NA" & data.min$Chromosome.scaffold.name == "MT"] <- "MT"
data.min$Karyotype.band[data.min$Karyotype.band == "NA"] <- "Unknown"
data.min$Strand <- str_split(data.min$Strand,pattern = ",",simplify = T)[,1]

data.min$Gene.type[data.min$Gene.type == "protein_coding"] <- "Protein coding"
data.min$Gene.type[data.min$Gene.type == "pseudogene"] <- "Pseudogene"
data.min$Gene.type[data.min$Gene.type == "non-protein_coding_RNA"] <- "Non-protein coding RNA"
data.min$Strand[data.min$Strand == "-1"] <- "Minus"
data.min$Strand[data.min$Strand == "1"] <- "Plus"

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

data.min$Gene.description <- as.character(sapply(data.min$Gene.description, simpleCap))
write.table(data.min,file = "gene_info_formatted.tsv",quote = F,sep = "\t",row.names = F,col.names = T)
