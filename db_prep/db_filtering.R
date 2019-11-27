rm(list = ls())
args = commandArgs(trailingOnly=TRUE)
###set working directory and import arguments and libraries

setwd(args[1]) ##comment out and set manually if working locally - i.e. non-server side and without bash script
cohort <- args[2]
require(stringr)
require(dplyr)
require(vcfR)
options(scipen=999)

## Read vcf file and convert to tabular format
vcf <- read.vcfR(paste(cohort,".FILT.vcf",sep=""))
tb <- vcfR2tidy(vcf)

variant.data <- as.data.frame(tb[[1]])
GTs <- extract.gt(x = vcf,element = "GT")

## Replace genotypes with numeric values
GTs[GTs == "0/0"] <- 0
GTs[GTs == "0/1"] <- 1
GTs[GTs == "1/1"] <- 2
GTs[is.na(GTs)] <- -9

##apply function to sum the number of missing, het, hom and ref sites
refHOM <- apply(GTs,1, function(x) sum(x == 0))
HETp <- apply(GTs,1, function(x) sum(x == 1))
nonHOM <- apply(GTs, 1, function(x) sum(x == 2))
miss <- apply(GTs, 1, function(x) sum(x == -9))
variant.data$HET_val <- HETp
variant.data$HOM_val <- nonHOM

##form matrix for pct calculations
calc <- cbind(refHOM,HETp,nonHOM,miss)

##calculate hetpct & hompct (excluding missing sites) and missingness over all sites
hetpct <- ((calc[,2] / (calc[,1] + calc[,2] + calc[,3]))*100)
hompct <- ((calc[,3] / (calc[,1] + calc[,2] + calc[,3]))*100)
misspct <- ((calc[,4] / length(GTs[1,])*100))

## Add het/hom/miss rates to table
variant.data$HET_rate <- hetpct
variant.data$HOM_rate <- hompct
variant.data$MISS_rate <- misspct

## Calculate and add internal cohort minor allele frequeny
variant.data$INTERNAL_freq <- (HETp + (nonHOM * 2)) / ((HETp + nonHOM + refHOM)*2)
rm(hetpct, hompct, misspct, calc, HETp, nonHOM, refHOM, miss)

## Update ID column with non-RsID vars using chrom and position
variant.data$ID[is.na(variant.data$ID)] <- paste(variant.data$CHROM[is.na(variant.data$ID)],"_",variant.data$POS[is.na(variant.data$ID)],sep = "")

## Generate allelic depth matrix
allelic_DEPTH <- AD_frequency(ad = extract.gt(vcf,element = "AD"),allele = 2)

## Multiallelic check
if(any(grepl(pattern = "rs[0-9]+_[0-9]+$",perl = T,rownames(allelic_DEPTH)))){
  variant.data$ID <- paste(variant.data$ID,seq.int(1,length(variant.data$ID),1),sep = "_")
}

## Check all variant Ids are equal to rownames in allelic_depth matrix
all(rownames(allelic_DEPTH) == variant.data$ID)
all(rownames(allelic_DEPTH) == rownames(GTs))

## Remove sites with no allelic depth greater than 0.3
AD_names <- names(which(apply(allelic_DEPTH,1,FUN = function(x) max(x[2:length(x)], na.rm = TRUE)) > 0.3))

variant.data <- variant.data[variant.data$ID %in% AD_names,]
allelic_DEPTH <- allelic_DEPTH[rownames(allelic_DEPTH) %in% AD_names,]
GTs <- GTs[rownames(GTs) %in% AD_names,]

## Check all variant Ids are equal to rownames in allelic_depth matrix
all(rownames(allelic_DEPTH) == variant.data$ID)
all(rownames(allelic_DEPTH) == rownames(GTs))

## Select sites from DP matrix
DP_mat <- extract.gt(vcf,element = "DP",as.numeric = T)
DP_mat <- DP_mat[rownames(DP_mat) %in% AD_names,]
## Check the rownames are matching and in order
all(rownames(allelic_DEPTH) == rownames(DP_mat))
## Add mean read depth per site
variant.data$Mean_RD <- round(rowMeans(DP_mat,na.rm = T),digits = 1)

## Formatting changes
## Replace all "\\x3b" characters inserted by annovar
variant.data <- as.data.frame(apply(variant.data,MARGIN = 2,FUN = function(y) gsub(x = y,pattern = "\\x3b",replacement = ";",fixed = T)),stringsAsFactors = F)

variant.data$ExAC_ALL[is.na(variant.data$ExAC_ALL)] <- 0
variant.data$`1000g2015aug_all`[is.na(variant.data$`1000g2015aug_all`)] <- 0
variant.data$ExAC_AFR[is.na(variant.data$ExAC_AFR)] <- 0
variant.data$ExAC_AMR[is.na(variant.data$ExAC_AMR)] <- 0
variant.data$ExAC_EAS[is.na(variant.data$ExAC_EAS)] <- 0
variant.data$ExAC_FIN[is.na(variant.data$ExAC_FIN)] <- 0
variant.data$ExAC_NFE[is.na(variant.data$ExAC_NFE)] <- 0
variant.data$ExAC_OTH[is.na(variant.data$ExAC_OTH)] <- 0
variant.data$ExAC_SAS[is.na(variant.data$ExAC_SAS)] <- 0

## Add splicing value to function column - add genedetail information to AA change - merged for ease of referencing
variant.data$ExonicFunc.refGene[variant.data$Func.refGene == "splicing"] <- "splicing"
variant.data$AAChange.refGene[variant.data$GeneDetail.refGene != "."] <- variant.data$GeneDetail.refGene[variant.data$GeneDetail.refGene != "."]
variant.data <- variant.data[,!(colnames(variant.data) == "GeneDetail.refGene")]

## Add intronic flag to exonicfunc
variant.data$ExonicFunc.refGene[variant.data$ExonicFunc.refGene == "."] <- "intronic"
## Replace all "-9" missing values with NA
variant.data[variant.data == "."] <- NA

## Merge rsID and avsnp144 columns which contain differing rsIds
variant.data$ID[!grepl("rs",x = variant.data$ID)] <- variant.data$avsnp150[!grepl("rs",x = variant.data$ID)]
## Update ID column with non-RsID vars using chrom and position
variant.data$ID[is.na(variant.data$ID)] <- paste(variant.data$CHROM[is.na(variant.data$ID)],"_",variant.data$POS[is.na(variant.data$ID)],sep = "")
## Remove avsnp150 column
variant.data <- variant.data[,!(colnames(variant.data) == "avsnp150")]
## Remove var tags from rsIds
variant.data$ID <- gsub(gsub(pattern = "((chr.*_.*))_.*$",replacement = "\1",perl = T,x = variant.data$ID),pattern = " ",replacement = "")
variant.data$ID[grep("rs",variant.data$ID)] <- gsub(pattern = "_.*$",replacement = "",perl = T,x = variant.data$ID[grep("rs",variant.data$ID)])

## Remove SNV tag from consequence column
variant.data$ExonicFunc.refGene <- gsub("_SNV","",variant.data$ExonicFunc.refGene,fixed = TRUE)

## Keep rankscores only for FUNCTIONAL ANNOTATION
variant.data <- variant.data[,!(colnames(variant.data) %in% c("SIFT_score","SIFT_pred","Polyphen2_HDIV_score","Polyphen2_HDIV_pred",
                                                              "Polyphen2_HVAR_score","Polyphen2_HVAR_pred","LRT_score","LRT_pred",
                                                              "MutationTaster_score","MutationTaster_pred","MutationAssessor_score",
                                                              "MutationAssessor_pred","FATHMM_score","FATHMM_pred","PROVEAN_score",
                                                              "PROVEAN_pred","MetaSVM_score","MetaSVM_pred","MetaLR_score","MetaLR_pred",
                                                              "M-CAP_score","M-CAP_pred","VEST3_score","CADD_phred",
                                                              "DANN_score","fathmm-MKL_coding_score","fathmm-MKL_coding_pred",
                                                              "Eigen_coding_or_noncoding","Eigen-raw","Eigen-PC-raw","GenoCanyon_score",
                                                              "integrated_fitCons_score","integrated_confidence_value","GERP++_RS",
                                                              "phyloP100way_vertebrate","phyloP20way_mammalian","phastCons100way_vertebrate",
                                                              "phastCons20way_mammalian","SiPhy_29way_logOdds"))]

## Spliting AA_NT_EXON_TRANSCRIPT information into seperate fields - retaining all transcripts - cbind to main table
exonic_func_split <- gsub("\\;",",",variant.data$AAChange.refGene)
exonic_func_split <- str_split(exonic_func_split,",")

AA_split <- do.call(rbind,lapply(exonic_func_split,FUN = function(x){
  string <- as.data.frame(str_split(x,":",simplify = TRUE))
  if(length(string) == 1){
    return(data.frame(GENE="GENE_PH",TRANSCRIPT=NA,EXON=NA,NT_CHANGE=NA,AA_CHANGE=NA))
  } else if(length(string) == 3) {
    string <- data.frame(V1=rep_len("gene_PH",nrow(string)),string)
    len3 <- as.data.frame(string %>%  group_by(GENE=V1)%>% 
                            summarise(TRANSCRIPT=paste(V1.1,collapse = ","),
                                      EXON=paste(V2,collapse = ","),
                                      NT_CHANGE=paste(V3,collapse = ",")))
    if(nrow(len3) > 1){print(i)}
    return(data.frame(len3,AA_CHANGE=NA))
  } else if(length(string) == 5){
    len5 <- as.data.frame(string %>% 
                            summarise(GENE=paste(V1,collapse = ","),
                                      TRANSCRIPT=paste(V2,collapse = ","),
                                      EXON=paste(V3,collapse = ","),
                                      NT_CHANGE=paste(V4,collapse = ","),
                                      AA_CHANGE=paste(V5,collapse = ",")))
    if(nrow(len5) > 1){print(i)}
    return(len5)
  } 
}))

AA_split <- AA_split[-1]
variant.data <- cbind(variant.data,AA_split)
variant.data <- variant.data[,!(colnames(variant.data) == "AAChange.refGene")]

## Spliting and taking unique in CLINSIG
variant.data$CLINVAR <- gsub(x = variant.data$CLNSIG,pattern = ",_",replacement = ";")

## Spliting and taking unique in CLINSIG
variant.data$DISEASE <- str_to_title(gsub(x = variant.data$CLNDN,pattern = "\\|",replacement = ";"))

## Clean up
rm(AA_split,exonic_func_split,AD_names)

## Fix cosmic column
variant.data$cosmic70 <- gsub(pattern = "\\\\x3d",";",variant.data$cosmic70)

## Dropped columns
dropped <- c("DB","DP","DS","Dels","FS","MLEAC","MLEAF","MQ0","OND","QD",
             "RPA","RU","SOR","STR","ANNOVAR_DATE","AC","AF","CADD_raw",
             "CLNALLELEID","CLNDISDB","CLNREVSTAT","CLNSIG","CLNDN",
             "ALLELE_END","GTEx_V6_gene","GTEx_V6_tissue","dbscSNV_ADA_SCORE",
             "dbscSNV_RF_SCORE","ChromKey")
variant.data <- variant.data[,!(colnames(variant.data) %in% dropped)]
rm(dropped)

# Column typing
variant.data$POS <-as.numeric(variant.data$POS)
variant.data$QUAL <-as.numeric(variant.data$QUAL)
variant.data$ABHet <-as.numeric(variant.data$ABHet)
variant.data$ABHom <-as.numeric(variant.data$ABHom)
variant.data$AN <-as.numeric(variant.data$AN)
variant.data$BaseQRankSum <-as.numeric(variant.data$BaseQRankSum)
variant.data$ExcessHet <-as.numeric(variant.data$ExcessHet)
variant.data$HaplotypeScore <-as.numeric(variant.data$HaplotypeScore)
variant.data$InbreedingCoeff <-as.numeric(variant.data$InbreedingCoeff)
variant.data$MQ <-as.numeric(variant.data$MQ)
variant.data$MQRankSum <-as.numeric(variant.data$MQRankSum)
variant.data$ReadPosRankSum <-as.numeric(variant.data$ReadPosRankSum)
variant.data$`1000g2015aug_all` <-as.numeric(variant.data$`1000g2015aug_all`)
variant.data$ExAC_ALL <-as.numeric(variant.data$ExAC_ALL)
variant.data$ExAC_NFE <-as.numeric(variant.data$ExAC_NFE)
variant.data$ExAC_AFR <-as.numeric(variant.data$ExAC_AFR)
variant.data$ExAC_AMR <-as.numeric(variant.data$ExAC_AMR)
variant.data$ExAC_EAS <-as.numeric(variant.data$ExAC_EAS)
variant.data$ExAC_FIN <-as.numeric(variant.data$ExAC_FIN)
variant.data$ExAC_OTH <-as.numeric(variant.data$ExAC_OTH)
variant.data$ExAC_SAS <-as.numeric(variant.data$ExAC_SAS)
variant.data[grep(pattern = "rankscore",colnames(variant.data))] <- as.data.frame(apply(variant.data[grep(pattern = "rankscore",colnames(variant.data))],2,as.numeric))
variant.data$HET_val <-as.numeric(variant.data$HET_val)
variant.data$HOM_val <-as.numeric(variant.data$HOM_val)
variant.data$HET_rate <-as.numeric(variant.data$HET_rate)
variant.data$HOM_rate <-as.numeric(variant.data$HOM_rate)
variant.data$MISS_rate <-as.numeric(variant.data$MISS_rate)
variant.data$INTERNAL_freq <-as.numeric(variant.data$INTERNAL_freq)
variant.data$Mean_RD <-as.numeric(variant.data$Mean_RD)

## Substite frameshift to FS for table widths etc
variant.data$ExonicFunc.refGene <- gsub("frameshift","FS",variant.data$ExonicFunc.refGene)

## Add index column for continuity with old version
variant.data <- cbind(Id=seq.int(1,nrow(variant.data),1),variant.data)

## Name columns
colnames(variant.data) <- c("Id","CHR","POS","rsID","REF","ALT","QUAL","FILTER","ABHet",
                                             "ABHom","AN","BaseQRankSum","ExcessHet","HaplotypeScore","InbreedingCoeff","MQ","MQRankSum","ReadPosRankSum",
                                             "FUNC","GENE","CONSEQ","1Kg_all","ExAC_ALL","ExAC_AFR","ExAC_AMR","ExAC_EAS","ExAC_FIN","ExAC_NFE","ExAC_OTH","ExAC_SAS",
                                             "SIFT","POLYP","POLYP_VAR","LRT","MUT_TASTER","MUT_ASSESSOR","FATHMM","PROVEAN","VEST3","METASVM","METALR","MCAP","CADD",
                                             "DANN","FATHMMMKL","GENOCANYON","INT_FITCONS","GERP","PHYLOP100","PHYLOP20","PHAST100","PHAST20","SIPHY29","INTERPRO","COSMIC",
                                             "HET_val","HOM_val","HET_rate","HOM_rate","MISS_rate","INT_freq","MEAN_RD","TRANSCRIPT","EXON","DNA","AA","CLINVAR","DISEASE")

## Add genotype information for remaining variants
final_file <- cbind(variant.data,GTs)

## Data for phenotype file
sample_names <- colnames(GTs)
sample_names_orig <- gsub("\\.","-",sample_names)
sample_names_orig[grep("^X[0-9]",sample_names_orig)] <- gsub("^X",replacement = "",sample_names_orig[grep("^X[0-9]",sample_names_orig)])

sample_info <- data.frame(rep(cohort,length(sample_names)),
                          sample_names,sample_names_orig,
                          rep(NA,length(sample_names)),
                          rep(NA,length(sample_names)),
                          rep(NA,length(sample_names)))

write.table(x = sample_info,file = "sample_data.info",quote = FALSE,append = TRUE,sep = "\t",row.names = FALSE,col.names = FALSE)

## Remove preprocessed data - leave only final named dataframe and cohort value for file names
rm(allelic_DEPTH,DP_mat,GTs,sample_info,variant.data,sample_names,sample_names_orig,tb,vcf)


## Make list of supporting data - VAF and RD - test to see if able to implement in app (POTENTIAL FUTURE GOAL)
#data <- list(final_file,VAF_matrix,DEPTH_matrix)
#assing(cohort,data)

## Assign cohort name to data table
assign(cohort,final_file)
rm(final_file)

###write filtered table out
save(list = setdiff(ls(), "cohort"),file = paste("data.",cohort,".RData",sep = ""))
