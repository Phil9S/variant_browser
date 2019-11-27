rm(list = ls())
## Enable cmd line args
args = commandArgs(trailingOnly=TRUE)
if (!requireNamespace("BiocManager", quietly = TRUE)){
    install.packages("BiocManager")
}

if(!require(snpStats)){
  BiocManager::install("snpStats")
}
if(!require(ggplot2)){
  install.packagesg("ggplot2")
}

cohort <- args[1]
data <- snpStats::read.plink(bed = paste(args[1],"_REF.maf0.05.bed",sep = ""),bim = paste(args[1],"_REF.maf0.05.bim",sep = ""),fam = paste(args[1],"_REF.maf0.05.fam",sep = ""))
pop <- read.table("/home/pss41/G1K_vcf/1KG_samplePopulations.tsv",sep="\t",stringsAsFactors=F,header=T)

data.matrix <- as.data.frame(data[1]$genotypes)
col <- as.character(colnames(data.matrix))
row <- as.character(rownames(data.matrix))

data.matrix <- apply(data.matrix,2,as.numeric)
rownames(data.matrix) <- row
pca <- prcomp(data.matrix)
pca.data <- as.data.frame(pca$x[,1:5])

pca.data$sample <- rownames(pca.data)
data.merge <- merge(pca.data,pop[,c(2,4)],by.x="sample",by.y="Sample",all.x=T)
data.merge$Super_population[is.na(data.merge$Super_population)] <- "Sample"

assign(cohort,data.merge)
rm(args,data,col,row,data.matrix,pca,pca.data,data.merge,pop)
save(list = setdiff(ls(), "cohort"),file = paste("pcaPOP.",cohort,".RData",sep = ""))