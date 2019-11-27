#Load original data
load("www/db_cohortdata.RData")
sample_data <- read.table(file = "www/sample_data.final.info", header = TRUE, sep = "\t")
load("www/db_admixture.RData")
load("www/db_pca.RData")

samples <- 0.25 # Percentage of samples 
sites <- 0.05 # Percentage of sites to kept prior to genotype filtering
sample_col <- 69 # Index of first sample column after site and annotation information

## Randomly subset sites and samples per group to reduce cohort size
vb_data_samp <- lapply(cohort_list, function(x){
  x <- x[sample(c(1:nrow(x)),size = nrow(x)*sites),c(colnames(x)[1:(sample_col - 1)],sample(colnames(x)[sample_col:ncol(x)],size = length(colnames(x)[sample_col:ncol(x)])*samples))]
  y <- apply(x[,sample_col:ncol(x)],MARGIN = 1,FUN = function(y) (sum(as.numeric(gsub(x = y,pattern = "-9",replacement = "0")))>0))
  x[names(y[which(y == TRUE)]),]
  return(x)
})

## Admixture sub-sampling
vb_admix_samp <- lapply(1:length(vb_data_samp),FUN = function(x){
  x <- admixture[[x]][,admixture[[x]][1,] %in% c("Sample",colnames(vb_data_samp[[x]][,sample_col:ncol(vb_data_samp[[x]])]))]
  return(x)
})

## PCA plot sub-samling
vb_pca_samp <- lapply(1:length(vb_data_samp),FUN = function(x){
  samples <- pca_data[[x]][pca_data[[x]][,7] == "Sample",1]
  n_samples <- pca_data[[x]][pca_data[[x]][,7] != "Sample",1]
  t_samples <- colnames(vb_data_samp[[x]][,sample_col:ncol(vb_data_samp[[x]])])
  samples <- samples[samples %in% t_samples]
  x <- pca_data[[x]][which(pca_data[[x]][,1] %in% c(samples,n_samples)),]
  return(x)
})

## Clinical Sample information sub-sampling
vb_sample_data <- do.call(rbind,lapply(names(vb_data_samp),FUN = function(x){
  sample_data[sample_data$COHORT == x & sample_data$DATA_ID %in% colnames(vb_data_samp[[x]][,sample_col:ncol(vb_data_samp[[x]])]),]
}))

## Renaming cohorts
orig_names <- names(cohort_list)
names(vb_data_samp) <- paste("Cohort",seq.int(length(names(vb_data_samp))),sep = "_")
names(vb_pca_samp) <- names(vb_data_samp)
names(vb_admix_samp) <- names(vb_data_samp)

## Renaming and anonymising sample columns in variant data
vb_data_samp <- lapply(names(vb_data_samp),FUN=function(x){
  new_cols <- c(colnames(vb_data_samp[[x]][,c(1:(sample_col - 1))]),paste(x,"_","sample_",seq.int(sample_col:ncol(vb_data_samp[[x]])),sep = ""))
  colnames(vb_data_samp[[x]]) <- new_cols
  return(vb_data_samp[[x]])
})
names(vb_data_samp) <- paste("Cohort",seq.int(length(orig_names)),sep = "_")

## Renaming and anonymising sample columns in PCA data
vb_pca_samp <- lapply(1:length(vb_data_samp),FUN = function(x){
  a <- colnames(vb_data_samp[[x]][,c(sample_col:ncol(vb_data_samp[[x]]))])
  vb_pca_samp[[x]][which(vb_pca_samp[[x]][,7] == "Sample"),1] <- a
  return(vb_pca_samp[[x]])
})
names(vb_pca_samp) <- names(vb_data_samp)

## Renaming and anonymising sample columns in admixture data
vb_admix_samp <- lapply(1:length(vb_data_samp),FUN = function(x){
  a <- c("Sample",colnames(vb_data_samp[[x]][,c(sample_col:ncol(vb_data_samp[[x]]))]))
  vb_admix_samp[[x]][1,] <- a
  return(vb_admix_samp[[x]])
})
names(vb_admix_samp) <- names(vb_data_samp)

## Renaming and anonymising clinical information
vb_sample_data <- merge(vb_sample_data,cbind(orig_names,cohort_new=names(vb_data_samp)),all.x = T,all.y = F,by.x = "COHORT",by.y = "orig_names")
vb_sample_data$COHORT <- vb_sample_data$cohort_new

vb_sample_data <- do.call(rbind,lapply(unique(vb_sample_data$COHORT),FUN=function(x){
  a <- vb_sample_data[vb_sample_data$COHORT == x,]
  l <- nrow(a)
  sample_names <- paste(x,"_","sample_",seq.int(1:l),sep = "")
  a$DATA_ID <- sample_names
  a$SAMPLE_ID <- sample_names
  a$PHENOTYPE <- paste(x,"_pheno",sep = "")
  a <- a[,!colnames(a) %in% c("cohort_new")]
}))

#Write sub-sampled output to RData files and TSV
cohort_list <- vb_data_samp
admixture <- vb_admix_samp
pca_data <- vb_pca_samp
sample_data <- vb_sample_data
rm(vb_admix_samp,vb_data_samp,vb_pca_samp,vb_sample_data,orig_names,sample_col,samples,sites)

save(cohort_list,file = "db_cohortdata.RData")
save(admixture,file = "db_admixture.RData")
save(pca_data,file = "db_pca.RData")
write.table(x = sample_data,file = "sample_data.final.info",quote = FALSE,append = FALSE,sep = "\t",row.names = FALSE,col.names = TRUE)
