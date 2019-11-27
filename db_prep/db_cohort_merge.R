rm(list=ls())
args = commandArgs(trailingOnly=TRUE)

setwd(args[1])

filenames <- dir(path = ".",pattern = "data.*.RData")
lapply(filenames,load,.GlobalEnv)
rm(filenames,args)
cohort_list <- sapply(ls(), function(x) get(x),USE.NAMES = TRUE)
save(cohort_list,file = "db_cohortdata.RData")
rm(list=ls())

filenames <- dir(path = ".",pattern = "admixture.*.RData")
lapply(filenames,load,.GlobalEnv)
rm(filenames)
admixture <- sapply(ls(), function(x) get(x),USE.NAMES = TRUE)
save(admixture,file = "db_admixture.RData")
rm(list=ls())

filenames <- dir(path = ".",pattern = "pcaPOP.*.RData")
lapply(filenames,load,.GlobalEnv)
rm(filenames)
pca_data <- lapply(ls(), function(x) get(x))
names(pca_data) <- ls()[!ls() %in% c("pca_data")]
save(pca_data,file = "db_pca.RData")
rm(list=ls())
