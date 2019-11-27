rm(list = ls())
## Enable cmd line args
args = commandArgs(trailingOnly=TRUE)

if(!require(ggplot2)){
  install.packages("ggplot2")
}
if(!require(reshape2)){
  install.packages("reshape2")
}

if(!require(RColorBrewer)){
  install.packages("RColorBrewer")
}

cohort <- args[1]
q.table <- read.table(paste(args[1],"_REF.maf0.05.5.Q",sep=""))
#colnames(q.table) <- c("SAS","EUR","EAS","AFR","AMR")

sample.table <- read.table("sample_list_pop")
colnames(sample.table) <- c("Sample")

pop.table <- read.table("/home/pss41/G1K_vcf/1KG_samplePopulations.tsv",header = T)

plot.table <- cbind(sample.table,q.table)

merge.table <- merge(plot.table,pop.table[,c(2,4)],by = "Sample",all.x = T,all.y = F,sort = F)
merge.table$pred <- apply(merge.table[2:6],1,function(x) names(which.max(x)))

counts <- lapply(sort(unique(merge.table$pred)), function(x){print(table(merge.table$Super_population[merge.table$pred == x]))})
countsnames <- unlist(lapply(counts, function(x){names(which(x == max(x)))}))

merge.table <- merge.table[with(merge.table, order(pred)),]
merge.table$Sample <- factor(merge.table$Sample,levels = merge.table$Sample)

merge.table.cases <- merge.table[is.na(merge.table$Super_population),]

merge.table.cases$pred <- ifelse(merge.table.cases$pred == "V1",countsnames[1],
                                             ifelse(merge.table.cases$pred == "V2",countsnames[2],
                                             ifelse(merge.table.cases$pred == "V3",countsnames[3],
                                             ifelse(merge.table.cases$pred == "V4",countsnames[4],
                                             ifelse(merge.table.cases$pred == "V5",countsnames[5],NA)))))
colnames(merge.table.cases) <- c("Sample",countsnames,"Super_population","pred")
merge.table.cases <- merge.table.cases[c("Sample","EUR","AFR","SAS","EAS","AMR","Super_population","pred")]

write.table(merge.table.cases[,-7],paste("admixture.",args[1],".tsv",sep=""),sep = "\t",quote = F,col.names = T,row.names = F)

melt.table <- melt(merge.table.cases,id.vars = c("Sample","Super_population","pred"))
names(melt.table) <- c("Sample","SuperPop","Pred","Population","Admixture")

P <-  ggplot(melt.table, aes(x = Sample, y = Admixture, fill = Population)) + 
              geom_bar(stat = "identity") + 
              ggtitle("Admixture population proportions") +
              scale_fill_brewer(palette = "Set1") +
              theme(panel.grid = element_blank(),panel.background = element_blank(),axis.line = element_line(),axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
              scale_y_continuous(expand = c(0,0)) +
              scale_x_discrete(expand = c(0,0))

png(paste("admixture.",args[1],".png",sep=""),width = 12,height = 3,units = "in",res = 600)
P
dev.off()

data.json <- t(merge.table.cases[,1:6])
data.json <- cbind(rownames(data.json),data.json)
assign(cohort,data.json)
rm(args,counts,merge.table,pop.table,countsnames,merge.table.cases,q.table,data.json,plot.table,sample.table,melt.table,P)

save(list = setdiff(ls(), "cohort"),file = paste("admixture.",cohort,".RData",sep = ""))