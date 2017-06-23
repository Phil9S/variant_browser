library(ggplot2)
library(reshape2)
library(shiny)

h <- as.matrix(variant_data[,(ncol(variant_data)-nrow(sample_data)+1):ncol(variant_data)])
#h_melted <- melt(h)

h_fil <- h[1:10,]
h_drop <- h_fil[,apply(h_fil,2,function (x) sum(x == 0) == 10) == FALSE]
h_drop <- h_drop[,apply(h_drop,2,function (x) sum(x == -9) == 10) == FALSE]
h_melted <- melt(h_drop)

ggplot(data = h_melted, aes(x=h_melted$Var2, y=h_melted$Var1, fill=factor(value))) + 
      geom_tile(aes(width=0.95, height=0.95, text = h_melted$Var2), color="gray20", size=0.7) +
      coord_fixed(ratio = 1) +
      labs(fill='Genotype', x = "Sample", y = "Variant") +
      scale_y_reverse(expand = c(0,0),position="right",breaks=seq(1,10,by = 1)) +
      scale_x_discrete(position = "top",expand = c(0,0)) +
      scale_fill_manual(values=c("-9" = "gray80","0" = "white","1" = "cadetblue2","2" = "cadetblue4"),labels=c("Miss","Ref","Het","Hom")) +
      theme(
        legend.position="bottom",
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background =element_blank()
      )



