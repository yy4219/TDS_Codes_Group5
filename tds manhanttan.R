# Manhattan Plots


rm(list=ls())

library(openxlsx)
library(ggplot2)
library(tidyverse)

#  import data and takes p values from results 



results=read.xlsx("/tds/Manhattan and forest/Manhattan_overall.xlsx")
results$pval<-as.numeric(results$pval)
results$model<-as.factor(results$model)


# calcualte bonferroni
bonf <- 0.05/length(unique(results$variables))
bonf_log <- -log10(bonf)


# Manhattan plot 

ggplot(results, aes(y = -log10(pval), x = variables)) +   
  geom_point(aes(color=model))+ geom_hline(yintercept = bonf_log, color="red", linetype="dashed")+
  theme(text = element_text(size=8),
        axis.text.x = element_text(angle = 75, hjust = 1))+
  xlab("Predictors")+
  ylab("-log10(P value)")+
  ggtitle("Univariate results of all selected covariates")+
  facet_grid(.~group, scales = "free", switch = "x")+ggsave("/Users/yyang/Manhattan.png",
                                                                   width = 1280/72, height = 800/72, dpi = 72)




