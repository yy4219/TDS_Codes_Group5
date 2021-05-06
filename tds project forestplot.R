# forest plot
install.packages("readxl")
library(ggplot2)
library(readxl)
install.packages("qqman")
library(qqman)

rm(list=ls())
OR_data <- data.frame(read_excel("/Users/yyang/forest_overall.xlsx"))

p = ggplot(data=OR_data,
           aes(x = variables,y = OR, ymin = L95, ymax = U95, shape=model ))+
  geom_pointrange(aes(col=group))+
  geom_hline(aes(fill=variables),yintercept =1, linetype=2)+
  xlab('variables')+ ylab("Odds Ratio (95% Confidence Interval)")+
  geom_errorbar(aes(ymin=L95, ymax=U95,col=group),width=0.2,cex=1)+ 
  facet_wrap(~group,strip.position="top",nrow=1,scales = "free_x") +
  theme(plot.title=element_text(size=16,face="bold"),
        axis.text.x=element_text(angle = 90, hjust = 0.5, vjust = 0.5,face="bold"),
        axis.title=element_text(size=12,face="bold"))+
  scale_y_log10(breaks=c(0.5,1,2))
p
