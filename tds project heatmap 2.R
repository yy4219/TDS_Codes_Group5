# heatmap and correlation
library(openxlsx)
library("gplots")
library("pheatmap")
#
rm(list=ls())
data=readRDS("/datasets/dummy_diet_subtype.rds")
str(data)


# character ¡úfactor ¡únumeric
#own or rent
table(data$own_or_rent,exclude = F)
t1<-as.factor(data$own_or_rent)
table(t1,exclude = F)
t2<-as.numeric(t1)
table(t2,exclude = F)  #own=1, rent=2
data$own_or_rent<-t2


#breastfed_as_baby 
table(data$breastfed_as_baby,exclude = F)
t1<-as.factor(data$breastfed_as_baby)
table(t1,exclude = F)
t2<-as.numeric(t1)
table(t2,exclude = F)  #No=1, Yes=2
data$breastfed_as_baby<-t2

#maternal_smoking
table(data$maternal_smoking,exclude = F)
t1<-as.factor(data$maternal_smoking)
table(t1,exclude = F)
t2<-as.numeric(t1)
table(t2,exclude = F)  #No=1, Yes=2
data$maternal_smoking<-t2

#moderate_activity_duration
table(data$moderate_activity_duration,exclude = F)
t1<-as.factor(data$moderate_activity_duration)
table(t1,exclude = F)
t2<-as.numeric(t1)
table(t2,exclude = F)  #<20=1, >=20=2
data$moderate_activity_duration<-t2

#num_moderate_activity
table(data$num_moderate_activity,exclude = F)
t1<-as.factor(data$num_moderate_activity)
table(t1,exclude = F)
t2<-as.numeric(t1)
table(t2,exclude = F)  #<3=1, >=3=2
data$num_moderate_activity<-t2


#####
#vigorous_activity_duration
table(data$vigorous_activity_duration,exclude = F)
t1<-as.factor(data$vigorous_activity_duration)
table(t1,exclude = F)
t2<-as.numeric(t1)
table(t2,exclude = F)  #<20=1, >=20=2
data$vigorous_activity_duration<-t2

#num_vigorous_activity
table(data$num_vigorous_activity,exclude = F)
t1<-as.factor(data$num_vigorous_activity)
table(t1,exclude = F)
t2<-as.numeric(t1)
table(t2,exclude = F)  #<3=1, >=3=2
data$num_vigorous_activity<-t2

#PC1cat
table(data$PC1cat,exclude = F)
t1<-as.factor(data$PC1cat)
table(t1,exclude = F)
t2<-as.numeric(t1)
table(t2,exclude = F)  #High=1, Low=2
data$PC1cat<-t2

#PC2cat
table(data$PC2cat,exclude = F)
t1<-as.factor(data$PC2cat)
table(t1,exclude = F)
t2<-as.numeric(t1)
table(t2,exclude = F)  #High=1, Low=2
data$PC2cat<-t2

#PC3cat
table(data$PC3cat,exclude = F)
t1<-as.factor(data$PC3cat)
table(t1,exclude = F)
t2<-as.numeric(t1)
table(t2,exclude = F)  #High=1, Low=2
data$PC3cat<-t2

#breast_cancer_screening
table(data$breast_cancer_screening,exclude = F)
t1<-as.factor(data$breast_cancer_screening)
table(t1,exclude = F)
t2<-as.numeric(t1)
table(t2,exclude = F)  #No=1, Yes=2
data$breast_cancer_screening<-t2

#stillbirth_status
table(data$stillbirth_status,exclude = F)
t1<-as.factor(data$stillbirth_status)
table(t1,exclude = F)
t2<-as.numeric(t1)
table(t2,exclude = F)  #No=1, Yes=2
data$stillbirth_status<-t2

#oc_status
table(data$oc_status,exclude = F)
t1<-as.factor(data$oc_status)
table(t1,exclude = F)
t2<-as.numeric(t1)
table(t2,exclude = F)  #No=1, Yes=2
data$oc_status<-t2

#HRT_status
table(data$HRT_status,exclude = F)
t1<-as.factor(data$HRT_status)
table(t1,exclude = F)
t2<-as.numeric(t1)
table(t2,exclude = F)  #No=1, Yes=2
data$HRT_status<-t2

saveRDS(data,"E:/tds/datasets/dummy_numeric_diet_subtype.rds")


# calculate spearman correlation and produce simple heatmap plot
rm(list=ls())
data=readRDS("/datasets/dummy_numeric_diet_subtype2.rds")
str(data)
colnames(data)

data$own<-ifelse(data$own_or_rent==0,1,ifelse(data$own_or_rent==1,0,NA))
data$rent<-ifelse(data$own_or_rent==1,1,ifelse(data$own_or_rent==0,0,NA))
data$PC1cat<-ifelse(data$PC1cat==2,1,ifelse(data$PC1cat==1,0,NA))
data$PC2cat<-ifelse(data$PC2cat==2,1,ifelse(data$PC2cat==1,0,NA))
data$PC3cat<-ifelse(data$PC3cat==2,1,ifelse(data$PC3cat==1,0,NA))
  
heatmap_data<-data[,-c(14,77:79)]
corr<-cor(heatmap_data, method = "spearman", use = "complete.obs")
pheatmap(corr)

# produce advanced correlation plot 
install.packages("ggcorrplot")
install.packages("ggthemes")
library(ggcorrplot)
library(ggthemes)

corr<-cor(heatmap_data, method = "spearman", use = "complete.obs")
corr_p<-cor_pmat(heatmap_data, method = "spearman", use = "complete.obs")
ggcorrplot(corr,method = "circle",
           hc.order=T, hc.method = "ward.D",
           outline.color = "white", ggtheme = theme_bw(),
           colors = c("#6D9EC1","white","#E46726"),
           #lab = T, lab_size = 2,
           p.mat=corr_p, insig = "blank")

library(tidyverse)
new_data<-data%>%select(c(1:13,80,81,15:79))
colnames(new_data)

saveRDS(new_data,"/datasets/dummy_numeric_diet_subtype3.rds")
