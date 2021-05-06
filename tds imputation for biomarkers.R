#imputation for biomarkers

rm(list=ls())

mydata=readRDS("/tds/datasets/dummy_numeric_diet_subtype2.rds")

colnames(mydata)

biomarkers<-mydata[,c(48:53)]

head(biomarkers)

# calculate missing proportion
miss<-function(x){
  sum(is.na(x))/length(x)
 
  }

apply(biomarkers,2,miss)

head(apply(biomarkers,1,miss),30)



list <-which(rowSums(is.na(biomarkers)) > 0)  #find rows with NAs
biomarkers_NA <- biomarkers[list,]            #extract rows with NAs
dim(biomarkers_NA)                            #see how many rows with NAs


## mice package: Multivariate Imputation by Chained Equations
install.packages("mice")
library(mice)
md.pattern(biomarkers)

# visualize the missing data
install.packages("VIM")
library(VIM)
miss_plot <- aggr(biomarkers, col=c('navyblue','yellow'),
                                      numbers=TRUE, sortVars=TRUE,
                                      labels=names(biomarkers), cex.axis=.7,
                                      gap=3, ylab=c("Missing data","Pattern"))
#imputation
imputed_Data <- mice(biomarkers, m=5, maxit = 50, method = 'pmm', seed = 500) # imputation 5 times
stripplot(imputed_Data, col=c("grey",mdc(2)),pch=c(1,20))

completeData <- complete(imputed_Data,2) # 2-- choose second times imputation

head(completeData)
head(biomarkers)

mydata[,c(48:53)]<-completeData

saveRDS(mydata,"/tds/datasets/dummy_numeric_diet_subtype2_bioimpu.rds")


