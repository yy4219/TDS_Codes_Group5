   #PCA

# import data


df=readRDS("/Users/yyang/data.rds")

df.pca<-prcomp(df[,],scale=T)

#loadings
loadings<-df.pca$rotation


scores<-diet.pca$x    
scores

install.packages("factoextra")
library(factoextra)

fviz_eig(diet.pca,addlabels=T,ylim=c(0,20))

eig.val<-get_eigenvalue(diet.pca)
eig.val
