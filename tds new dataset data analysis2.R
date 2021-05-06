remove(list=ls())

data <- readRDS("/dataset2/mydata.rds")

## lasso
library(tidyverse)
library(caret)
library(glmnet)
library(ROCR)

#categorical -> dummy variables
xfactors<-model.matrix(~ave_total_household_income+breastfed_as_baby+maternal_smoking+family_history+own_or_rent+accommodation_type+number_in_household
                       +employment+education+ethnic
                       +num_moderate_activity+num_vigorous_activity+sleep_duration
                       +insomnia+alcohol_drinker+smoking_status+BMI+menopause_status
                       +live_births_num+stillbirth_status+oc_status+HRT_status
                       +Cardiovascular+Hypertension+Diabetes+Respiratory,data=data)

dim(xfactors)
head(xfactors)

xfactors_nointecept<-xfactors[,-1]

 # add outcome to do cross validation
df<-cbind(xfactors_nointecept,data[c(6,7,20,22,28:37,45:47)])

# standalization -- PCs have already scaled when doing pca
df[c(41:54)] <- lapply(df[c(41:54)], function(x) c(scale(x,center = T,scale = T)))



X<-as.matrix(df)
Y_overall<-as.matrix(data[,"outcome_status"])
Y_inner<-as.matrix(data[,"bc_inner"])
Y_outer<-as.matrix(data[,"bc_outer"])

# stability  analysis ####   can't running on my pc!!! 

X<-as.matrix(df)
Y_overall<-as.matrix(data[,"outcome_status"])
Y_inner<-as.matrix(data[,"bc_inner"])
Y_outer<-as.matrix(data[,"bc_outer"])

LassoSub = function(k = 1, Xdata, Ydata, family = "binomial",
                    penalty.factor = NULL) {
  if (is.null(penalty.factor)) {
    penalty.factor = rep(1, ncol(Xdata))
  }
  set.seed(k)
  s = sample(nrow(Xdata), size = 0.8 * nrow(Xdata))
  Xsub = Xdata[s, ]
  Ysub = Ydata[s]
  model.sub = cv.glmnet(x = Xsub, y = Ysub, alpha = 1,
                        family = family, penalty.factor = penalty.factor)
  coef.sub = coef(model.sub, s = "lambda.1se")[-1]  # lambda.min or lambda.1se
  return(coef.sub)
}
niter = 100

# overall  
lasso.stab1 = sapply(1:niter, FUN = LassoSub, Xdata = X,
                    Ydata = Y_overall)
# The proportion of selection of each variable
lasso.prop1 = apply(lasso.stab1, 1, FUN = function(x) {
  sum(x != 0)/length(x)
})
names(lasso.prop1) = colnames(X)


# inner
lasso.stab2 = sapply(1:niter, FUN = LassoSub, Xdata = X,
                    Ydata = Y_inner)
lasso.prop2 = apply(lasso.stab2, 1, FUN = function(x) {
  sum(x != 0)/length(x)
})
names(lasso.prop2) = colnames(X)

# bouter
lasso.stab3 = sapply(1:niter, FUN = LassoSub, Xdata = X,
                    Ydata = Y_outer)
lasso.prop3 = apply(lasso.stab3, 1, FUN = function(x) {
  sum(x != 0)/length(x)
})
names(lasso.prop3) = colnames(X)






######################################################################################################################


# ROC curve 

# overall
remove(list=ls())
data <- readRDS("/dataset2/mydata.rds")

## lasso
library(tidyverse)
library(caret)
library(glmnet)
library(ROCR)

#categorical -> dummy variables
xfactors<-model.matrix(~ave_total_household_income+breastfed_as_baby+maternal_smoking+family_history+own_or_rent+accommodation_type+number_in_household
                       +employment+education+ethnic
                       +num_moderate_activity+num_vigorous_activity+sleep_duration
                       +insomnia+alcohol_drinker+smoking_status+BMI+menopause_status
                       +live_births_num+stillbirth_status+oc_status+HRT_status
                       +Cardiovascular+Hypertension+Diabetes+Respiratory,data=data)

dim(xfactors)
head(xfactors)

xfactors_nointecept<-xfactors[,-1]

# add outcome to do cross validation
df<-cbind(xfactors_nointecept,data[c(6,7,20,22,28:37,45:47,38)])

# standalization -- PCs have already scaled when doing pca
df[c(41:54)] <- lapply(df[c(41:54)], function(x) c(scale(x,center = T,scale = T)))



set.seed(123) 
train<-sample(seq(1,dim(df)[1]),floor(0.8*dim(df)[1]))
test<-seq(1,dim(df)[1])[-train]
data_train<-df[train,]
data_test<-df[test,]

X_train<-as.matrix(data_train[,-grep("outcome_status",colnames(data_train))])
Y_train<-as.matrix(data_train[,"outcome_status"])

X_test<-as.matrix(data_test[,-grep("outcome_status",colnames(data_train))])
Y_test<-as.matrix(data_test[,"outcome_status"])

sum(is.na(X_train))
sum(is.na(Y_train))
sum(is.na(X_test))
sum(is.na(Y_test))

#model1 <- cv.glmnet(x=X_train, y=Y_train, alpha = 1,family = "binomial",type.measure="deviance")
#plot(model1)
#cbind(coef(model1, model1$lambda.min),coef(model1, model1$lambda.1se))


model2 <- cv.glmnet(x=X_train, y=Y_train, alpha = 1,family = "binomial",type.measure="auc")
plot(model2)
cbind(coef(model2, model2$lambda.min),coef(model2, model2$lambda.1se))


predict_lasso<-predict(model2,newx=X_test,type="response",s=model2$lambda.1se)
pr_lasso<-prediction(predict_lasso,Y_test)
prf_lasso<-performance(pr_lasso,measure="tpr",x.measure="fpr")
auc_lasso<-performance(pr_lasso,measure="auc")
auc_lasso<-auc_lasso@y.values[[1]]

# ROC curve
plot(prf_lasso,col="tomato")
abline(a=0,b=1,lty="dashed",col="green")
legend("bottomright",legend=paste("LASSO, AUC= ",round(auc_lasso,digits = 3)),bty="n",col="tomato",lty=1,cex=0.7)


# predict_lasso<-predict(model2,newx=X_test,type="response",s=model2$lambda.min)
# pr_lasso<-prediction(predict_lasso,Y_test)
# prf_lasso<-performance(pr_lasso,measure="tpr",x.measure="fpr")
# auc_lasso<-performance(pr_lasso,measure="auc")
# auc_lasso<-auc_lasso@y.values[[1]]

# plot(prf_lasso,col="tomato")
# abline(a=0,b=1,lty="dashed",col="green")
# legend("bottomright",legend=paste("LASSO, AUC= ",round(auc_lasso,digits = 3)),bty="n",col="tomato",lty=1,cex=0.7)

#visualize non-zero coefficients
par(mar=c(9,4,1,2))
betas=coef(model2,s="lambda.1se")[-1]
names(betas)=rownames(coef(model2,s="lambda.1se"))[-1]
plot(betas[betas!=0],type="h",col="navy",lwd=3,xaxt="n",xlab="",ylab = expression(beta))
axis(side=1,at=1:sum(betas!=0),labels=names(betas)[betas!=0],las=2,cex.axis=0.8)
abline(h=0,lty=2)




# /////// bc-inner /////////////////////////////

remove(list=ls())
data <- readRDS("/dataset2/mydata.rds")

library(tidyverse)
library(caret)
library(glmnet)
library(ROCR)

xfactors<-model.matrix(~ave_total_household_income+breastfed_as_baby+maternal_smoking+family_history+own_or_rent+accommodation_type+number_in_household
                       +employment+education+ethnic
                       +num_moderate_activity+num_vigorous_activity+sleep_duration
                       +insomnia+alcohol_drinker+smoking_status+BMI+menopause_status
                       +live_births_num+stillbirth_status+oc_status+HRT_status
                       +Cardiovascular+Hypertension+Diabetes+Respiratory,data=data)

xfactors_nointecept<-xfactors[,-1]

df<-cbind(xfactors_nointecept,data[c(6,7,20,22,28:37,45:47,43)])
df[c(41:54)] <- lapply(df[c(41:54)], function(x) c(scale(x,center = T,scale = T)))

set.seed(123) 
train<-sample(seq(1,dim(df)[1]),floor(0.8*dim(df)[1]))
test<-seq(1,dim(df)[1])[-train]
data_train<-df[train,]
data_test<-df[test,]

X_train<-as.matrix(data_train[,-grep("bc_inner",colnames(data_train))])
Y_train<-as.matrix(data_train[,"bc_inner"])

X_test<-as.matrix(data_test[,-grep("bc_inner",colnames(data_train))])
Y_test<-as.matrix(data_test[,"bc_inner"])

sum(is.na(X_train))
sum(is.na(Y_train))

model2 <- cv.glmnet(x=X_train, y=Y_train, alpha = 1,family = "binomial",type.measure="auc")
plot(model2)
cbind(coef(model2, model2$lambda.min),coef(model2, model2$lambda.1se))

predict_lasso<-predict(model2,newx=X_test,type="response",s=model2$lambda.1se)
pr_lasso<-prediction(predict_lasso,Y_test)
prf_lasso<-performance(pr_lasso,measure="tpr",x.measure="fpr")
auc_lasso<-performance(pr_lasso,measure="auc")
auc_lasso<-auc_lasso@y.values[[1]]


plot(prf_lasso,col="tomato")
abline(a=0,b=1,lty="dashed",col="green")
legend("bottomright",legend=paste("LASSO, AUC= ",round(auc_lasso,digits = 3)),bty="n",col="tomato",lty=1,cex=0.7)


par(mar=c(9,4,1,2))
betas=coef(model2,s="lambda.1se")[-1]
names(betas)=rownames(coef(model2,s="lambda.1se"))[-1]
plot(betas[betas!=0],type="h",col="navy",lwd=3,xaxt="n",xlab="",ylab = expression(beta))
axis(side=1,at=1:sum(betas!=0),labels=names(betas)[betas!=0],las=2,cex.axis=0.8)
abline(h=0,lty=2)

# /////// bc-outer /////////////////////////////

remove(list=ls())
data <- readRDS("/dataset2/mydata.rds")

library(tidyverse)
library(caret)
library(glmnet)
library(ROCR)

xfactors<-model.matrix(~ave_total_household_income+breastfed_as_baby+maternal_smoking+family_history+own_or_rent+accommodation_type+number_in_household
                       +employment+education+ethnic
                       +num_moderate_activity+num_vigorous_activity+sleep_duration
                       +insomnia+alcohol_drinker+smoking_status+BMI+menopause_status
                       +live_births_num+stillbirth_status+oc_status+HRT_status
                       +Cardiovascular+Hypertension+Diabetes+Respiratory,data=data)

xfactors_nointecept<-xfactors[,-1]

df<-cbind(xfactors_nointecept,data[c(6,7,20,22,28:37,45:47,44)])
df[c(41:54)] <- lapply(df[c(41:54)], function(x) c(scale(x,center = T,scale = T)))

set.seed(123) 
train<-sample(seq(1,dim(df)[1]),floor(0.8*dim(df)[1]))
test<-seq(1,dim(df)[1])[-train]
data_train<-df[train,]
data_test<-df[test,]

X_train<-as.matrix(data_train[,-grep("bc_outer",colnames(data_train))])
Y_train<-as.matrix(data_train[,"bc_outer"])

X_test<-as.matrix(data_test[,-grep("bc_outer",colnames(data_train))])
Y_test<-as.matrix(data_test[,"bc_outer"])

sum(is.na(X_train))
sum(is.na(Y_train))

model2 <- cv.glmnet(x=X_train, y=Y_train, alpha = 1,family = "binomial",type.measure="auc")
plot(model2)
cbind(coef(model2, model2$lambda.min),coef(model2, model2$lambda.1se))

predict_lasso<-predict(model2,newx=X_test,type="response",s=model2$lambda.1se)
pr_lasso<-prediction(predict_lasso,Y_test)
prf_lasso<-performance(pr_lasso,measure="tpr",x.measure="fpr")
auc_lasso<-performance(pr_lasso,measure="auc")
auc_lasso<-auc_lasso@y.values[[1]]


plot(prf_lasso,col="tomato")
abline(a=0,b=1,lty="dashed",col="green")
legend("bottomright",legend=paste("LASSO, AUC= ",round(auc_lasso,digits = 3)),bty="n",col="tomato",lty=1,cex=0.7)


par(mar=c(9,4,1,2))
betas=coef(model2,s="lambda.1se")[-1]
names(betas)=rownames(coef(model2,s="lambda.1se"))[-1]
plot(betas[betas!=0],type="h",col="navy",lwd=3,xaxt="n",xlab="",ylab = expression(beta))
axis(side=1,at=1:sum(betas!=0),labels=names(betas)[betas!=0],las=2,cex.axis=0.8)
abline(h=0,lty=2)