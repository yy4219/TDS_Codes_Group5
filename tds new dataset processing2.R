
# tds new dataset processing --- third time


remove(list=ls())

data <- readRDS("/dataset2/female_finaldataset_nodummy.rds")  #199279 obs.

# remove menopause_status==hysterectomy & NA
data1<- subset(data,menopause_status=="Yes"| menopause_status=="No") #166071 obs.


# follow up time 
# it drives me crazy. If I use as.numeric or as.character, it randomly produce so many NAs.
# let's remember this disaster mode : difftime 

t<-data1[order(data1$Follow_up_time),]
data2<-t[3350:166071,]   # remove days<0 and days<=365    #162722 obs.

# processing family history of breast cancer -- from chuyan dataset ---double checked 

chuyan <- readRDS("/dataset2/mydata_final_with_family_his.rds")
chuyan<- chuyan[,c("eid","family_history")]
#data3<- data2[,-grep("family_bc_history",colnames(data2))] # remove inaccurate bc_history
data3<-data2[,-84]
data3<-merge(data3,chuyan,by="eid",all.x=T)  # combine correct bc_history

# check missing values
miss<-function(x){
  sum(is.na(x))/length(x)
}

apply(data3,2,miss)


# remove variables include year_of_birth,sex(all females),moderate_activity_duration(27%),vigorous_activity_duration(49%)
# remove breast_cancer_screening(bias),bio_oestrodial(77%)
# remove variables that we will not use anymore like first.epi

data4<-data3[,-c(3,4,15,17,46,67,70:73)]


# although 4 variables with relatively higher missing proportion, they are associated with bc.  
# ave_total_household_income(17%),breastfed_as_baby(18%),maternal_smoking(12%),family_history(19%)
# remove cases with 3 of 4 and 4 of 4 variables above are missing, then do imputation. 
data5<-data4[,c(1,7,11,12,78)]
table(apply(data5,1,miss))

t<-apply(data5,1,miss)
ls<-which(t==0.8|t==0.6)  # discover 3 of 4 and 4 of 4 varibles are missing -- remove 6266 obs.
data6 <- data5[-ls,]     # 156456 obs. 
apply(data6,2,miss)    # missing proportion of 4 variables are lower-- around 15%. It's OK to further imputation.

data7<-data4[,-c(7,11,12,78)]
data8<-merge(data6,data7,by="eid",all.x=T)  


# subtypes for cancer
data8$bc_inner<-ifelse(data8$bc_C502==0&data8$bc_C503==0,0,1)  #inner
data8$bc_outer<-ifelse(data8$bc_C504==0&data8$bc_C505==0,0,1)  #outer
data8<-data8[,-c(66:74)]
colnames(data8)
apply(data8,2,miss)

# recode diet items 
#recode
#never--0
#Less than once a week--1
#Once a week--2
#2-4 times a week--3
#5-6 times a week--4
#Once or more daily--5


#oily_fish_intake
mydata_diet=data8
mydata_diet$oily_fish_intake<-ifelse(mydata_diet$oily_fish_intake=="Never",0,ifelse(mydata_diet$oily_fish_intake=="Less than once a week",1,
                                                                                    ifelse(mydata_diet$oily_fish_intake=="Once a week",2,
                                                                                           ifelse(mydata_diet$oily_fish_intake=="2-4 times a week",3,
                                                                                                  ifelse(mydata_diet$oily_fish_intake=="5-6 times a week",4,
                                                                                                         ifelse(mydata_diet$oily_fish_intake=="Once or more daily",5,NA))))))

table(mydata_diet$oily_fish_intake,exclude = F)

#non.oily_fish_intake
mydata_diet$non.oily_fish_intake<-ifelse(mydata_diet$non.oily_fish_intake=="Never",0,ifelse(mydata_diet$non.oily_fish_intake=="Less than once a week",1,
                                                                                            ifelse(mydata_diet$non.oily_fish_intake=="Once a week",2,
                                                                                                   ifelse(mydata_diet$non.oily_fish_intake=="2-4 times a week",3,
                                                                                                          ifelse(mydata_diet$non.oily_fish_intake=="5-6 times a week",4,
                                                                                                                 ifelse(mydata_diet$non.oily_fish_intake=="Once or more daily",5,NA))))))

table(mydata_diet$non.oily_fish_intake,exclude = F)

#processed_meat_intake
mydata_diet$processed_meat_intake<-ifelse(mydata_diet$processed_meat_intake=="Never",0,ifelse(mydata_diet$processed_meat_intake=="Less than once a week",1,
                                                                                              ifelse(mydata_diet$processed_meat_intake=="Once a week",2,
                                                                                                     ifelse(mydata_diet$processed_meat_intake=="2-4 times a week",3,
                                                                                                            ifelse(mydata_diet$processed_meat_intake=="5-6 times a week",4,
                                                                                                                   ifelse(mydata_diet$processed_meat_intake=="Once or more daily",5,NA))))))

table(mydata_diet$processed_meat_intake,exclude = F)

#poultry_intake
mydata_diet$poultry_intake<-ifelse(mydata_diet$poultry_intake=="Never",0,ifelse(mydata_diet$poultry_intake=="Less than once a week",1,
                                                                                ifelse(mydata_diet$poultry_intake=="Once a week",2,
                                                                                       ifelse(mydata_diet$poultry_intake=="2-4 times a week",3,
                                                                                              ifelse(mydata_diet$poultry_intake=="5-6 times a week",4,
                                                                                                     ifelse(mydata_diet$poultry_intake=="Once or more daily",5,NA))))))

table(mydata_diet$poultry_intake,exclude = F)

#beef_intake
mydata_diet$beef_intake<-ifelse(mydata_diet$beef_intake=="Never",0,ifelse(mydata_diet$beef_intake=="Less than once a week",1,
                                                                          ifelse(mydata_diet$beef_intake=="Once a week",2,
                                                                                 ifelse(mydata_diet$beef_intake=="2-4 times a week",3,
                                                                                        ifelse(mydata_diet$beef_intake=="5-6 times a week",4,
                                                                                               ifelse(mydata_diet$beef_intake=="Once or more daily",5,NA))))))

table(mydata_diet$beef_intake,exclude = F)


#lamb_intake
mydata_diet$lamb_intake<-ifelse(mydata_diet$lamb_intake=="Never",0,ifelse(mydata_diet$lamb_intake=="Less than once a week",1,
                                                                          ifelse(mydata_diet$lamb_intake=="Once a week",2,
                                                                                 ifelse(mydata_diet$lamb_intake=="2-4 times a week",3,
                                                                                        ifelse(mydata_diet$lamb_intake=="5-6 times a week",4,
                                                                                               ifelse(mydata_diet$lamb_intake=="Once or more daily",5,NA))))))

table(mydata_diet$lamb_intake,exclude = F)

#pork_intake
mydata_diet$pork_intake<-ifelse(mydata_diet$pork_intake=="Never",0,ifelse(mydata_diet$pork_intake=="Less than once a week",1,
                                                                          ifelse(mydata_diet$pork_intake=="Once a week",2,
                                                                                 ifelse(mydata_diet$pork_intake=="2-4 times a week",3,
                                                                                        ifelse(mydata_diet$pork_intake=="5-6 times a week",4,
                                                                                               ifelse(mydata_diet$pork_intake=="Once or more daily",5,NA))))))

table(mydata_diet$pork_intake,exclude = F)


#cheese_intake
mydata_diet$cheese_intake<-ifelse(mydata_diet$cheese_intake=="Never",0,ifelse(mydata_diet$cheese_intake=="Less than once a week",1,
                                                                              ifelse(mydata_diet$cheese_intake=="Once a week",2,
                                                                                     ifelse(mydata_diet$cheese_intake=="2-4 times a week",3,
                                                                                            ifelse(mydata_diet$cheese_intake=="5-6 times a week",4,
                                                                                                   ifelse(mydata_diet$cheese_intake=="Once or more daily",5,NA))))))

table(mydata_diet$cheese_intake,exclude = F)



#alcohol_intake_frequency

# Never--0
# Special occasions only--1
# One to three times a month--2
# Once or twice a week--3
# Three or four times a week--4
# Daily or almost daily--5
# Prefer not to answer, NA --NA   (this is why in data8, there is no missing values but in mydata_diet has) 

mydata_diet$alcohol_intake_frequency<-ifelse(mydata_diet$alcohol_intake_frequency=="Never",0,ifelse(mydata_diet$alcohol_intake_frequency=="Special occasions only",1,
                                                                                                    ifelse(mydata_diet$alcohol_intake_frequency=="One to three times a month",2,
                                                                                                           ifelse(mydata_diet$alcohol_intake_frequency=="Once or twice a week",3,
                                                                                                                  ifelse(mydata_diet$alcohol_intake_frequency=="Three or four times a week",4,
                                                                                                                         ifelse(mydata_diet$alcohol_intake_frequency=="Daily or almost daily",5,NA))))))

table(mydata_diet$alcohol_intake_frequency,exclude = F)




# imputation for diet items and biomarkers first
# after imputation, diet items can be changed into PCs
# why not do imputation for all variables£¿ 
#1. too many variables, too much time 
#2. imputation not work well for collinearity variables, and collinearity variables we may remove later
library(mice)
data9<-mydata_diet
imputed <- mice(data9[,c(18:34,36,59:64)],
                m=1, method = 'pmm', seed = 500) # only imputation once 


complete <- complete(imputed,1)    # -- choose  imputation

data9[,c(18:34,36,59:64)]<-complete

apply(data9,2,miss)

# diet factors
# Use SPSS and SAS software to do PCA and combine diet score with original dataset.
# It is quite complex using R since doing pca without eid. It's hard to match for each obs. 
# I also tried to use rownames, but didn't work. Some cases always didn't match.

write.csv(x = data9,file = "/dataset2/data1.csv") # export dataset by using SAS and SPSS


# Notice: In order to confirm, I still compare results by using SPSS, SAS and R. 
# Luckily, it's equal.  

# calculate PCA 
data10<-data9[,c(18:34,36)]  #extract diet items


library(psych)
diet.pca<-principal(data10,nfactors=10,rotate="none",scores=T)
diet.pca

# scree plot 
library(factoextra)
diet.pca2<-prcomp(data10,scale=T,center = T)
fviz_eig(diet.pca2,addlabels=T,ylim=c(0,18))


#PC score
score<-diet.pca$scores[,1:3]

score




#/////////////////////////////////////////////////////////////////////
remove(list=ls())
data11<-read.csv("/dataset2/data2.csv") 
data11<-data11[,-c(1,19:35,37)]  # remove diet items and keep PCs

miss<-function(x){
  sum(is.na(x))/length(x)
}

apply(data11,2,miss)
str(data11)

## convert character variables to factor
library(dplyr)

data11<-data11 %>% mutate_if(is.character, as.factor) 


data11$outcome_status<-as.factor(data11$outcome_status)
data11$Cardiovascular<-as.factor(data11$Cardiovascular)
data11$Hypertension<-as.factor(data11$Hypertension)
data11$Diabetes<-as.factor(data11$Diabetes)
data11$Respiratory<-as.factor(data11$Respiratory)
data11$bc_inner<-as.factor(data11$bc_inner)
data11$bc_outer<-as.factor(data11$bc_outer)
data11$family_history<-as.factor(data11$family_history)

str(data11)
apply(data11,2,miss)


# before imputation for other variables, we check collinearity first. 
# in order to do impuatation easily and quickly.
# so, we use heatmap to check collinearity

df<-na.omit(data11)


#categorical -> dummy variables
xfactors<-model.matrix(~ave_total_household_income+breastfed_as_baby+maternal_smoking+family_history+own_or_rent+accommodation_type+number_in_household
                       +employment+education+ethnic
                       +num_moderate_activity+num_vigorous_activity+sleep_duration
                       +insomnia+alcohol_drinker+smoking_status+BMI+menopause_status
                       +live_births_num+stillbirth_status+oc_status+HRT_status
                       +Cardiovascular+Hypertension+Diabetes+Respiratory,data=df)


xfactors_nointecept<-xfactors[,-1]

df<-cbind(xfactors_nointecept,df[c(6,7,20:22,24,25,31:46,54:56)])


# heatmap
library("pheatmap")
corr<-cor(df, method = "spearman")
pheatmap(corr)


# according to heatmap, remove strong blocks 
# remove ap_Nitrogen_oxides,ap_2.5.10um,np_ave_daytime_sound_level,np_ave_evening_sound_level,np_ave_night.time_sound_level,np_ave_16.hour_sound_level
# remove standing_height,weight,hip_circumference

data12<-data11[,-c(20,22,24,31,35,36:39)]
str(data12)
apply(data12,2,miss)

# imputation
# like other group, we accept missing proportion 15%.
library(mice)

# imputation for variables --- "pmm" can be used for any datatype
# only include incomplete variables
imputed <- mice(data12[,c(2:5,7:12,14:22,24:31)],
                m=1, method = 'pmm', seed = 500) # only imputation once 


complete <- complete(imputed,1)    # -- choose  imputation

data12[,c(2:5,7:12,14:22,24:31)]<-complete

apply(data12,2,miss)

str(data12)

# do heatmap again 

xfactors1<-model.matrix(~ave_total_household_income+breastfed_as_baby+maternal_smoking+family_history+own_or_rent+accommodation_type+number_in_household
                       +employment+education+ethnic
                       +num_moderate_activity+num_vigorous_activity+sleep_duration
                       +insomnia+alcohol_drinker+smoking_status+BMI+menopause_status
                       +live_births_num+stillbirth_status+oc_status+HRT_status
                       +Cardiovascular+Hypertension+Diabetes+Respiratory,data=data12)


xfactors_nointecept1<-xfactors1[,-1]

df1<-cbind(xfactors_nointecept1,data12[c(6,7,20,22,28:37,45:47)])


# heatmap
library("pheatmap")
corr1<-cor(df1, method = "spearman")
pheatmap(corr1)

# # adjust levels and final polish
data12$accommodation_type<-factor(data12$accommodation_type,levels = c("House","Flat","Care home/temporary or sheltered"))
data12$number_in_household<-factor(data12$number_in_household,levels = c("1~2",">=3"))
data12$employment<-factor(data12$employment,levels = c("Unemployed","Employed","Retired"))
data12$education<-factor(data12$education,levels = c("Low","Intermediate","High"))
data12$ethnic<-factor(data12$ethnic,levels = c("White","Asian","Black","Others"))
data12$sleep_duration<-factor(data12$sleep_duration,levels = c("7~8","<7",">8"))
data12$alcohol_drinker<-factor(data12$alcohol_drinker,levels = c("Never","Previous","Current"))
data12$smoking_status<-factor(data12$smoking_status,levels = c("Never","Previous","Current"))
data12$BMI<-factor(data12$BMI,levels = c("<25","25-30","30-40",">40"))
data12$live_births_num<-factor(data12$live_births_num,levels = c("0","1~3",">3"))
data12$ave_total_household_income<-factor(data12$ave_total_household_income,levels = c("<18000","18000~30999","31000~51999",">52000"))

levels(data12$family_history)[1]<-"No"
levels(data12$family_history)[2]<-"Yes"

levels(data12$Cardiovascular)[1]<-"No"
levels(data12$Cardiovascular)[2]<-"Yes"

levels(data12$Hypertension)[1]<-"No"
levels(data12$Hypertension)[2]<-"Yes"

levels(data12$Diabetes)[1]<-"No"
levels(data12$Diabetes)[2]<-"Yes"

levels(data12$Respiratory)[1]<-"No"
levels(data12$Respiratory)[2]<-"Yes"

str(data12)
summary(data12)
apply(data12,2,miss)


# save dataset
saveRDS(data12,"/dataset2/mydata.rds")
