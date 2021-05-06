# set path to include my libraries
.libPaths(c("/rds/general/user/ac1220/home/R/x86_64-redhat-linux-gnu-library/3.6",
            "/usr/lib64/R/library",
            "/usr/share/R/library" ))





#df_clean$PC1cat <- NULL
#df_clean$PC2cat <- NULL
#df_clean$PC3cat <- NULL
#df_clean$menopause_status_No <- NULL
#df_clean$breast_cancer_screening <- NULL

#colnames(df_clean)[which(names(df_clean) == "18000~30999")] <- "income_18000_30999"
#colnames(df_clean)[which(names(df_clean) == "31000~51999")] <- "income_31000_51999"
#colnames(df_clean)[which(names(df_clean) == ">52000")] <- "income_over_52000"
#colnames(df_clean)[which(names(df_clean) == "31000~51999")] <- "income_31000_51999"
#colnames(df_clean)[which(names(df_clean) == "bmi25-30")] <- "bmi25_30"
#colnames(df_clean)[which(names(df_clean) == "bmi30-40")] <- "bmi30_40"
#colnames(df_clean)[which(names(df_clean) == "bmi>40")] <- "bmi_over_40"
#colnames(df_clean)[which(names(df_clean) == "sleep7~8h")] <- "sleep_7_8h"
#colnames(df_clean)[which(names(df_clean) == "sleep>8h")] <- "sleep_over_8h"
#colnames(df_clean)[which(names(df_clean) == "num_livebirth=0")] <- "num_livebirth_0"
#colnames(df_clean)[which(names(df_clean) == "num_livebirth1~3")] <- "num_livebirth1_3"
#colnames(df_clean)[which(names(df_clean) == "stillbirth_status")] <- "stillbirth"

#dim(df_clean) #66019    65
#colSums(is.na(df_clean))
#colnames(df_clean)

#table(df_clean$bc_inner)
#table(df_clean$bc_outer)

#X_pooled <- df_clean
#X_pooled$outcome_status <- NULL
#X_pooled$bc_inner <- NULL
#X_pooled$bc_outer <- NULL
#dim(X_pooled) #62

#Y_pooled <- df_clean$outcome_status


#MyPLSDA_pooled <- plsda(X_pooled, Y_pooled, ncomp = 1)
#MyPLSDA_pooled$loadings$X
#MyPLSDA_pooled$loadings$Y 


#Groups = c("recruitment_age", "Black", "White" , "ethnic_others",
           
#           "tdi" , "own", "rent", "House", "Flat",
#           "18000~30999", "31000~51999", ">52000", "Retired","Unemployed","edu_Intermediate","edu_Low",
           
#           "breastfed_as_baby", "maternal_smoking", "num_moderate_activity","num_vigorous_activity", "sleep7~8h", "sleep>8h",  "insomnia_sometimes","insomnia_usually","drink_never", "drink_previous", "smoke_never", "smoke_previous",
 #          
 #          "standing_height", "weight", "waist_circumference", "hip_circumference","bmi25-30" ,"bmi30-40" , "bmi>40", "Cardiovascular", "Hypertension", "Diabetes" ,"Respiratory",
           
 #          "breast_cancer_screening", "family_bc_history",
 #          
#           "menarche_age", "menopause_status_Yes", "menopause_status_No" , "num_livebirth1~3", "num_livebirth=0",   "stillbirth_status","oc_status", "HRT_status",
           
  #         "bio_triglycerides", "bio_hba1c", "bio_glucose", "bio_HDL" , "bio_SHBG" ,"bio_IGF1",
  #         
   #        "ap_Nitrogen_dioxide", "ap_pm10", "ap_pm2.5", "np_ave_24.hour_sound_level", 
           
    #       "diet_PC1", "diet_PC2", "diet_PC3")


#X_pooled = X_pooled[, Groups]
#Xgroups = c(4,16, 28, 39, 41, 49, 55, 59)






#########0407 start from here!!!

project_path="/rds/general/project/hda_students_data/live/Group5/General/AC"
setwd(project_path)

library(mixOmics)
library(pheatmap)
library(tidyr)
library(ROSE)
library(sgPLS)
source("Scripts/pls_functions.R")

# Load  dataset
df_clean <- readRDS("0411_dummy.rds")

dim(df_clean)
colnames(df_clean)

##### undersample for sPLSDA calibration






#set.seed(1)

#res_splsda = CalibratesPLSDA(dataX = X_pooled_d_total, dataY = Y_pooled_d_total, ncomp = 1, Nrepeat = 100)


#pdf('0411_spls_plotcalib_downsample_overall.pdf')
#PlotCalib(res = res_splsda)
#dev.off()



#### sgPLS
Groups = c("recruitment_age", "ethnic_Asian", "ethnic_Black" , "ethnic_Others",
           
           "tdi" , "own_or_rent_rent", "accommodation_type_Flat", "accommodation_type_Care_home_others", "ave_total_household_income_18000_30999", 
           "ave_total_household_income_31000_51999", "ave_total_household_income_over_52000",
           "number_in_household_over_3","employment_Employed","employment_Retired","education_Intermediate","education_High",
           
           "breastfed_as_baby_Yes", "maternal_smoking_Yes", "num_moderate_activity_over_3","num_vigorous_activity_over_3", "sleep_duration_7_8h", "sleep_duration_over_8h",  
           "insomnia_Sometimes","insomnia_Usually","alcohol_drinker_Previous", 
           "alcohol_drinker_Current", "smoking_status_Previous", "smoking_status_Current","diet_PC1", "diet_PC2", "diet_PC3",
           
           "waist_circumference","BMI_25_30" ,"BMI_30_40" , "BMI_over_40", 
           "Cardiovascular_Yes", "Hypertension_Yes", "Diabetes_Yes" ,"Respiratory_Yes",
           
           "family_history_Yes", "menarche_age", "menopause_status_Yes", "live_births_num_1_3", "live_births_num_over_3", 
           "stillbirth_status_Yes","oc_status_Yes", "HRT_status_Yes",
           
           "bio_triglycerides", "bio_hba1c", "bio_glucose", "bio_HDL" , "bio_SHBG" ,"bio_IGF1",
           
           "ap_Nitrogen_oxides", "ap_pm10", "ap_pm2.5", "np_ave_24.hour_sound_level")




Xgroups = c(4,16, 31, 39, 47, 53)

# outcome_status
downsample_df <- ovun.sample(outcome_status ~ ., data = df_clean, p=0.5, method = "under", seed = 96)$data


print(table(downsample_df$outcome_status))


X_pooled_d_total <- downsample_df
X_pooled_d_total$outcome_status <- NULL
X_pooled_d_total$bc_inner <- NULL
X_pooled_d_total$bc_outer <- NULL
dim(X_pooled_d_total) #62

Y_pooled_d_total <- downsample_df$outcome_status
table(Y_pooled_d_total)


X_pooled_d_total = X_pooled_d_total[,Groups]


set.seed(1)
res_sgplsda = CalibratesgPLSDA(dataX = X_pooled_d_total, dataY = Y_pooled_d_total, ncomp = 1, Nrepeat = 100, Xgroups = Xgroups)

pdf('0413_sgpls_downsample_plotcalib.pdf')
PlotCalib(res = res_sgplsda, type = "sgPLSDA")
dev.off()


###bc_inner

downsample_df <- ovun.sample(bc_inner ~ ., data = df_clean, p=0.5, # N = 1000, 
                             method = "under", seed = 96)$data



X_pooled_d_inner <- downsample_df
X_pooled_d_inner$outcome_status <- NULL
X_pooled_d_inner$bc_inner <- NULL
X_pooled_d_inner$bc_outer <- NULL
dim(X_pooled_d_inner) #62

X_pooled_d_inner = X_pooled_d_inner[, Groups]


Y_pooled_d_inner <- downsample_df$bc_inner
table(Y_pooled_d_inner)


set.seed(1)
res_sgplsda = CalibratesgPLSDA(dataX = X_pooled_d_inner, dataY = Y_pooled_d_inner, ncomp = 1, Nrepeat = 100, Xgroups = Xgroups)

pdf('0413_sgpls_downsample_plotcalib_inner.pdf')
PlotCalib(res = res_sgplsda, type = "sgPLSDA")
dev.off()


#set.seed(1)

#res_splsda_inner = CalibratesPLSDA(dataX = X_pooled_d_inner, dataY = Y_pooled_d_inner,ncomp = 1, Nrepeat = 100)


#pdf('0411_spls_plotcalib_downsample_inner.pdf')
#PlotCalib(res = res_splsda_inner)
#dev.off()


###bc_outer

downsample_df <- ovun.sample(bc_outer ~ ., data = df_clean, p=0.5, # N = 1000, 
                             method = "under", seed = 96)$data



X_pooled_d_outer <- downsample_df
X_pooled_d_outer$outcome_status <- NULL
X_pooled_d_outer$bc_inner <- NULL
X_pooled_d_outer$bc_outer <- NULL
dim(X_pooled_d_outer) #62
X_pooled_d_outer = X_pooled_d_outer[, Groups]

Y_pooled_d_outer <- downsample_df$bc_outer
table(Y_pooled_d_outer)


set.seed(1)
res_sgplsda = CalibratesgPLSDA(dataX = X_pooled_d_outer, dataY = Y_pooled_d_outer, ncomp = 1, Nrepeat = 100, Xgroups = Xgroups)

pdf('0413_sgpls_downsample_plotcalib_outer.pdf')
PlotCalib(res = res_sgplsda, type = "sgPLSDA")
dev.off()



#set.seed(1)

#res_splsda_outer = CalibratesPLSDA(dataX = X_pooled_d_outer, dataY = Y_pooled_d_outer,ncomp = 1, Nrepeat = 100)


#pdf('0411_spls_plotcalib_downsample_outer.pdf')
#PlotCalib(res = res_splsda_outer)
#dev.off()






