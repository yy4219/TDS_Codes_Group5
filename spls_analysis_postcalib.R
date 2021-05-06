# set path to include my libraries
.libPaths(c("/rds/general/user/ac1220/home/R/x86_64-redhat-linux-gnu-library/3.6",
            "/usr/lib64/R/library",
            "/usr/share/R/library" ))




library(mixOmics)
library(pheatmap)
library(sgPLS)
library(ROSE)


project_path="/rds/general/project/hda_students_data/live/Group5/General/AC"
setwd(project_path)

source("Scripts/pls_functions.R")


df_clean <- readRDS("0411_dummy.rds")

#df_clean <- df[complete.cases(df), ]
#df_clean$PC1cat <- NULL
#df_clean$PC2cat <- NULL
#df_clean$PC3cat <- NULL
#df_clean$menopause_status_No <- NULL
#df_clean$breast_cancer_screening <- NULL

#dim(df_clean) #66019    65
#colSums(is.na(df_clean))
#colnames(df_clean)

#table(df_clean$bc_inner)
#table(df_clean$bc_outer)

X_pooled <- df_clean
X_pooled$outcome_status <- NULL
X_pooled$bc_inner <- NULL
X_pooled$bc_outer <- NULL
dim(X_pooled) #60

Y_pooled <- df_clean$outcome_status
Y_pooled_inner <- df_clean$bc_inner
Y_pooled_outer <- df_clean$bc_outer

#0405_removed breast cancer screening
#0405_changes grouping: family history to health risk

#Groups = c("recruitment_age", "Black", "White" , "ethnic_others",
#           "tdi" , "own", "rent", "House", "Flat",
#           "18000~30999", "31000~51999", ">52000", "Retired","Unemployed","edu_Intermediate","edu_Low",
#           "breastfed_as_baby", "maternal_smoking", "num_moderate_activity","num_vigorous_activity", "sleep7~8h", "sleep>8h",  "insomnia_sometimes","insomnia_usually","drink_never", "drink_previous", "smoke_never", "smoke_previous",
#           "standing_height", "weight", "waist_circumference", "hip_circumference","bmi25-30" ,"bmi30-40" , "bmi>40", "Cardiovascular", "Hypertension", "Diabetes" ,"Respiratory","family_bc_history",
#           "menarche_age", "menopause_status_Yes", "num_livebirth1~3", "num_livebirth=0",   "stillbirth_status","oc_status", "HRT_status",
#          "bio_triglycerides", "bio_hba1c", "bio_glucose", "bio_HDL" , "bio_SHBG" ,"bio_IGF1",
#          "ap_Nitrogen_dioxide", "ap_pm10", "ap_pm2.5", "np_ave_24.hour_sound_level", 
#         "diet_PC1", "diet_PC2", "diet_PC3")

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


X_pooled = X_pooled[, Groups]
Xgroups = c(4,16, 31, 39, 47, 53)




# normal PLS
#MyPLSDA_pooled <- plsda(X_pooled, Y_pooled, ncomp = 1)
#MyPLSDA_pooled$explained_variance
#MyPLSDA_pooled$loadings$X
#MyPLSDA_pooled$loadings$Y 

# inner PLS
MyPLSDA_pooled <- plsda(X_pooled, Y_pooled_outer, ncomp = 1)
MyPLSDA_pooled$explained_variance
MyPLSDA_pooled$loadings$X
MyPLSDA_pooled$loadings$Y 




### calibration result = 7/15/53 variables

MysPLSDA_pooled <- splsda(X_pooled, Y_pooled_outer, ncomp = 1, keepX = 53)
MysPLSDA_pooled$explained_variance
MysPLSDA_pooled$loadings$X
MysPLSDA_pooled$loadings$X[MysPLSDA_pooled$loadings$X != 0, ]



# loading plot: pls and spls
Loadings = cbind(MyPLSDA_pooled$loadings$X, MysPLSDA_pooled$loadings$X, rep(NA, 28), rep(NA, 28))
Loadings = as.vector(t(Loadings))
Loadings = Loadings[-c(length(Loadings) - 1, length(Loadings))]

#pdf('0411_pls_spls_loadings_plot_7vars.pdf')
#par(mar=c(10,4,4,4))
#plot(Loadings, xaxt = "n", ylab = "Loadings Coefficients", type = "h",lwd = 3, xlab = "", col = c("red", "blue"), cex.sub=0.2,
#     main = "PLS and sPLS loadings for overall Breast Cancer")
#axis(1, at = seq(1.5, 57 * 4, by = 4), labels = colnames(MyPLSDA_pooled$X), las = 2, cex.lab = 0.1)
#axis(1, at = c(0, Xgroups, 54) * 4, line = 6, labels = NA) 
#axis(1, at = c(4,16, 31, 39, 47, 53) * 4, labels = c("Demographic","Socioeconomical", "Lifestyle", "Medical-Risk", "Female-specific", "Biomarkers" ,"Environmental"), line = 7, tick = FALSE, cex.lab = 0.2) 
#
#abline(v = c(0, Xgroups, 54) * 4, lty = 3, col = "black") 
#abline(h = 0, lty = 2)
#legend("bottomright", legend = c("PLS-DA", "sPLS-DA"), col = c("red", "blue"),lty = 1, lwd = 2)

#dev.off()



MygPLSDA_pooled <- gPLSda(X_pooled, Y_pooled_outer, ncomp = 1, ind.block.x = Xgroups, keepX = 2)
MygPLSDA_pooled$loadings$X

MysgPLSDA_pooled <- sgPLSda(X_pooled, Y_pooled_outer, ncomp = 1, ind.block.x = Xgroups, keepX = 2, alpha.x = 0.7)
MysgPLSDA_pooled$loadings$X

