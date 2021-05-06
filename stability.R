
# set path to include my libraries
.libPaths(c("/rds/general/user/ac1220/home/R/x86_64-redhat-linux-gnu-library/3.6",
            "/usr/lib64/R/library",
            "/usr/share/R/library" ))


project_path="/rds/general/project/hda_students_data/live/Group5/General/AC"
setwd(project_path)


library(mixOmics)
library(pheatmap)
library(tidyr)
library(sgPLS)
source("Scripts/pls_functions.R")

# Load  dataset
dummy <- readRDS("0411_dummy.rds")

colnames(dummy)

# Create X_pooled and Y_pooled
X_pooled <- dummy
X_pooled$outcome_status <- NULL
X_pooled$bc_inner <- NULL
X_pooled$bc_outer <- NULL
dim(X_pooled) #57
colnames(X_pooled)


Y_pooled <- dummy$outcome_status
Y_pooled_inner <- dummy$bc_inner
Y_pooled_outer <- dummy$bc_outer

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
dim(X_pooled) #156456     57
colnames(X_pooled)

Xgroups = c(4,13, 26, 37, 44, 50)



### Stability analysis, 100 iterations

set.seed(1)
Stab_bc = StabilityPlot(X = X_pooled, Y = Y_pooled, NIter = 100)
pheatmap(Stab_bc, cluster_rows = FALSE, cluster_cols = FALSE, display_numbers = TRUE,
         height = 30, width = 60, fontsize = 20, filename = "0411_stab_bc.pdf")


set.seed(1)
Stab_bc_inner = StabilityPlot(X = X_pooled, Y = Y_pooled_inner, NIter = 100)
pheatmap(Stab_bc_inner , cluster_rows = FALSE, cluster_cols = FALSE, display_numbers = TRUE,
         height = 30, width = 60, fontsize = 20, filename = "0411_stab_bc_inner.pdf")


set.seed(1)
Stab_bc_outer = StabilityPlot(X = X_pooled, Y = Y_pooled_outer, NIter = 100)
pheatmap(Stab_bc_outer , cluster_rows = FALSE, cluster_cols = FALSE, display_numbers = TRUE,
         height = 30, width = 60, fontsize = 20, filename = "0411_stab_bc_outer.pdf")

