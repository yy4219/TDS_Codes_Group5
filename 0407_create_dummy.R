project_path="/rds/general/project/hda_students_data/live/Group5/General/AC"
setwd(project_path)


library(mixOmics)
library(pheatmap)
library(tidyr)
library(sgPLS)
library(fastDummies)
source("Scripts/pls_functions.R")



# Load  dataset
df <- readRDS("mydata.rds")
head(df)
dim(df)

# Conduct complete case 156456 -> 156456
df_clean <- df[complete.cases(df), ]

dim(df_clean)
table(df_clean$outcome_status) #83620  2283 ->>>> 152228   4228 
table(df_clean$bc_inner) #85548   355  ->>>>> 155832    624 
table(df_clean$bc_outer) #85189   714  ->>>>> 155202   1254



# Dummy variables
dummy <- dummy_cols(df_clean, select_columns = c("ave_total_household_income", "breastfed_as_baby", "maternal_smoking", "family_history",
                                                 'own_or_rent', 'accommodation_type',
                                                 'number_in_household', 'employment', 'education', "Cardiovascular", "Hypertension", "Diabetes",
                                                 "Respiratory",
                                                 'ethnic', 'num_moderate_activity', 'num_vigorous_activity', 'sleep_duration',
                                                 'insomnia', 'alcohol_drinker', 'smoking_status', 'BMI', 'menopause_status',
                                                 'live_births_num', 'stillbirth_status', 'oc_status', 'HRT_status'), 
                    remove_selected_columns = TRUE)

head(dummy)






# Remove reference group for each categorical variable
dummy[,"Cardiovascular_No"] <- NULL
dummy[,"Hypertension_No"] <- NULL
dummy[,"Diabetes_No"] <- NULL
dummy[,"Respiratory_No"] <- NULL
dummy[,"ave_total_household_income_<18000"] <- NULL
dummy[,"breastfed_as_baby_No"] <- NULL
dummy[,"maternal_smoking_No"] <- NULL
dummy[,"family_history_No"] <- NULL
dummy$own_or_rent_own <- NULL
dummy$accommodation_type_House <- NULL
dummy[,"number_in_household_1~2"] <- NULL
dummy$employment_Unemployed <- NULL
dummy$ethnic_White <- NULL
dummy$education_Low <- NULL
dummy[,"num_moderate_activity_<3"] <- NULL
dummy[,"num_vigorous_activity_<3"] <- NULL
dummy[,"sleep_duration_<7"] <- NULL
dummy[,"insomnia_Rarely"] <- NULL
dummy[,"alcohol_drinker_Never"] <- NULL
dummy[,"smoking_status_Never"] <- NULL
dummy[,"BMI_<25"] <- NULL
dummy[,"menopause_status_No"] <- NULL
dummy[,"live_births_num_0"] <- NULL
dummy[,"live_births_num_0"] <- NULL
dummy[,"stillbirth_status_No"] <- NULL
dummy[,"oc_status_No"] <- NULL
dummy[,"HRT_status_No"] <- NULL

# Change names to avoid errors
colnames(dummy)[which(names(dummy) == "ave_total_household_income_18000~30999")] <- "ave_total_household_income_18000_30999"
colnames(dummy)[which(names(dummy) == "ave_total_household_income_31000~51999")] <- "ave_total_household_income_31000_51999"
colnames(dummy)[which(names(dummy) == "ave_total_household_income_>52000")] <- "ave_total_household_income_over_52000"
colnames(dummy)[which(names(dummy) == "number_in_household_>=3")] <- "number_in_household_over_3"
colnames(dummy)[which(names(dummy) == "num_moderate_activity_>=3")] <- "num_moderate_activity_over_3"
colnames(dummy)[which(names(dummy) == "num_vigorous_activity_>=3")] <- "num_vigorous_activity_over_3"
colnames(dummy)[which(names(dummy) == "sleep_duration_7~8")] <- "sleep_duration_7_8h"
colnames(dummy)[which(names(dummy) == "sleep_duration_>8")] <- "sleep_duration_over_8h"
colnames(dummy)[which(names(dummy) == "BMI_25-30")] <- "BMI_25_30"
colnames(dummy)[which(names(dummy) == "BMI_30-40")] <- "BMI_30_40"
colnames(dummy)[which(names(dummy) == "BMI_>40")] <- "BMI_over_40"
colnames(dummy)[which(names(dummy) == "live_births_num_1~3")] <- "live_births_num_1_3"
colnames(dummy)[which(names(dummy) == "live_births_num_>3")] <- "live_births_num_over_3"
colnames(dummy)[which(names(dummy) == "PC1")] <- "diet_PC1"
colnames(dummy)[which(names(dummy) == "PC2")] <- "diet_PC2"
colnames(dummy)[which(names(dummy) == "PC3")] <- "diet_PC3"
colnames(dummy)[which(names(dummy) == "accommodation_type_Care home/temporary or sheltered")] <- "accommodation_type_Care_home_others"

dim(dummy) #85903    57 ----> 156456     61
colnames(dummy)

# Remove eid
dummy$eid <- NULL

dummy <- mutate_all(dummy, function(x) as.numeric(as.character(x)))

saveRDS(dummy, "0411_dummy.rds")
