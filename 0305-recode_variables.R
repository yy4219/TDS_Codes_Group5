library(caret)
library("dplyr")
library("tidyr")
library("ggplot2")


m =readRDS("0305_final_32cols_raw.rds")

colSums(is.na(m)) # Zero NA in m


###### 1. Qualification (X6138.0.0) -> Low, Intermediate, High, NA
coding_id="100305"
mycoding=read.csv(file = "/rds/general/project/hda_students_data/live/Group5/General/Codings_Showcase.csv")
print(head(mycoding))
mycoding_field=mycoding[which(mycoding[,1]==coding_id),]
mycoding_field=mycoding_field[,-1]
rownames(mycoding_field)=mycoding_field[,1]
print(mycoding_field)

# As it is in raw data:
print(m$X6138.0.0)
table(m$X6138.0.0)

# Recoded categories:
m$X6138.recode = as.character(mycoding_field[as.character(m$X6138.0.0),"Meaning"])



table(m$X6138.recode)

m$education = ifelse(m$X6138.recode == "College or University degree", "High",
                          ifelse(m$X6138.recode == "None of the above", "Low",
                                 ifelse(m$X6138.recode == "Prefer not to answer" , NA, "Intermediate")))


table(m$X6138.recode)

#####2. Smoking (200116) -> Current, Previous, Never, NA
coding_id="90"
print(head(mycoding))
mycoding_field=mycoding[which(mycoding[,1]==coding_id),]
mycoding_field=mycoding_field[,-1]
rownames(mycoding_field)=mycoding_field[,1]
print(mycoding_field)

# As it is in raw data:
print(m$X20116.0.0)
table(m$X20116.0.0)

# Recoded categories:
m$X20116.recode = as.character(mycoding_field[as.character(m$X20116.0.0),"Meaning"])
m$smoking_status <- gsub("Prefer not to answer", NA, m$X20116.recode)


######3. Alcohol intake (X20117.0.0) -> Current, Previous, Never, NA
coding_id="90"
print(head(mycoding))
mycoding_field=mycoding[which(mycoding[,1]==coding_id),]
mycoding_field=mycoding_field[,-1]
rownames(mycoding_field)=mycoding_field[,1]
print(mycoding_field)

# As it is in raw data:
print(m$X20117.0.0)
table(m$X20117.0.0)

# Recoded categories:
m$X20117.recode = as.character(mycoding_field[as.character(m$X20117.0.0),"Meaning"])

m$alcohol_status <- gsub("Prefer not to answer", NA, m$X20117.recode)
table(m$X20117.recode)



###### 4. Family history of BC: Illnesses of mother (20110) -> Yes, No
coding_id="1010"
print(head(mycoding))
mycoding_field=mycoding[which(mycoding[,1]==coding_id),]
mycoding_field=mycoding_field[,-1]
rownames(mycoding_field)=mycoding_field[,1]
print(mycoding_field)

# As it is in raw data:
print(m$X20110.0.0)
table(m$X20110.0.1)
table(m$X20110.0.0)

# Recoded categories:
m$X20110.recode = as.character(mycoding_field[as.character(m$X20110.0.0),"Meaning"])
m$X20110.1.recode = as.character(mycoding_field[as.character(m$X20110.0.1),"Meaning"])

table(m$X20110.1.recode)

m$family_history <- ifelse(m$X20110.recode == "Breast cancer"|m$X20110.1.recode == "Breast cancer", "Yes", "No")
table(m$X20110.recode)




#####5. Ethnicity (21000) 
coding_id="1001"
print(head(mycoding))
mycoding_field=mycoding[which(mycoding[,1]==coding_id),]
mycoding_field=mycoding_field[,-1]
rownames(mycoding_field)=mycoding_field[,1]
print(mycoding_field)

# As it is in raw data:
print(m$X21000.0.0)
table(m$X21000.0.0)

# Recoded categories:
m$ethnicity = as.character(mycoding_field[as.character(m$X21000.0.0),"Meaning"])





######## recategorise variables
###### 1. BMI (21001) -> <25, 20-39
m$bmi_cat <- ifelse(m$X21001.0.0 < 25,"<25",
                          ifelse(m$X21001.0.0 >= 25 & m$X21001.0.0 < 30, "25-30",
                                 ifelse(m$X21001.0.0 >= 30 & m$X21001.0.0 < 40, "30-40", ">40")))

table(m$X21001.recode)


###### 2. Days in a week of moderate exercise (884) -> 0, 1-3, 4-7, NA
m$active_days <- ifelse(m$X884.0.0 == 0,"0",
                        ifelse(m$X884.0.0 > 0 & m$X884.0.0 <= 3, "1-3",
                               ifelse(m$X884.0.0 > 3 & m$X884.0.0 <= 7, "4-7", NA)))

table(m$X884.0.0)




#--------------Creating new variables (Waist-to-hip ratio, Ever had children)

###### 1. Waist to hip ratio (48/49)

m$waist_hip_ratio = m$X48.0.0 / m$X49.0.0


###### 2. Ever had children (no. of live births: 2734) -> Yes, No, NA

m$ever_had_birth = ifelse(m$X2734.0.0 == 0, "No",
                        ifelse(m$X2734.0.0 > 0, "Yes", NA))

table(m$X2734.recode)                        


###### menarche age: change -1 and -3 into NA
m$menarche_age <- gsub("-1", NA, m$X2714.0.0)
m$menarche_age <- gsub("-3", NA, m$menarche_age)


head(m)

#removing temporary recoding variables that are no longer useful
m$X6138.recode = NULL
m$X20116.recode = NULL
m$X20117.recode = NULL
m$X20110.recode = NULL
m$X20110.1.recode = NULL
m$Xbmi_cat = NULL
m$Xactive_days = NULL


#removing original variables that have been recoded
clean_m <- m
clean_m$X6138.0.0 = NULL
clean_m$X20116.0.0 = NULL
clean_m$X20117.0.0 = NULL
clean_m$X884.0.0 = NULL
clean_m$X20110.0.0 = NULL
clean_m$X20110.0.1 = NULL
clean_m$X21000.0.0 = NULL
clean_m$X2714.0.0 = NULL

# remove waist and hip variables
clean_m$X48.0.0 = NULL
clean_m$X49.0.0 = NULL

dim(clean_m)
head(clean_m)

colSums(is.na(clean_m)) # some NA introduced due to "-1", "-3" placements


# remove rows with NA
clean_m <- drop_na(clean_m)

# 136551 samples
dim(clean_m) # 136551     34

# controls: 131858   cases: 4693 
table(clean_m$outcome_status)
table(clean_m$alcohol_status)

saveRDS(clean_m, "test.rds")

head(clean_m)
dim(clean_m)











# open the new dataset
new <- readRDS("test.rds")
dim(new)
head(new)
colSums(is.na(new))

clean_m$menarche_age <- as.numeric(clean_m$menarche_age)
hist(new$menarche_age)

clean_msummary(new)
hist(new$X189.0.0)

# 2674: Breast cancer screening
table(new$X2674.0.0)
new$X2674.0.0 <- gsub("-1", NA, new$X2674.0.0)
new$X2674.0.0 <- gsub("-3", NA, new$X2674.0.0)
table(new$X2674.0.0)

# 2724: Menopause status
table(new$X2724.0.0)
#--- "-3":Prefer not to say  , "3": No sure
new$X2724.0.0 <- gsub("-3", NA, new$X2724.0.0)
new$X2724.0.0 <- gsub("3", NA, new$X2724.0.0)
table(new$X2724.0.0)

# 2774: Ever has miscarriages
table(new$X2774.0.0)
#--- "-3":Prefer not to say  , "3": No sure
new$X2774.0.0 <- gsub("-1", NA, new$X2774.0.0)
new$X2774.0.0 <- gsub("-3", NA, new$X2774.0.0)
table(new$X2774.0.0)


# 2784: History of OC
table(new$X2784.0.0)
new$X2784.0.0<- gsub("-1", NA, new$X2784.0.0)
new$X2784.0.0<- gsub("-3", NA, new$X2784.0.0)
table(new$X2784.0.0)

# 2814: HISTORY HRT
table(new$X2814.0.0)
new$X2814.0.0<- gsub("-1", NA, new$X2814.0.0)
new$X2814.0.0<- gsub("-3", NA, new$X2814.0.0)
table(new$X2814.0.0)

#==================RECODE ETHNICITY

table(new$ethnicity)
new$ethnicity<- gsub("Prefer not to answer", NA, new$ethnicity)
new$ethnicity<- gsub("Do not know", NA, new$ethnicity)


new$ethnicity_cat <- ifelse(new$ethnicity == "British" | new$ethnicity == "White" | new$ethnicity == "Any other white background" |new$ethnicity == "Irish", "White",
             ifelse(new$ethnicity == "African"|new$ethnicity == "Caribbean" | new$ethnicity == "Any other Black background"| new$ethnicity == "Black or Black British", "Black",
                    ifelse(new$ethnicity == "Chinese" | new$ethnicity == "Any other Asian background" | new$ethnicity == "Asian or Asian British"| new$ethnicity == "Bangladeshi" | new$ethnicity == "Indian", "Asian", "Others")))

cancer <- subset(new, outcome_status == 1)
table(cancer$ethnicity_cat)
#Asian  Black Others  White 
#2172   1897   2294 129878 

#===============Remove 48 waist and 49 waist
new$X49.0.0 <- NULL
new$X48.0.0 <- NULL
new$ethnicity <- NULL
new$X21001.0.0  <- NULL


#================Change categorical to factor
str(new)
head(new)
dim(new)

new$X31.0.0 <- as.factor(new$X31.0.0)
new$X2674.0.0 <- as.factor(new$X2674.0.0)
new$X2724.0.0 <- as.factor(new$X2724.0.0)
new$X2734.0.0 <- as.factor(new$X2734.0.0)
new$X2774.0.0 <- as.factor(new$X2774.0.0)
new$X2784.0.0 <- as.factor(new$X2784.0.0)
new$X2814.0.0 <- as.factor(new$X2814.0.0)
new$outcome_status <- as.factor(new$outcome_status)
new$Cardiovascular <- as.factor(new$Cardiovascular)
new$Hypertension <- as.factor(new$Hypertension)
new$Diabetes <- as.factor(new$Diabetes)
new$Respiratory <- as.factor(new$Respiratory)
new$education <- as.factor(new$education)
new$smoking_status <- as.factor(new$smoking_status)
new$alcohol_status <- as.factor(new$alcohol_status)
new$family_history <- as.factor(new$family_history)
new$ever_had_birth <- as.factor(new$ever_had_birth)
new$menarche_age <- as.integer(new$menarche_age)
new$bmi_cat <- as.factor(new$bmi_cat)
new$active_days <- as.factor(new$active_days)
new$ethnicity_cat <- as.factor(new$ethnicity_cat)
head(new)
dim(new)

saveRDS(new, "0309_31cols_recoded_raw.rds")


dim(new)
dim(label_df)

#--------------Create a dataset with real labels

label_df <- new %>% 
  rename(
    sex = X31.0.0,
    recruitment_age = X21022.0.0,
    tdi = X189.0.0,
    screening_status = X2674.0.0,
    menopause_status = X2724.0.0, 
    num_live_births = X2734.0.0,
    ever_had_stillbirth = X2774.0.0,
    oc_status = X2784.0.0,
    hrt_status = X2814.0.0
    
  )



label_df$ever_had_stillbirth = ifelse(label_df$ever_had_stillbirth == 0, "No", "Yes")
table(label_df$ever_had_stillbirth)

label_df$menopause_status = ifelse(label_df$menopause_status == 0, "No",
                                   ifelse(label_df$menopause_status == 1, "Yes", "Had hysterectomy"))
table(label_df$menopause_status)


label_df$screening_status = ifelse(label_df$screening_status == 0, "No", "Yes")
table(label_df$screening_status )


summary(label_df)

label_df$ever_had_stillbirth <- as.factor(label_df$ever_had_stillbirth)
label_df$menopause_status <- as.factor(label_df$menopause_status)
label_df$screening_status <- as.factor(label_df$screening_status)


label_df$bio_triglycerides = label_df[, c("30870-0.0")]

label_df$bio_hba1c = label_df[, c("30750-0.0")]

label_df$bio_glucose = label_df[, c("30740-0.0")]

label_df$bio_HDL = label_df[, c("30760-0.0")]

label_df$bio_SHBG = label_df[, c("30830-0.0")]

label_df$bio_IGF1 = label_df[, c("30770-0.0")]


# Remove original biomarker data
label_df[, c("30870-0.0")] <- NULL
label_df[, c("30750-0.0")] <- NULL
label_df[, c("30740-0.0")] <- NULL
label_df[, c("30760-0.0")] <- NULL
label_df[, c("30830-0.0")] <- NULL
label_df[, c("30770-0.0")] <- NULL

hist(m$menarche_age)
m$menarche_age <- as.numeric(m$menarche_age)
hist(m_bio_hes$X2714.0.0)


head(label_df)
summary(label_df)
table(new$outcome_status)


saveRDS(label_df, "0309_31cols_recoded_label.rds")




