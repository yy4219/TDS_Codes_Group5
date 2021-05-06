
rm(list=ls())
mydata=readRDS("/tds/female_dataset.rds")


#recode own_or_rent--- own v.s. rent
# own:"Own outright (by you or someone in your household)","Own with a mortgage"
# rent:"Live in accommodation rent free","Pay part rent and part mortgage (shared ownership)",
#      "Rent - from local authority, local council, housing association",
#      "Rent - from private landlord or letting agency"

mydata$own_or_rent<-ifelse((mydata$own_or_rent=="Live in accommodation rent free"|mydata$own_or_rent=="Pay part rent and part mortgage (shared ownership)"|mydata$own_or_rent=="Rent - from local authority, local council, housing association"|mydata$own_or_rent=="Rent - from private landlord or letting agency"),
                            "rent", ifelse((mydata$own_or_rent=="Own outright (by you or someone in your household)"|mydata$own_or_rent=="Own with a mortgage"),"own",NA))

table(mydata$own_or_rent,exclude=F)

#recode accomodation_type --- House v.s. Flat v.s. Care home/temporary or sheltered 
# House: "A house or bungalow"     
# Flat: "A flat, maisonette or apartment"
# Care home/temporary or sheltered: "Care home", "Mobile or temporary structure (i.e. caravan)", "Sheltered accommodation"
mydata$accommodation_type<-ifelse(mydata$accommodation_type=="NA",NA,
                                   ifelse(mydata$accommodation_type=="A house or bungalow","House",
                                          ifelse(mydata$accommodation_type=="A flat, maisonette or apartment","Flat","Care home/temporary or sheltered")))

table(mydata$accommodation_type,exclude=F)


#recode number_in_household ---1-2 v.s.>=3
mydata$number_in_household<-ifelse((mydata$number_in_household=="1"|mydata$number_in_household=="2"),"1~2",
                                    ifelse(mydata$number_in_household=="NA",NA,">=3"))
table(mydata$number_in_household,exclude = F)

# recode ave_total_household_income ---<18000,18000~30999,31000~51999,>52000
mydata$ave_total_household_income<-ifelse(mydata$ave_total_household_income=="Less than 18,000","<18000",
                                           ifelse(mydata$ave_total_household_income=="18,000 to 30,999","18000~30999",
                                                  ifelse(mydata$ave_total_household_income=="31,000 to 51,999","31000~51999",
                                                         ifelse(mydata$ave_total_household_income=="NA",NA,">52000"))))
table(mydata$ave_total_household_income,exclude = F)

# recode employment -- employed v.s. unemployed v.s. retired
# employed: "Doing unpaid or voluntary work" , " In paid employment or self¡ªemployed"
# unemployed: "Full or part-time student", "Looking after home and/or family",
#             "Unemployed", "Unable to work because of sickness or disability"
# retired: "Retired"
# NA : "", "None of the above"
mydata$employment<-ifelse((mydata$employment==""|mydata$employment=="None of the above"),NA,
                       ifelse((mydata$employment=="Full or part-time student"|mydata$employment=="Looking after home and/or family"|mydata$employment=="Unemployed"|mydata$employment=="Unable to work because of sickness or disability"),"Unemployed",
                              ifelse(mydata$employment=="Retired","Retired","Employed")))

table(mydata$employment,exclude=F)

# recode moderate_activity_duration -- <20, >=20
mydata$moderate_activity_duration<-ifelse(mydata$moderate_activity_duration<20,"<20",
                                          ifelse(mydata$moderate_activity_duration>=20,">=20",NA))
table(mydata$moderate_activity_duration,exclude = F)

# recode num_moderate_activity -- <3,>=3
mydata$num_moderate_activity<-ifelse(mydata$num_moderate_activity<3,"<3",
                                           ifelse(mydata$num_moderate_activity>=3,">=3",NA))
table(mydata$num_moderate_activity,exclude = F)

# recode vigorous_activity_duration -- <20, >=20
mydata$vigorous_activity_duration<-ifelse(mydata$vigorous_activity_duration<20,"<20",
                                           ifelse(mydata$vigorous_activity_duration>=20,">=20",NA))
table(mydata$vigorous_activity_duration,exclude = F)

# recode num_vigorous_activity -- <3,>=3
mydata$num_vigorous_activity<-ifelse(mydata$num_vigorous_activity<3,"<3",
                                      ifelse(mydata$num_vigorous_activity>=3,">=3",NA))
table(mydata$num_vigorous_activity,exclude = F)

# recode sleep_duration-- <7,7-8,>8
mydata$sleep_duration<-as.numeric(mydata$sleep_duration)
mydata$sleep_duration<-ifelse(mydata$sleep_duration<7,"<7",
                               ifelse(mydata$sleep_duration==7|mydata$sleep_duration==8,"7~8",
                                      ifelse(mydata$sleep_duration>8,">8",NA)))

table(mydata$sleep_duration,exclude = F)


#transform datatype for cereal_intake,tea_intake,coffee_intake,water_intake
mydata$cereal_intake<-as.numeric(mydata$cereal_intake)
mydata$tea_intake<-as.numeric(mydata$tea_intake)
mydata$coffee_intake<-as.numeric(mydata$coffee_intake)
mydata$water_intake<-as.numeric(mydata$water_intake)
  

#recode live_births_num -- 0, 1~3,>3
mydata$live_births_num<-ifelse(mydata$live_births_num=="no live birth","0",
                                ifelse(mydata$live_births_num=="more than 3 live births",">3",
                                       ifelse(mydata$live_births_num=="NA",NA,"1~3")))

table(mydata$live_births_num,exclude = F)


# deal with bc subtypes -- transform NA to 0 

mydata[which(is.na(mydata["bc_C500"])),"bc_C500"]<-0
mydata[which(is.na(mydata["bc_C501"])),"bc_C501"]<-0
mydata[which(is.na(mydata["bc_C502"])),"bc_C502"]<-0
mydata[which(is.na(mydata["bc_C503"])),"bc_C503"]<-0
mydata[which(is.na(mydata["bc_C504"])),"bc_C504"]<-0
mydata[which(is.na(mydata["bc_C505"])),"bc_C505"]<-0
mydata[which(is.na(mydata["bc_C506"])),"bc_C506"]<-0
mydata[which(is.na(mydata["bc_C508"])),"bc_C508"]<-0
mydata[which(is.na(mydata["bc_C509"])),"bc_C509"]<-0

str(mydata)
saveRDS(mydata,"female_finaldataset_nodummy.rds")