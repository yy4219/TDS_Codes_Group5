library('ROSE')
library(stringr)
library("dplyr")
library("tidyr")
library("ggplot2")
library(Hmisc)
library('openxlsx')
library('bit64')
library('data.table')
library(corrplot)
library("gplots")
library("pheatmap")
library(tidyverse)
library(caret)
library(glmnet)

library(gtable)

library(grid)

library(ggplot2)
library(gridExtra)
library(cowplot)



setwd('~/tds/Translational Data Science/TDS_Project/05042021/')

data.path="~/tds/Translational Data Science/TDS_Project/05042021/datasets/"

covariate<- readRDS(paste0(data.path,"04162021_TDS_mydata_yang.rds"))



str(covariate)

Var_Cat=data.frame(fread(paste0(data.path,"Var_Cat_04192021.csv")))

#Group variables
  Demographic<- Var_Cat %>% filter(Var_Cat=="Demographic")
  
  Demographic<- Demographic$Var_Name
  
  Social_variable<- Var_Cat %>% filter(Var_Cat=="Social_variable")
  
 Social_variable<- Social_variable$Var_Name
  
  Health_risk<- Var_Cat %>% filter(Var_Cat=="Health_risk")
  
  Health_risk<- Health_risk$Var_Name
  
Environment_exposure<- Var_Cat %>% filter(Var_Cat=="Environment_exposure")
  
Environment_exposure<- Environment_exposure$Var_Name

Social_variable

demographics (which don’t change over the lifecourse): Demographic, 
social variables: Diet_composite_score, Lifestyle, Socio_economical
health risk: Biomarker, Breast_cancer, Health_risk, Reproductive
environmental exposures: Air_pollution, Noise_pollution

data <-  covariate %>% select(Demographic,Social_variable)
model <- glm(covariate$outcome_status ~., family = "binomial", data =data)
get_multi_results <- function(model){
  cbind(
    coef_cat="m1:Demographic,Social_variable",
    coef_name=names(coef(model))[-1],
    OR=exp(coef(model)[-1]),
    ci_low=exp(summary(model)$coefficients[,1][-1]+qnorm(0.025)*summary(model)$coefficients[,2][-1]),
    ci_high=exp(summary(model)$coefficients[,1][-1]+qnorm(0.97)*summary(model)$coefficients[,2][-1]),
    p_val=summary(model)$coefficients[,4][-1])
}

m1_df<- as.data.frame(get_multi_results(model))



m1_df$OR<- as.numeric(as.character(m1_df$OR))
m1_df$ci_low<- as.numeric(as.character(m1_df$ci_low))
m1_df$ci_high<- as.numeric(as.character(m1_df$ci_high))
m1_df$p_val<- as.numeric(as.character(m1_df$p_val))



write.csv(m1_df,file=paste0(data.path,"m1_df_05042021.csv"),row.names = FALSE)

m1_df_2<- data.frame(fread((paste0(data.path,"m1_df_05042021.csv"))))

m1_df_2$Sec_Cat<- factor(m1_df_2$Sec_Cat,levels=c('Demographic','Social_variable','Health_risk','Environment_exposure'))

m1_df_2$coef_cat<- "Model 2"



M2<- ggplot(m1_df_2, aes(y = OR, x = coef_name))+     
  geom_point(aes(color=Sec_Cat)) + theme(text = element_text(size=8),
                                           axis.text.x = element_blank())+
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high,col=Sec_Cat), width=.05)+
  labs(x="",y="Odds ratio",color = "Legend")+
  theme(legend.position="none")+ facet_grid(coef_cat~Sec_Cat, scales="free_x")+
  geom_hline(aes(fill=variables),yintercept =1, linetype=2)

ggsave(paste0(data.path,"m1_05042021.png"))





data <-  covariate %>% select(Demographic,Social_variable,Health_risk)
model <- glm(covariate$outcome_status ~., family = "binomial", data =data)
get_multi_results <- function(model){
  cbind(
    coef_cat="m2:Demographic,Social_variable,,Health_risk",
    coef_name=names(coef(model))[-1],
    OR=exp(coef(model)[-1]),
    ci_low=exp(summary(model)$coefficients[,1][-1]+qnorm(0.025)*summary(model)$coefficients[,2][-1]),
    ci_high=exp(summary(model)$coefficients[,1][-1]+qnorm(0.97)*summary(model)$coefficients[,2][-1]),
    p_val=summary(model)$coefficients[,4][-1])
}

m2_df<- as.data.frame(get_multi_results(model))



m2_df$OR<- as.numeric(as.character(m2_df$OR))
m2_df$ci_low<- as.numeric(as.character(m2_df$ci_low))
m2_df$ci_high<- as.numeric(as.character(m2_df$ci_high))
m2_df$p_val<- as.numeric(as.character(m2_df$p_val))


write.csv(m2_df,file=paste0(data.path,"m2_df_05042021.csv"),row.names = FALSE)

m2_df_2<- data.frame(fread((paste0(data.path,"m2_df_05042021.csv"))))

m2_df_2$Sec_Cat<- factor(m2_df_2$Sec_Cat,levels=c('Demographic','Social_variable','Health_risk','Environment_exposure'))

m2_df_2$coef_cat<- "Model 3"

M3<- ggplot(m2_df_2, aes(y = OR, x = coef_name))+     
  geom_point(aes(color=Sec_Cat)) + theme(text = element_text(size=8),
                                           axis.text.x = element_blank())+
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high,col=Sec_Cat), width=.05)+
  labs(x="",y="Odds ratio",color = "Legend")+
  theme(legend.position="none")+ facet_grid(coef_cat~Sec_Cat, scales="free_x")+
  geom_hline(aes(fill=variables),yintercept =1, linetype=2)

ggsave(paste0(data.path,"m2_05042021.png"))

Environment_exposure

data <-  covariate %>% select(Demographic,Social_variable,Health_risk,Environment_exposure)
model <- glm(covariate$outcome_status ~., family = "binomial", data =data)
get_multi_results <- function(model){
  cbind(
    coef_cat="m3:Demographic,Social_variable,Health_risk,Environment_exposure",
    coef_name=names(coef(model))[-1],
    OR=exp(coef(model)[-1]),
    ci_low=exp(summary(model)$coefficients[,1][-1]+qnorm(0.025)*summary(model)$coefficients[,2][-1]),
    ci_high=exp(summary(model)$coefficients[,1][-1]+qnorm(0.97)*summary(model)$coefficients[,2][-1]),
    p_val=summary(model)$coefficients[,4][-1])
}

m3_df<- as.data.frame(get_multi_results(model))



m3_df$OR<- as.numeric(as.character(m3_df$OR))
m3_df$ci_low<- as.numeric(as.character(m3_df$ci_low))
m3_df$ci_high<- as.numeric(as.character(m3_df$ci_high))
m3_df$p_val<- as.numeric(as.character(m3_df$p_val))

write.csv(m3_df,file=paste0(data.path,"m3_df_05042021.csv"),row.names = FALSE)

m3_df_2<- data.frame(fread((paste0(data.path,"m3_df_05042021.csv"))))

m3_df_2$Sec_Cat<- factor(m3_df_2$Sec_Cat,levels=c('Demographic','Social_variable','Health_risk','Environment_exposure'))

m3_df_2$coef_cat<- "Model 4"

M4<- ggplot(m3_df_2, aes(y = OR, x = coef_name))+     
  geom_point(aes(color=Sec_Cat)) + theme(text = element_text(size=5.5),
                                           axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high,col=Sec_Cat), width=.05)+
  labs(x="Predictors",y="Odds ratio",color = "Legend")+
  theme(legend.position="none")+
 
  geom_hline(aes(fill=variables),yintercept =1, linetype=2)+ facet_grid(coef_cat~Sec_Cat, scales="free_x")

ggsave(paste0(data.path,"m3_05042021.png"))



data <-  covariate %>% select(Demographic)
model <- glm(covariate$outcome_status ~., family = "binomial", data =data)
get_multi_results <- function(model){
  cbind(
    coef_cat="m0:Demographic",
      Sec_Cat="Demographic",
    coef_name=names(coef(model))[-1],
    OR=exp(coef(model)[-1]),
    ci_low=exp(summary(model)$coefficients[,1][-1]+qnorm(0.025)*summary(model)$coefficients[,2][-1]),
    ci_high=exp(summary(model)$coefficients[,1][-1]+qnorm(0.97)*summary(model)$coefficients[,2][-1]),
    p_val=summary(model)$coefficients[,4][-1])
}

m0_df<- as.data.frame(get_multi_results(model))



m0_df$OR<- as.numeric(as.character(m0_df$OR))
m0_df$ci_low<- as.numeric(as.character(m0_df$ci_low))
m0_df$ci_high<- as.numeric(as.character(m0_df$ci_high))
m0_df$p_val<- as.numeric(as.character(m0_df$p_val))

write.csv(m0_df,file=paste0(data.path,"m0_df_05042021.csv"),row.names = FALSE)

m0_df_2<- data.frame(fread((paste0(data.path,"m0_df_05042021.csv"))))

m0_df_2$Sec_Cat<- factor(m0_df_2$Sec_Cat,levels=c('Demographic','Social_variable','Health_risk','Environment_exposure'))

m0_df_2$coef_cat<- "Model 1"


M1<- ggplot(m0_df_2, aes(y = OR, x = coef_name))+     
  geom_point(aes(color=Sec_Cat)) + theme(text = element_text(size=8),
                                           axis.text.x = element_blank())+
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high,col=Sec_Cat), width=.05)+
  labs(x="",y="Odds ratio",color = "Legend")+
  theme(legend.position="none")+ facet_grid(coef_cat~Sec_Cat, scales="free_x")+
  geom_hline(aes(fill=variables),yintercept =1, linetype=2)

ggsave(paste0(data.path,"m0_05042021.png"))

plot_grid(M1, M2,M3,M4, align = "v", nrow = 4, rel_heights = c(2, 2, 2,6))
ggsave(paste0(data.path,"m_all_05042021.png"))

#### Forest plot 
```{r forest, echo = FALSE,out.width="1000%"}
knitr::include_graphics(paste0(data.path,"m_all.png"))
```
