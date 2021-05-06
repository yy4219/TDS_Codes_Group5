

#univariate logistic regression

logitUniVar <- function(dat, group, var, digit = 3){
  formu <- as.formula(paste0(group, " ~ ", var))
  dat[[group]] <- as.factor(dat[[group]])
  subgroup <- levels(as.factor(dat[[group]]))
  subgroup1 <- paste0(subgroup[2], " vs ", subgroup[1])
  fit <- glm(formu, data = dat, family = binomial())
  unisum <- summary(fit)
  OR <- exp(coef(fit))[2]
  OR <- round(OR, digit)
  ci <- exp(confint(fit))[2,]
  ci <- round(ci, digit)
  cito <- paste0(ci[1], " - ", ci[2])
  p <- unisum$coefficients[2, "Pr(>|z|)"]
  p <- ifelse(p < 0.001, "< 0.001", round(p, 3))
  var1 <- names(exp(coef(fit)))[2]
  result <- c(var1, group,subgroup1, OR, cito, p)
  names(result) <- c("var", "group","subgroup", "OR", "95%CI", "p.val")
  return(result)
}


# fixed effects
logitMultiVar <- function(dat, group, var, adjvar,digit = 3){
  if(length(adjvar) == 1){
    formu <- as.formula(paste0(group, " ~ ", var, "+", adjvar))
  }else{
    formu <- as.formula(paste0(group, " ~ ", var, "+", paste(adjvar, collapse = "+")))
  }
  dat[[group]] <- as.factor(dat[[group]])
  subgroup <- levels(as.factor(dat[[group]]))
  subgroup1 <- paste0(subgroup[2], " vs ", subgroup[1])
  fit <- glm(formu, data = dat, family = binomial())
  unisum <- summary(fit)
  OR <- exp(coef(fit))[2]
  OR <- round(OR, digit)
  ci <- exp(confint(fit))[2,]
  ci <- round(ci, digit)
  cito <- paste0(ci[1], " - ", ci[2])
  p <- unisum$coefficients[2, "Pr(>|z|)"]
  p <- ifelse(p < 0.001, "< 0.001", round(p, 3))
  var1 <- names(exp(coef(fit)))[2]
  result <- c(var1, group,subgroup1, OR, cito, p)
  names(result) <- c("var", "group", "subgroup","OR", "95%CI", "p.val")
  return(result)
}

logitUniVar(pbc, group = "trt", var = "sex")

logitMultiVar(dat = pbc, group = "trt", var = "sex",
              adjvar = c("age", "bili"))


multivar <- c("bili", "chol", "albumin", "copper")
logitRes <- lapply(multivar, function(x) logitUniVar(pbc, group = "trt", var = x))





# univariate cox

library(survival)
CoxUniVar <- function(dat, status, times, var, digit = 3){
  dat[[status]] <- as.factor(dat[[status]])
  subgroup <- levels(as.factor(dat[[status]]))
  subgroup1 <- paste0(subgroup[2], " vs ", subgroup[1])
  dat[[status]] <- as.numeric(dat[[status]])
  formu <- as.formula(paste0("Surv(",times,",",status,") ~", var))
  fit <- coxph(formu,data= dat)
  unisum <- summary(fit)
  HR <- exp(coef(fit))[1]
  HR <- round(HR, digit)
  ci <- exp(confint(fit))[1,]
  ci <- round(ci, digit)
  cito <- paste0(ci[1], " - ", ci[2])
  p <- unisum$coefficients[1, "Pr(>|z|)"]
  p <- ifelse(p < 0.001, "< 0.001", round(p, 3))
  var1 <- names(exp(coef(fit)))[1]
  result <- c(var1, status,subgroup1, HR, cito, p)
  names(result) <- c("var", "group", "subgroup","HR", "95%CI", "p.val")
  return(result)
}

# fixed effects

CoxMultiVar <- function(dat, status, times,var, adjvar,digit = 3){
  if(length(adjvar) == 1){
    formu <- as.formula(paste0("Surv(",times,",",status,") ~", var, "+", adjvar))
  }else{
    formu <- as.formula(paste0("Surv(",times,",",status,") ~", var, "+", paste(adjvar, collapse = "+")))
  }
  dat[[status]] <- as.factor(dat[[status]])
  subgroup <- levels(as.factor(dat[[status]]))
  subgroup1 <- paste0(subgroup[2], " vs ", subgroup[1])
  dat[[status]] <- as.numeric(dat[[status]])
  fit <- coxph(formu,data= dat)
  unisum <- summary(fit)
  HR <- exp(coef(fit))[1]
  HR <- round(HR, digit)
  ci <- exp(confint(fit))[1,]
  ci <- round(ci, digit)
  cito <- paste0(ci[1], " - ", ci[2])
  p <- unisum$coefficients[1, "Pr(>|z|)"]
  p <- ifelse(p < 0.001, "< 0.001", round(p, 3))
  var1 <- names(exp(coef(fit)))[1]
  result <- c(var1, status,subgroup1, HR, cito, p)
  names(result) <- c("var", "group", "subgroup","HR", "95%CI", "p.val")
  return(result)
}


CoxUniVar(pbc, status = "trt", times = "time", var = "age")

CoxMultiVar(pbc, status = "trt", times = "time", var = "sex", adjvar = c("age", "status"))

multivar <- c("bili", "chol", "albumin", "copper")
CoxRes <- lapply(multivar, function(x) CoxUniVar(pbc, status  = "trt", times = "time",var = x))
CoxResDat <- do.call(rbind, CoxRes)
CoxResDat
