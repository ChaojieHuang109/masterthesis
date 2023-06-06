library(dplyr) 
library(nlme)
library(lme4)
library(JMbayes2)
library(survival)
library(GLMMadaptive)
library(caTools)

# Load data
longformat_patients_train <- read.csv("/ANALYSE_AREA/P_INTG01/Chaojie/code_new/longformat_patients_train.csv",row.names = 1) %>%
  mutate(sex=factor(sex),smoking=factor(smoking),cvd_status=factor(cvd),death_status=factor(death),status=factor(status))

longformat_patients_test <- read.csv("/ANALYSE_AREA/P_INTG01/Chaojie/code_new/longformat_patients_test.csv",row.names = 1) %>%
  mutate(sex=factor(sex),smoking=factor(smoking),cvd_status=factor(cvd),death_status=factor(death),status=factor(status))

survival_df <- longformat_patients_train %>%
  filter(year==0)


#-----------------------------------------------------------------------------------------Slope-----------------------------------------------------------------------------------#

slope_forms <- list(
  "sys" = ~ value(sys) + slope(sys),
  "chlratio" = ~ value(chlratio) + slope(chlratio))
#--------------------------------------------------------------------------------Joint Model1 S1*CR1-------------------------------------------------------------------------#
fit_sys1 <- lme(sys~year+age+sex+year:age+age:sex,data=longformat_patients_train, random=~year|ID)
fit_chlratio1 <- lme(chlratio~year+age+sex+year:age+age:sex,data=longformat_patients_train, random=~year|ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit1 <- jm(fitcox, list(fit_sys1, fit_chlratio1), time_var = "year", functional_forms=slope_forms)
summary(Jointfit1)

#--------------------------------------------------------------------------------Joint Model2 S2*CR2-------------------------------------------------------------------------#
fit_sys2 <- lme(sys~year+age+sex+age:sex,data=longformat_patients_train, random=~year|ID)
fit_chlratio2 <- lme(chlratio~year+age+sex+age:sex,data=longformat_patients_train, random=~year|ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit2 <- jm(fitcox, list(fit_sys2, fit_chlratio2), time_var = "year",functional_forms=slope_forms) 
summary(Jointfit2)

#--------------------------------------------------------------------------------Joint Model3 S3*CR3-------------------------------------------------------------------------#
fit_sys3 <- lme(sys~year+age+sex+year:age+year:sex+age:sex,data=longformat_patients_train, random=~year|ID)
fit_chlratio3 <- lme(chlratio~year+age+sex+year:age+year:sex+age:sex,data=longformat_patients_train, random=~year|ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit3 <- jm(fitcox, list(fit_sys3, fit_chlratio3), time_var = "year",functional_forms=slope_forms) 
summary(Jointfit3)

#--------------------------------------------------------------------------------Joint Model4 S4*CR4-------------------------------------------------------------------------#
fit_sys4 <- lme(sys~year+age+sex+year:age,data=longformat_patients_train, random=~year|ID)
fit_chlratio4 <- lme(chlratio~year+age+sex+year:age,data=longformat_patients_train, random=~year|ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit4 <- jm(fitcox, list(fit_sys4, fit_chlratio4), time_var = "year",functional_forms=slope_forms) 
summary(Jointfit4)

#--------------------------------------------------------------------------------Joint Model5 S1*CR2-------------------------------------------------------------------------#
fit_sys1 <- lme(sys~year+age+sex+year:age+age:sex,data=longformat_patients_train, random=~year|ID)
fit_chlratio2 <- lme(chlratio~year+age+sex+age:sex,data=longformat_patients_train, random=~year|ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit5 <- jm(fitcox, list(fit_sys1, fit_chlratio2), time_var = "year",functional_forms=slope_forms)
summary(Jointfit5)

#--------------------------------------------------------------------------------Joint Model6 S1*CR3-------------------------------------------------------------------------#
fit_sys1 <- lme(sys~year+age+sex+year:age+age:sex,data=longformat_patients_train, random=~year|ID)
fit_chlratio3 <- lme(chlratio~year+age+sex+year:age+year:sex+age:sex,data=longformat_patients_train, random=~year|ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit6 <- jm(fitcox, list(fit_sys1, fit_chlratio3), time_var = "year",functional_forms=slope_forms)
summary(Jointfit6)

#--------------------------------------------------------------------------------Joint Model7 S1*CR4-------------------------------------------------------------------------#
fit_sys1 <- lme(sys~year+age+sex+year:age+age:sex,data=longformat_patients_train, random=~year|ID)
fit_chlratio4 <- lme(chlratio~year+age+sex+year:age,data=longformat_patients_train, random=~year|ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit7 <- jm(fitcox, list(fit_sys1, fit_chlratio4), time_var = "year",functional_forms=slope_forms)
summary(Jointfit7)

#--------------------------------------------------------------------------------Joint Model8 S2*CR3-------------------------------------------------------------------------#
fit_sys2 <- lme(sys~year+age+sex+age:sex,data=longformat_patients_train, random=~year|ID)
fit_chlratio3 <- lme(chlratio~year+age+sex+year:age+year:sex+age:sex,data=longformat_patients_train, random=~year|ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit8 <- jm(fitcox, list(fit_sys2, fit_chlratio3), time_var = "year",functional_forms=slope_forms)
summary(Jointfit8)

#--------------------------------------------------------------------------------Joint Model9 S2*CR4-------------------------------------------------------------------------#
fit_sys2 <- lme(sys~year+age+sex+age:sex,data=longformat_patients_train, random=~year|ID)
fit_chlratio4 <- lme(chlratio~year+age+sex+year:age,data=longformat_patients_train, random=~year|ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit9 <- jm(fitcox, list(fit_sys2, fit_chlratio4), time_var = "year",functional_forms=slope_forms)
summary(Jointfit9)

#--------------------------------------------------------------------------------Joint Model10 S3*CR4-------------------------------------------------------------------------#
fit_sys3 <- lme(sys~year+age+sex+year:age+year:sex+age:sex,data=longformat_patients_train, random=~year|ID)
fit_chlratio4 <- lme(chlratio~year+age+sex+year:age,data=longformat_patients_train, random=~year|ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit10 <- jm(fitcox, list(fit_sys3, fit_chlratio4), time_var = "year",functional_forms=slope_forms)
summary(Jointfit10)

save(Jointfit7,Jointfit8,Jointfit9,Jointfit10,file="/ANALYSE_AREA/P_INTG01/Chaojie/code_new/RData_s/Jointfit7to10_s.RData")

roc1 <- tvROC(Jointfit1, newdata = longformat_patients_test, Tstart = 1, Dt = 2)
AUC1 <- tvAUC(roc1)
tvBrier1 <- tvBrier(Jointfit1, newdata = longformat_patients_test, Tstart = 1, Dt = 2)

roc2 <- tvROC(Jointfit2, newdata = longformat_patients_test, Tstart = 1, Dt = 2)
AUC2 <- tvAUC(roc2)
tvBrier2 <- tvBrier(Jointfit2, newdata = longformat_patients_test, Tstart = 1, Dt = 2)

roc3 <- tvROC(Jointfit3, newdata = longformat_patients_test, Tstart = 1, Dt = 2)
AUC3 <- tvAUC(roc3)
tvBrier3 <- tvBrier(Jointfit3, newdata = longformat_patients_test, Tstart = 1, Dt = 2)

roc4 <- tvROC(Jointfit4, newdata = longformat_patients_test, Tstart = 1, Dt = 2)
AUC4 <- tvAUC(roc4)
tvBrier4 <- tvBrier(Jointfit4, newdata = longformat_patients_test, Tstart = 1, Dt = 2)

roc5 <- tvROC(Jointfit5, newdata = longformat_patients_test, Tstart = 1, Dt = 2)
AUC5 <- tvAUC(roc5)
tvBrier5 <- tvBrier(Jointfit5, newdata = longformat_patients_test, Tstart = 1, Dt = 2)

roc6 <- tvROC(Jointfit6, newdata = longformat_patients_test, Tstart = 1, Dt = 2)
AUC6 <- tvAUC(roc6)
tvBrier6 <- tvBrier(Jointfit6, newdata = longformat_patients_test, Tstart = 1, Dt = 2)

roc7 <- tvROC(Jointfit7, newdata = longformat_patients_test, Tstart = 1, Dt = 2)
AUC7 <- tvAUC(roc7)
tvBrier7 <- tvBrier(Jointfit7, newdata = longformat_patients_test, Tstart = 1, Dt = 2)

roc8 <- tvROC(Jointfit8, newdata = longformat_patients_test, Tstart = 1, Dt = 2)
AUC8 <- tvAUC(roc8)
tvBrier8 <- tvBrier(Jointfit8, newdata = longformat_patients_test, Tstart = 1, Dt = 2)

roc9 <- tvROC(Jointfit9, newdata = longformat_patients_test, Tstart = 1, Dt = 2)
AUC9 <- tvAUC(roc9)
tvBrier9 <- tvBrier(Jointfit9, newdata = longformat_patients_test, Tstart = 1, Dt = 2)

roc10 <- tvROC(Jointfit10, newdata = longformat_patients_test, Tstart = 1, Dt = 2)
AUC10 <- tvAUC(roc10)
tvBrier10 <- tvBrier(Jointfit10, newdata = longformat_patients_test, Tstart = 1, Dt = 2)

save(roc1,AUC1,roc2,AUC2,roc3,AUC3,roc4,AUC4,roc5,AUC5,roc6,AUC6,roc7,AUC7,roc8,AUC8,roc9,AUC9,roc10,AUC10,
     tvBrier1,tvBrier2,tvBrier3,tvBrier4,tvBrier5,tvBrier6,tvBrier7,tvBrier8,tvBrier9,tvBrier10,
     file="/ANALYSE_AREA/P_INTG01/Chaojie/code_new/RData_s/prediction_s.RData")

AUC_list <- c(AUC1$auc,AUC2$auc,AUC3$auc,AUC4$auc,AUC5$auc,AUC6$auc,AUC7$auc,AUC8$auc,AUC9$auc,AUC10$auc)
tvBrier_list <- c(tvBrier1$Brier,tvBrier2$Brier,tvBrier3$Brier,tvBrier4$Brier,tvBrier5$Brier,tvBrier6$Brier,tvBrier7$Brier,tvBrier8$Brier,tvBrier9$Brier,tvBrier10$Brier)

summarize_joint_models <- function(joint_models) {
  num_models <- length(joint_models)
  summaries <- vector("list", num_models)
  
  for (i in 1:num_models) {
    summaries[[i]] <- summary(joint_models[[i]])
  }
  
  # Extract desired metrics from summaries
  DIC_marginal <- sapply(summaries, function(summary) summary$fit_stats$marginal$DIC)
  LPML_marginal <- sapply(summaries, function(summary) summary$fit_stats$marginal$LPML)
  WAIC_marginal <- sapply(summaries, function(summary) summary$fit_stats$marginal$WAIC)
  DIC_conditional <- sapply(summaries, function(summary) summary$fit_stats$conditional$DIC)
  LPML_conditional <- sapply(summaries, function(summary) summary$fit_stats$conditional$LPML)
  WAIC_conditional <- sapply(summaries, function(summary) summary$fit_stats$conditional$WAIC)
  
  # Create data frames
  marginal_df <- data.frame(DIC = DIC_marginal, LPML = LPML_marginal, WAIC = WAIC_marginal)
  conditional_df <- data.frame(DIC = DIC_conditional, LPML = LPML_conditional, WAIC = WAIC_conditional)
  
  # Return the data frames
  return(list(marginal = marginal_df, conditional = conditional_df))
}

joint_models <- list(Jointfit1, Jointfit2, Jointfit3, Jointfit4, Jointfit5, Jointfit6, Jointfit7, Jointfit8, Jointfit9, Jointfit10)
summaries <- summarize_joint_models(joint_models)

marginal_list <- summaries$marginal %>%
  rename("m.DIC"=DIC,"m.LPML"=LPML,"m.WAIC"=WAIC)
conditional_list <- summaries$conditional %>%
  rename("c.DIC"=DIC,"c.LPML"=LPML,"c.WAIC"=WAIC)

values_list <- cbind(marginal_list,conditional_list,AUC_list,tvBrier_list) %>%
  `rownames<-`(paste0("Jointfit", 1:10, "_s"))

write.csv(values_list,file="/ANALYSE_AREA/P_INTG01/Chaojie/code_new/RData_s/values_s.csv")







