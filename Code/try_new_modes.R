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

fit_sys1 <- lme(sys~year+age+sex+year:age+age:sex,data=longformat_patients_train, random=~year|ID)
fit_chlratio1 <- lme(chlratio~age+sex+year:age+age:sex,data=longformat_patients_train, random=~year|ID)
fit_eGFR1 <- lme(log(eGFR)~year+age+sex+year:age+age:sex,data=longformat_patients_train, random=~year|ID)
fit_hdl1 <- lme(hdl~year*sex,data=longformat_patients_train, random=~year|ID)
fitcox1 <- coxph(Surv(years, status2)~sex+age, data=survival_df)
fitcox2 <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)

# Joint Model 1 #
Jointfit1 <- jm(fitcox, list(fit_sys1,fit_chlratio1,fit_eGFR1), time_var = "year") 
summary(Jointfit1)

# Joint Model 2 #
Jointfit2 <- jm(fitcox2, list(fit_sys1,fit_eGFR1), time_var = "year") 
summary(Jointfit2)

# Joint Model 3 #
Jointfit3 <- jm(fitcox1, list(fit_sys1,fit_eGFR1), time_var = "year") 
summary(Jointfit3)

# Joint Model 4 #
Jointfit4 <- jm(fitcox2, list(fit_sys1,fit_hdl1), time_var = "year") 
summary(Jointfit4)

# Joint Model 5 #
Jointfit5 <- jm(fitcox1, list(fit_sys1,fit_hdl1), time_var = "year") 
summary(Jointfit5)

# Joint Model 6 #
Jointfit6 <- jm(fitcox2, list(fit_sys1,fit_hdl1,fit_eGFR1), time_var = "year") 
summary(Jointfit6)

# Joint Model 7 #
Jointfit7 <- jm(fitcox1, list(fit_sys1,fit_hdl1,fit_eGFR1), time_var = "year") 
summary(Jointfit7)

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
tvBrier5 <- tvBrier(Jointfit5, newdata = longformat_patients_test, Tstart = 1,Dt = 2)

AUC2$auc
AUC3$auc
AUC4$auc
AUC5$auc

save(roc4,AUC4,tvBrier4,Jointfit4,
     roc5,AUC5,tvBrier5,Jointfit5,
     file="/ANALYSE_AREA/P_INTG01/Chaojie/code_new/RData_new/tryfit4to5.RData")

save(roc2,AUC2,tvBrier2,Jointfit2,
     roc3,AUC3,tvBrier3,Jointfit3,
     file="/ANALYSE_AREA/P_INTG01/Chaojie/code_new/RData_new/tryfit2to3.RData")

