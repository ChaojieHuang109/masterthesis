memory.limit() 
memory.limit(35000)

library(dplyr) 
library(nlme)
library(lme4)
library(JMbayes2)
library(survival)
library(GLMMadaptive)

longformat_patients_df <- read.csv("longformat_patients_df_1.csv")%>%
  mutate(sex=factor(sex),smoking=factor(smoking),cvd=factor(cvd),death=factor(death),status=factor(status))

survival_df <- longformat_patients_df %>%
  filter(year==0) 

#-------------------------------------------------------Basic Jointmodels-----------------------------------------------------#
#---------------------------JointModel_Score1--------------------------------#
fit_sys1 <- lme(sys ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fit_chl1 <- lme(chl ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit_SCORE1 <- jm(fitcox, list(fit_sys1,fit_chl1), time_var = "year", n_iter = 5000L, n_burnin = 700L)

#---------------------------JointModel_Score2--------------------------------#
fit_sys1 <- lme(sys ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio1 <- lme(chlratio ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit_SCORE2 <- jm(fitcox, list(fit_sys1,fit_chlratio1), time_var = "year", n_iter = 5000L, n_burnin = 700L)

#------------------------------------------------------Jointmodels_updated---------------------------------------------------#
# a means that based on SCORE model, consider extra longitudinal analysis with other predictive factors in longitudinal submodels
# b means that based on SCORE model, replace other predictive factors with the predictive factor chlratio/ chl in longi
# c means that based on SCORE model, consider extra longitudinal analysis with other predictive factors in longitudinal submodels and modify surivival submodels
#------------------------------------------------------- eGFR biomaker 1------------------------------------------------------#
#-----------------------------------Jointmodels_1a-------------------------------#
fit_sys1 <- lme(sys ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio1 <- lme(chlratio ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fit_eGFR1 <- lme(eGFR ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit_1a <- jm(fitcox, list(fit_sys1,fit_chlratio1,fit_eGFR1), time_var = "year", n_iter = 5000L, n_burnin = 700L)

#-----------------------------------Jointmodels_1b-------------------------------#
fit_sys1 <- lme(sys ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fit_dia1 <- lme(eGFR ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit_1b <- jm(fitcox, list(fit_sys1,fit_dia1), time_var = "year", n_iter = 5000L, n_burnin = 700L)

#-----------------------------------Jointmodels_1c-------------------------------#
fit_sys1 <- lme(sys ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio1 <- lme(chlratio ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fit_eGFR1 <- lme(eGFR ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~age+smoking, data=survival_df)
Jointfit_1c <- jm(fitcox, list(fit_sys1,fit_chlratio1,fit_eGFR1), time_var = "year", n_iter = 5000L, n_burnin = 700L)

#------------------------------------------------------ dia biomaker 2--------------------------------------------------------#
#-----------------------------------Jointmodels_2a-------------------------------#
fit_sys1 <- lme(sys ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio1 <- lme(chlratio ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fit_dia1 <- lme(dia ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit_2a <- jm(fitcox, list(fit_sys1,fit_chlratio1,fit_dia1), time_var = "year", n_iter = 5000L, n_burnin = 700L)

#-----------------------------------Jointmodels_2b-------------------------------#
fit_sys1 <- lme(sys ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fit_dia1 <- lme(dia ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit_2b <- jm(fitcox, list(fit_sys1,fit_dia1), time_var = "year", n_iter = 5000L, n_burnin = 700L)

#-----------------------------------Jointmodels_2c-------------------------------#
fit_sys1 <- lme(sys ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio1 <- lme(chlratio ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fit_dia1 <- lme(dia ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~age+smoking, data=survival_df)
Jointfit_2a <- jm(fitcox, list(fit_sys1,fit_chlratio1,fit_dia1), time_var = "year", n_iter = 5000L, n_burnin = 700L)

#------------------------------------------------------ BMI biomaker 3--------------------------------------------------------#
#-----------------------------------Jointmodels_3a-------------------------------#
fit_sys1 <- lme(sys ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio1 <- lme(chlratio ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fit_BMI1 <- lme(BMI ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit_3a <- jm(fitcox, list(fit_sys1,fit_chlratio1,fit_BMI1), time_var = "year", n_iter = 5000L, n_burnin = 700L)

#-----------------------------------Jointmodels_3b-------------------------------#
fit_sys1 <- lme(sys ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fit_BMI1 <- lme(BMI ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit_3b <- jm(fitcox, list(fit_sys1,fit_dia1), time_var = "year", n_iter = 5000L, n_burnin = 700L)

#-----------------------------------Jointmodels_3c-------------------------------#
fit_sys1 <- lme(sys ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio1 <- lme(chlratio ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fit_BMI1 <- lme(BMI ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~age+smoking, data=survival_df)
Jointfit_3a <- jm(fitcox, list(fit_sys1,fit_chlratio1,fit_BMI1), time_var = "year", n_iter = 5000L, n_burnin = 700L)

#------------------------------------------------------ K biomaker 4----------------------------------------------------------#
#-----------------------------------Jointmodels_4a-------------------------------#
fit_sys1 <- lme(sys ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio1 <- lme(chlratio ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fit_K1 <- lme(K ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit_4a <- jm(fitcox, list(fit_sys1,fit_chlratio1,fit_K1), time_var = "year", n_iter = 5000L, n_burnin = 700L)

#-----------------------------------Jointmodels_4b-------------------------------#
fit_sys1 <- lme(sys ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fit_K1 <- lme(K ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit_4a <- jm(fitcox, list(fit_sys1,fit_K1), time_var = "year", n_iter = 5000L, n_burnin = 700L)

#-----------------------------------Jointmodels_4c-------------------------------#
fit_sys1 <- lme(sys ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio1 <- lme(chlratio ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fit_K1 <- lme(K ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~age+smoking, data=survival_df)
Jointfit_4a <- jm(fitcox, list(fit_sys1,fit_chlratio1,fit_K1), time_var = "year", n_iter = 5000L, n_burnin = 700L)

#------------------------------------------------------ alt  biomaker 5-------------------------------------------------------#
#-----------------------------------Jointmodels_5a-------------------------------#
fit_sys1 <- lme(sys ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio1 <- lme(chlratio ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fit_alt1 <- lme(eGFR ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit_5a <- jm(fitcox, list(fit_sys1,fit_chlratio1,fit_alt1), time_var = "year", n_iter = 5000L, n_burnin = 700L)

#-----------------------------------Jointmodels_5b-------------------------------#
fit_sys1 <- lme(sys ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fit_alt1 <- lme(eGFR ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit_5b <- jm(fitcox, list(fit_sys1,fit_alt1), time_var = "year", n_iter = 5000L, n_burnin = 700L)

#-----------------------------------Jointmodels_5c-------------------------------#
fit_sys1 <- lme(sys ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio1 <- lme(chlratio ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fit_alt1 <- lme(eGFR ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~age+smoking, data=survival_df)
Jointfit_5a <- jm(fitcox, list(fit_sys1,fit_chlratio1,fit_alt1), time_var = "year", n_iter = 5000L, n_burnin = 700L)







