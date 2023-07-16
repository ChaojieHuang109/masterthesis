memory.limit() 
memory.limit(35000)

library(dplyr) 
library(nlme)
library(lme4)
library(JMbayes2)
library(survival)
library(GLMMadaptive)

#----------------------------------------------------------------Imputation 1----------------------------------------------------------#
longformat_patients_df <- read.csv("~/chaojie/Data/longformat_patients_df_1.csv")%>%
  mutate(sex=factor(sex),smoking=factor(smoking),cvd=factor(cvd),death=factor(death),status=factor(status))

survival_df <- longformat_patients_df %>%
  filter(year==0) 

#------------------------------------Jointmodel  1------------------------------------#
fit_sys1 <- lme(sys ~ year + age + sex + year:age + year:sex + age:sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio1 <- lme(chlratio ~ year + age + sex + year:age + year:sex + age:sex, data = longformat_patients_df, random = ~year | ID)
fit_eGFR1 <- lme(eGFR ~ year + age + sex + year:age + year:sex + age:sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit1_1 <- jm(fitcox, list(fit_sys1,fit_chlratio1,fit_eGFR1), time_var = "year", n_iter = 6000L, n_burnin = 800L)

#------------------------------------Jointmodel  2------------------------------------#
fit_sys2 <- lme(sys ~ year + age + sex + year:age + year:sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio2 <- lme(chlratio ~ year + age + sex + year:age + year:sex, data = longformat_patients_df, random = ~year | ID)
fit_eGFR2 <- lme(eGFR ~ year + age + sex + year:age + year:sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit1_2 <- jm(fitcox, list(fit_sys2,fit_chlratio2,fit_eGFR2), time_var = "year", n_iter = 6000L, n_burnin = 800L)

#------------------------------------Jointmodel  3------------------------------------#
fit_sys3 <- lme(sys ~ year + age + sex + year:age + age:sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio3 <- lme(chlratio ~ year + age + sex + year:age + age:sex, data = longformat_patients_df, random = ~year | ID)
fit_eGFR3 <- lme(eGFR ~ year + age + sex + year:age + age:sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit1_3 <- jm(fitcox, list(fit_sys3,fit_chlratio3,fit_eGFR3), time_var = "year", n_iter = 6000L, n_burnin = 800L)

#------------------------------------Jointmodel  4------------------------------------#
fit_sys4 <- lme(sys ~ year + age + sex + year:sex + age:sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio4 <- lme(chlratio ~ year + age + sex + year:sex + age:sex, data = longformat_patients_df, random = ~year | ID)
fit_eGFR4 <- lme(eGFR ~ year + age + sex + year:sex + age:sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit1_4 <- jm(fitcox, list(fit_sys4,fit_chlratio4,fit_eGFR4), time_var = "year", n_iter = 6000L, n_burnin = 800L)

#------------------------------------Jointmodel  5------------------------------------#
fit_sys5 <- lme(sys ~ year + age + sex + year:age, data = longformat_patients_df, random = ~year | ID)
fit_chlratio5 <- lme(chlratio ~ year + age + sex + year:age, data = longformat_patients_df, random = ~year | ID)
fit_eGFR5 <- lme(eGFR ~ year + age + sex + year:age, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit1_5 <- jm(fitcox, list(fit_sys5,fit_chlratio5,fit_eGFR5), time_var = "year", n_iter = 6000L, n_burnin = 800L)

#------------------------------------Jointmodel  6------------------------------------#
fit_sys6 <- lme(sys ~ year + age + sex + year:sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio6 <- lme(chlratio ~ year + age + sex + year:sex, data = longformat_patients_df, random = ~year | ID)
fit_eGFR6 <- lme(eGFR ~ year + age + sex + year:sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit1_6 <- jm(fitcox, list(fit_sys6,fit_chlratio6,fit_eGFR6), time_var = "year", n_iter = 6000L, n_burnin = 800L)

#------------------------------------Jointmodel  7------------------------------------#
fit_sys7 <- lme(sys ~ year + age + sex + age:sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio7 <- lme(chlratio ~ year + age + sex + age:sex, data = longformat_patients_df, random = ~year | ID)
fit_eGFR7 <- lme(eGFR ~ year + age + sex + age:sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit1_7 <- jm(fitcox, list(fit_sys7,fit_chlratio7,fit_eGFR7), time_var = "year", n_iter = 6000L, n_burnin = 800L)

#------------------------------------Jointmodel  8------------------------------------#
fit_sys8 <- lme(sys ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio8 <- lme(chlratio ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fit_eGFR8 <- lme(eGFR ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit1_8 <- jm(fitcox, list(fit_sys8,fit_chlratio8,fit_eGFR8), time_var = "year", n_iter = 6000L, n_burnin = 800L)

#------------------------------------Jointmodel  9------------------------------------#
fit_sys9 <- lme(sys ~ year + age, data = longformat_patients_df, random = ~year | ID)
fit_chlratio9 <- lme(chlratio ~ year + age, data = longformat_patients_df, random = ~year | ID)
fit_eGFR9 <- lme(eGFR ~ year + age, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit1_9 <- jm(fitcox, list(fit_sys9,fit_chlratio9,fit_eGFR9), time_var = "year", n_iter = 6000L, n_burnin = 800L)

#------------------------------------Jointmodel  10------------------------------------#
fit_sys10 <- lme(sys ~ year + sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio10 <- lme(chlratio ~ year + sex, data = longformat_patients_df, random = ~year | ID)
fit_eGFR10 <- lme(eGFR ~ year + sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit1_10 <- jm(fitcox, list(fit_sys10,fit_chlratio10,fit_eGFR10), time_var = "year", n_iter = 6000L, n_burnin = 800L)

#------------------------------------Jointmodel  11------------------------------------#
fit_sys11 <- lme(sys ~ year * age, data = longformat_patients_df, random = ~year | ID)
fit_chlratio11 <- lme(chlratio ~ year * age, data = longformat_patients_df, random = ~year | ID)
fit_eGFR11 <- lme(eGFR ~ year * age, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit1_11 <- jm(fitcox, list(fit_sys11,fit_chlratio11,fit_eGFR11), time_var = "year", n_iter = 6000L, n_burnin = 800L)

#------------------------------------Jointmodel  12------------------------------------#
fit_sys12 <- lme(sys ~ year * sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio12 <- lme(chlratio ~ year * sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
summary(fitcox)
Jointfit1_12 <- jm(fitcox, list(fit_sys12,fit_chlratio12,fit_eGFR12), time_var = "year", n_iter = 6000L, n_burnin = 800L)


#----------------------------------------------------------------Imputation 2----------------------------------------------------------#
longformat_patients_df <- read.csv("~/chaojie/Data/longformat_patients_df_2.csv")%>%
  mutate(sex=factor(sex),smoking=factor(smoking),cvd=factor(cvd),death=factor(death),status=factor(status))

survival_df <- longformat_patients_df %>%
  filter(year==0) 

#------------------------------------Jointmodel  1------------------------------------#
fit_sys1 <- lme(sys ~ year + age + sex + year:age + year:sex + age:sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio1 <- lme(chlratio ~ year + age + sex + year:age + year:sex + age:sex, data = longformat_patients_df, random = ~year | ID)
fit_eGFR1 <- lme(eGFR ~ year + age + sex + year:age + year:sex + age:sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit2_1 <- jm(fitcox, list(fit_sys1,fit_chlratio1,fit_eGFR1), time_var = "year", n_iter = 6000L, n_burnin = 800L)

#------------------------------------Jointmodel  2------------------------------------#
fit_sys2 <- lme(sys ~ year + age + sex + year:age + year:sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio2 <- lme(chlratio ~ year + age + sex + year:age + year:sex, data = longformat_patients_df, random = ~year | ID)
fit_eGFR2 <- lme(eGFR ~ year + age + sex + year:age + year:sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit2_2 <- jm(fitcox, list(fit_sys2,fit_chlratio2,fit_eGFR2), time_var = "year", n_iter = 6000L, n_burnin = 800L)

#------------------------------------Jointmodel  3------------------------------------#
fit_sys3 <- lme(sys ~ year + age + sex + year:age + age:sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio3 <- lme(chlratio ~ year + age + sex + year:age + age:sex, data = longformat_patients_df, random = ~year | ID)
fit_eGFR3 <- lme(eGFR ~ year + age + sex + year:age + age:sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit2_3 <- jm(fitcox, list(fit_sys3,fit_chlratio3,fit_eGFR3), time_var = "year", n_iter = 6000L, n_burnin = 800L)

#------------------------------------Jointmodel  4------------------------------------#
fit_sys4 <- lme(sys ~ year + age + sex + year:sex + age:sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio4 <- lme(chlratio ~ year + age + sex + year:sex + age:sex, data = longformat_patients_df, random = ~year | ID)
fit_eGFR4 <- lme(eGFR ~ year + age + sex + year:sex + age:sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit2_4 <- jm(fitcox, list(fit_sys4,fit_chlratio4,fit_eGFR4), time_var = "year", n_iter = 6000L, n_burnin = 800L)

#------------------------------------Jointmodel  5------------------------------------#
fit_sys5 <- lme(sys ~ year + age + sex + year:age, data = longformat_patients_df, random = ~year | ID)
fit_chlratio5 <- lme(chlratio ~ year + age + sex + year:age, data = longformat_patients_df, random = ~year | ID)
fit_eGFR5 <- lme(eGFR ~ year + age + sex + year:age, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit2_5 <- jm(fitcox, list(fit_sys5,fit_chlratio5,fit_eGFR5), time_var = "year", n_iter = 6000L, n_burnin = 800L)

#------------------------------------Jointmodel  6------------------------------------#
fit_sys6 <- lme(sys ~ year + age + sex + year:sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio6 <- lme(chlratio ~ year + age + sex + year:sex, data = longformat_patients_df, random = ~year | ID)
fit_eGFR6 <- lme(eGFR ~ year + age + sex + year:sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit2_6 <- jm(fitcox, list(fit_sys6,fit_chlratio6,fit_eGFR6), time_var = "year", n_iter = 6000L, n_burnin = 800L)

#------------------------------------Jointmodel  7------------------------------------#
fit_sys7 <- lme(sys ~ year + age + sex + age:sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio7 <- lme(chlratio ~ year + age + sex + age:sex, data = longformat_patients_df, random = ~year | ID)
fit_eGFR7 <- lme(eGFR ~ year + age + sex + age:sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit2_7 <- jm(fitcox, list(fit_sys7,fit_chlratio7,fit_eGFR7), time_var = "year", n_iter = 6000L, n_burnin = 800L)

#------------------------------------Jointmodel  8------------------------------------#
fit_sys8 <- lme(sys ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio8 <- lme(chlratio ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fit_eGFR8 <- lme(eGFR ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit2_8 <- jm(fitcox, list(fit_sys8,fit_chlratio8,fit_eGFR8), time_var = "year", n_iter = 6000L, n_burnin = 800L)

#------------------------------------Jointmodel  9------------------------------------#
fit_sys9 <- lme(sys ~ year + age, data = longformat_patients_df, random = ~year | ID)
fit_chlratio9 <- lme(chlratio ~ year + age, data = longformat_patients_df, random = ~year | ID)
fit_eGFR9 <- lme(eGFR ~ year + age, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit2_9 <- jm(fitcox, list(fit_sys9,fit_chlratio9,fit_eGFR9), time_var = "year", n_iter = 6000L, n_burnin = 800L)

#------------------------------------Jointmodel  10------------------------------------#
fit_sys10 <- lme(sys ~ year + sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio10 <- lme(chlratio ~ year + sex, data = longformat_patients_df, random = ~year | ID)
fit_eGFR10 <- lme(eGFR ~ year + sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit2_10 <- jm(fitcox, list(fit_sys10,fit_chlratio10,fit_eGFR10), time_var = "year", n_iter = 6000L, n_burnin = 800L)

#------------------------------------Jointmodel  11------------------------------------#
fit_sys11 <- lme(sys ~ year * age, data = longformat_patients_df, random = ~year | ID)
fit_chlratio11 <- lme(chlratio ~ year * age, data = longformat_patients_df, random = ~year | ID)
fit_eGFR11 <- lme(eGFR ~ year * age, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit2_11 <- jm(fitcox, list(fit_sys11,fit_chlratio11,fit_eGFR11), time_var = "year", n_iter = 6000L, n_burnin = 800L)

#------------------------------------Jointmodel  12------------------------------------#
fit_sys12 <- lme(sys ~ year * sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio12 <- lme(chlratio ~ year * sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
summary(fitcox)
Jointfit2_12 <- jm(fitcox, list(fit_sys12,fit_chlratio12,fit_eGFR12), time_var = "year", n_iter = 6000L, n_burnin = 800L)

#----------------------------------------------------------------Imputation 3----------------------------------------------------------#
longformat_patients_df <- read.csv("~/chaojie/Data/longformat_patients_df_3.csv")%>%
  mutate(sex=factor(sex),smoking=factor(smoking),cvd=factor(cvd),death=factor(death),status=factor(status))

survival_df <- longformat_patients_df %>%
  filter(year==0) 

#------------------------------------Jointmodel  1------------------------------------#
fit_sys1 <- lme(sys ~ year + age + sex + year:age + year:sex + age:sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio1 <- lme(chlratio ~ year + age + sex + year:age + year:sex + age:sex, data = longformat_patients_df, random = ~year | ID)
fit_eGFR1 <- lme(eGFR ~ year + age + sex + year:age + year:sex + age:sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit3_1 <- jm(fitcox, list(fit_sys1,fit_chlratio1,fit_eGFR1), time_var = "year", n_iter = 6000L, n_burnin = 800L)

#------------------------------------Jointmodel  2------------------------------------#
fit_sys2 <- lme(sys ~ year + age + sex + year:age + year:sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio2 <- lme(chlratio ~ year + age + sex + year:age + year:sex, data = longformat_patients_df, random = ~year | ID)
fit_eGFR2 <- lme(eGFR ~ year + age + sex + year:age + year:sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit3_2 <- jm(fitcox, list(fit_sys2,fit_chlratio2,fit_eGFR2), time_var = "year", n_iter = 6000L, n_burnin = 800L)

#------------------------------------Jointmodel  3------------------------------------#
fit_sys3 <- lme(sys ~ year + age + sex + year:age + age:sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio3 <- lme(chlratio ~ year + age + sex + year:age + age:sex, data = longformat_patients_df, random = ~year | ID)
fit_eGFR3 <- lme(eGFR ~ year + age + sex + year:age + age:sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit3_3 <- jm(fitcox, list(fit_sys3,fit_chlratio3,fit_eGFR3), time_var = "year", n_iter = 6000L, n_burnin = 800L)

#------------------------------------Jointmodel  4------------------------------------#
fit_sys4 <- lme(sys ~ year + age + sex + year:sex + age:sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio4 <- lme(chlratio ~ year + age + sex + year:sex + age:sex, data = longformat_patients_df, random = ~year | ID)
fit_eGFR4 <- lme(eGFR ~ year + age + sex + year:sex + age:sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit3_4 <- jm(fitcox, list(fit_sys4,fit_chlratio4,fit_eGFR4), time_var = "year", n_iter = 6000L, n_burnin = 800L)

#------------------------------------Jointmodel  5------------------------------------#
fit_sys5 <- lme(sys ~ year + age + sex + year:age, data = longformat_patients_df, random = ~year | ID)
fit_chlratio5 <- lme(chlratio ~ year + age + sex + year:age, data = longformat_patients_df, random = ~year | ID)
fit_eGFR5 <- lme(eGFR ~ year + age + sex + year:age, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit3_5 <- jm(fitcox, list(fit_sys5,fit_chlratio5,fit_eGFR5), time_var = "year", n_iter = 6000L, n_burnin = 800L)

#------------------------------------Jointmodel  6------------------------------------#
fit_sys6 <- lme(sys ~ year + age + sex + year:sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio6 <- lme(chlratio ~ year + age + sex + year:sex, data = longformat_patients_df, random = ~year | ID)
fit_eGFR6 <- lme(eGFR ~ year + age + sex + year:sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit3_6 <- jm(fitcox, list(fit_sys6,fit_chlratio6,fit_eGFR6), time_var = "year", n_iter = 6000L, n_burnin = 800L)

#------------------------------------Jointmodel  7------------------------------------#
fit_sys7 <- lme(sys ~ year + age + sex + age:sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio7 <- lme(chlratio ~ year + age + sex + age:sex, data = longformat_patients_df, random = ~year | ID)
fit_eGFR7 <- lme(eGFR ~ year + age + sex + age:sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit3_7 <- jm(fitcox, list(fit_sys7,fit_chlratio7,fit_eGFR7), time_var = "year", n_iter = 6000L, n_burnin = 800L)

#------------------------------------Jointmodel  8------------------------------------#
fit_sys8 <- lme(sys ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio8 <- lme(chlratio ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fit_eGFR8 <- lme(eGFR ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit3_8 <- jm(fitcox, list(fit_sys8,fit_chlratio8,fit_eGFR8), time_var = "year", n_iter = 6000L, n_burnin = 800L)

#------------------------------------Jointmodel  9------------------------------------#
fit_sys9 <- lme(sys ~ year + age, data = longformat_patients_df, random = ~year | ID)
fit_chlratio9 <- lme(chlratio ~ year + age, data = longformat_patients_df, random = ~year | ID)
fit_eGFR9 <- lme(eGFR ~ year + age, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit3_9 <- jm(fitcox, list(fit_sys9,fit_chlratio9,fit_eGFR9), time_var = "year", n_iter = 6000L, n_burnin = 800L)

#------------------------------------Jointmodel  10------------------------------------#
fit_sys10 <- lme(sys ~ year + sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio10 <- lme(chlratio ~ year + sex, data = longformat_patients_df, random = ~year | ID)
fit_eGFR10 <- lme(eGFR ~ year + sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit3_10 <- jm(fitcox, list(fit_sys10,fit_chlratio10,fit_eGFR10), time_var = "year", n_iter = 6000L, n_burnin = 800L)

#------------------------------------Jointmodel  11------------------------------------#
fit_sys11 <- lme(sys ~ year * age, data = longformat_patients_df, random = ~year | ID)
fit_chlratio11 <- lme(chlratio ~ year * age, data = longformat_patients_df, random = ~year | ID)
fit_eGFR11 <- lme(eGFR ~ year * age, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit3_11 <- jm(fitcox, list(fit_sys11,fit_chlratio11,fit_eGFR11), time_var = "year", n_iter = 6000L, n_burnin = 800L)

#------------------------------------Jointmodel  12------------------------------------#
fit_sys12 <- lme(sys ~ year * sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio12 <- lme(chlratio ~ year * sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
summary(fitcox)
Jointfit3_12 <- jm(fitcox, list(fit_sys12,fit_chlratio12,fit_eGFR12), time_var = "year", n_iter = 6000L, n_burnin = 800L)
