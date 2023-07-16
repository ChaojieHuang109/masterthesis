library(dplyr) 
library(nlme)
library(lme4)
library(JMbayes2)
library(survival)
library(GLMMadaptive)

#----------------------------------------------------------------Imputation 1----------------------------------------------------------#
longformat_patients_df <- read.csv("~/chaojie/Data/longformat_patients_df_3.csv")%>%
  mutate(sex=factor(sex),smoking=factor(smoking),cvd=factor(cvd),death=factor(death),status=factor(status))

survival_df <- longformat_patients_df %>%
  filter(year==0) 

Functional_Forms <- list("sys" =~ value(sys) + slope(sys),
                         "chlratio" =~ value(chlratio) + slope(chlratio),
                         "eGFR" =~ value(eGFR) + slope(eGFR))

#------------------------------------Jointmodel  1------------------------------------#
fit_sys1 <- lme(sys ~ year + age + sex + year:age + year:sex + age:sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio1 <- lme(chlratio ~ year + age + sex + year:age + year:sex + age:sex, data = longformat_patients_df, random = ~year | ID)
fit_eGFR1 <- lme(eGFR ~ year + age + sex + year:age + year:sex + age:sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit1_1s <- jm(fitcox, list(fit_sys1,fit_chlratio1,fit_eGFR1), time_var = "year", n_iter = 6000L, n_burnin = 800L,
                   functional_forms = Functional_Forms)
save(Jointfit1_1s, file="~/chaojie/RData/IMP1/Jointfit1_1s.RData")

#------------------------------------Jointmodel  2------------------------------------#
fit_sys2 <- lme(sys ~ year + age + sex + year:age + year:sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio2 <- lme(chlratio ~ year + age + sex + year:age + year:sex, data = longformat_patients_df, random = ~year | ID)
fit_eGFR2 <- lme(eGFR ~ year + age + sex + year:age + year:sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit1_2s <- jm(fitcox, list(fit_sys2,fit_chlratio2,fit_eGFR2), time_var = "year", n_iter = 6000L, n_burnin = 800L,
                   functional_forms = Functional_Forms)
save(Jointfit1_2s, file="~/chaojie/RData/IMP1/Jointfit1_2s.RData")
summary(Jointfit1_2s)
#------------------------------------Jointmodel  3------------------------------------#
fit_sys3 <- lme(sys ~ year + age + sex + year:age + age:sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio3 <- lme(chlratio ~ year + age + sex + year:age + age:sex, data = longformat_patients_df, random = ~year | ID)
fit_eGFR3 <- lme(eGFR ~ year + age + sex + year:age + age:sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit1_3s <- jm(fitcox, list(fit_sys3,fit_chlratio3,fit_eGFR3), time_var = "year", n_iter = 6000L, n_burnin = 800L,
                   functional_forms = Functional_Forms)
save(Jointfit1_3s, file="~/chaojie/RData/IMP1/Jointfit1_3s.RData")

#------------------------------------Jointmodel  4------------------------------------#
fit_sys4 <- lme(sys ~ year + age + sex + year:sex + age:sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio4 <- lme(chlratio ~ year + age + sex + year:sex + age:sex, data = longformat_patients_df, random = ~year | ID)
fit_eGFR4 <- lme(eGFR ~ year + age + sex + year:sex + age:sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit1_4 <- jm(fitcox, list(fit_sys4,fit_chlratio4,fit_eGFR4), time_var = "year", n_iter = 6000L, n_burnin = 800L,
                  functional_forms = Functional_Forms)
save(Jointfit1_4s, file="~/chaojie/RData/IMP1/Jointfit1_4s.RData")

#------------------------------------Jointmodel  5------------------------------------#
fit_sys5 <- lme(sys ~ year + age + sex + year:age, data = longformat_patients_df, random = ~year | ID)
fit_chlratio5 <- lme(chlratio ~ year + age + sex + year:age, data = longformat_patients_df, random = ~year | ID)
fit_eGFR5 <- lme(eGFR ~ year + age + sex + year:age, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit1_5s <- jm(fitcox, list(fit_sys5,fit_chlratio5,fit_eGFR5), time_var = "year", n_iter = 6000L, n_burnin = 800L,
                   functional_forms = Functional_Forms)
save(Jointfit1_5s, file="~/chaojie/RData/IMP1/Jointfit1_5s.RData")

#------------------------------------Jointmodel  6------------------------------------#
fit_sys6 <- lme(sys ~ year + age + sex + year:sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio6 <- lme(chlratio ~ year + age + sex + year:sex, data = longformat_patients_df, random = ~year | ID)
fit_eGFR6 <- lme(eGFR ~ year + age + sex + year:sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit1_6s <- jm(fitcox, list(fit_sys6,fit_chlratio6,fit_eGFR6), time_var = "year", n_iter = 6000L, n_burnin = 800L,
                   functional_forms = Functional_Forms)
save(Jointfit1_6s, file="~/chaojie/RData/IMP1/Jointfit1_6s.RData")

#------------------------------------Jointmodel  7------------------------------------#
fit_sys7 <- lme(sys ~ year + age + sex + age:sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio7 <- lme(chlratio ~ year + age + sex + age:sex, data = longformat_patients_df, random = ~year | ID)
fit_eGFR7 <- lme(eGFR ~ year + age + sex + age:sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit1_7s <- jm(fitcox, list(fit_sys7,fit_chlratio7,fit_eGFR7), time_var = "year", n_iter = 6000L, n_burnin = 800L,
                   functional_forms = Functional_Forms)
save(Jointfit1_7s, file="~/chaojie/RData/IMP1/Jointfit1_7s.RData")

#------------------------------------Jointmodel  8------------------------------------#
fit_sys8 <- lme(sys ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio8 <- lme(chlratio ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fit_eGFR8 <- lme(eGFR ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit1_8s <- jm(fitcox, list(fit_sys8,fit_chlratio8,fit_eGFR8), time_var = "year", n_iter = 6000L, n_burnin = 800L,
                   functional_forms = Functional_Forms)
save(Jointfit1_8s, file="~/chaojie/RData/IMP1/Jointfit1_8s.RData")

#------------------------------------Jointmodel  9------------------------------------#
fit_sys9 <- lme(sys ~ year + age, data = longformat_patients_df, random = ~year | ID)
fit_chlratio9 <- lme(chlratio ~ year + age, data = longformat_patients_df, random = ~year | ID)
fit_eGFR9 <- lme(eGFR ~ year + age, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit1_9s <- jm(fitcox, list(fit_sys9,fit_chlratio9,fit_eGFR9), time_var = "year", n_iter = 6000L, n_burnin = 800L,
                   functional_forms = Functional_Forms)
save(Jointfit1_9s, file="~/chaojie/RData/IMP1/Jointfit1_9s.RData")

#------------------------------------Jointmodel  10------------------------------------#
fit_sys10 <- lme(sys ~ year + sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio10 <- lme(chlratio ~ year + sex, data = longformat_patients_df, random = ~year | ID)
fit_eGFR10 <- lme(eGFR ~ year + sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit1_10s <- jm(fitcox, list(fit_sys10,fit_chlratio10,fit_eGFR10), time_var = "year", n_iter = 6000L, n_burnin = 800L,
                    functional_forms = Functional_Forms)
save(Jointfit1_10s, file="~/chaojie/RData/IMP1/Jointfit1_10s.RData")

#------------------------------------Jointmodel  11------------------------------------#
fit_sys11 <- lme(sys ~ year * age, data = longformat_patients_df, random = ~year | ID)
fit_chlratio11 <- lme(chlratio ~ year * age, data = longformat_patients_df, random = ~year | ID)
fit_eGFR11 <- lme(eGFR ~ year * age, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit1_11s <- jm(fitcox, list(fit_sys11,fit_chlratio11,fit_eGFR11), time_var = "year", n_iter = 6000L, n_burnin = 800L,
                    functional_forms = Functional_Forms)
save(Jointfit1_11s, file="~/chaojie/RData/IMP1/Jointfit1_11s.RData")
#------------------------------------Jointmodel  12------------------------------------#
fit_sys12 <- lme(sys ~ year * sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio12 <- lme(chlratio ~ year * sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
summary(fitcox)
Jointfit1_12s <- jm(fitcox, list(fit_sys12,fit_chlratio12,fit_eGFR12), time_var = "year", n_iter = 6000L, n_burnin = 800L,
                    functional_forms = Functional_Forms)
save(Jointfit1_12s, file="~/chaojie/RData/IMP1/Jointfit1_12s.RData")

#----------------------------------------------------------------Imputation 2----------------------------------------------------------#
longformat_patients_df <- read.csv("longformat_patients_df_4.csv")%>%
  mutate(sex=factor(sex),smoking=factor(smoking),cvd=factor(cvd),death=factor(death),status=factor(status))

survival_df <- longformat_patients_df %>%
  filter(year==0) 

#------------------------------------Jointmodel  1------------------------------------#
fit_sys1 <- lme(sys ~ year + age + sex + year:age + year:sex + age:sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio1 <- lme(chlratio ~ year + age + sex + year:age + year:sex + age:sex, data = longformat_patients_df, random = ~year | ID)
fit_eGFR1 <- lme(eGFR ~ year + age + sex + year:age + year:sex + age:sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit2_1s <- jm(fitcox, list(fit_sys1,fit_chlratio1,fit_eGFR1), time_var = "year", n_iter = 6000L, n_burnin = 800L,
                   functional_forms = Functional_Forms)
save(Jointfit2_1s, file="~/chaojie/RData/IMP2/Jointfit2_1s.RData")

#------------------------------------Jointmodel  2------------------------------------#
fit_sys2 <- lme(sys ~ year + age + sex + year:age + year:sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio2 <- lme(chlratio ~ year + age + sex + year:age + year:sex, data = longformat_patients_df, random = ~year | ID)
fit_eGFR2 <- lme(eGFR ~ year + age + sex + year:age + year:sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit2_2s <- jm(fitcox, list(fit_sys2,fit_chlratio2,fit_eGFR2), time_var = "year", n_iter = 6000L, n_burnin = 800L,
                   functional_forms = Functional_Forms)
save(Jointfit2_2s, file="~/chaojie/RData/IMP2/Jointfit2_2s.RData")

#------------------------------------Jointmodel  3------------------------------------#
fit_sys3 <- lme(sys ~ year + age + sex + year:age + age:sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio3 <- lme(chlratio ~ year + age + sex + year:age + age:sex, data = longformat_patients_df, random = ~year | ID)
fit_eGFR3 <- lme(eGFR ~ year + age + sex + year:age + age:sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit2_3s <- jm(fitcox, list(fit_sys3,fit_chlratio3,fit_eGFR3), time_var = "year", n_iter = 6000L, n_burnin = 800L,
                   functional_forms = Functional_Forms)
save(Jointfit2_3s, file="~/chaojie/RData/IMP2/Jointfit2_3s.RData")

#------------------------------------Jointmodel  4------------------------------------#
fit_sys4 <- lme(sys ~ year + age + sex + year:sex + age:sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio4 <- lme(chlratio ~ year + age + sex + year:sex + age:sex, data = longformat_patients_df, random = ~year | ID)
fit_eGFR4 <- lme(eGFR ~ year + age + sex + year:sex + age:sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit2_4s <- jm(fitcox, list(fit_sys4,fit_chlratio4,fit_eGFR4), time_var = "year", n_iter = 6000L, n_burnin = 800L,
                   functional_forms = Functional_Forms)
save(Jointfit2_4s, file="~/chaojie/RData/IMP2/Jointfit2_4s.RData")

#------------------------------------Jointmodel  5------------------------------------#
fit_sys5 <- lme(sys ~ year + age + sex + year:age, data = longformat_patients_df, random = ~year | ID)
fit_chlratio5 <- lme(chlratio ~ year + age + sex + year:age, data = longformat_patients_df, random = ~year | ID)
fit_eGFR5 <- lme(eGFR ~ year + age + sex + year:age, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit2_5s <- jm(fitcox, list(fit_sys5,fit_chlratio5,fit_eGFR5), time_var = "year", n_iter = 6000L, n_burnin = 800L,
                   functional_forms = Functional_Forms)
save(Jointfit2_5s, file="~/chaojie/RData/IMP2/Jointfit2_5s.RData")

#------------------------------------Jointmodel  6------------------------------------#
fit_sys6 <- lme(sys ~ year + age + sex + year:sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio6 <- lme(chlratio ~ year + age + sex + year:sex, data = longformat_patients_df, random = ~year | ID)
fit_eGFR6 <- lme(eGFR ~ year + age + sex + year:sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit2_6s <- jm(fitcox, list(fit_sys6,fit_chlratio6,fit_eGFR6), time_var = "year", n_iter = 6000L, n_burnin = 800L,
                   functional_forms = Functional_Forms)
save(Jointfit2_6s, file="~/chaojie/RData/IMP2/Jointfit2_6s.RData")

#------------------------------------Jointmodel  7------------------------------------#
fit_sys7 <- lme(sys ~ year + age + sex + age:sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio7 <- lme(chlratio ~ year + age + sex + age:sex, data = longformat_patients_df, random = ~year | ID)
fit_eGFR7 <- lme(eGFR ~ year + age + sex + age:sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit2_7s <- jm(fitcox, list(fit_sys7,fit_chlratio7,fit_eGFR7), time_var = "year", n_iter = 6000L, n_burnin = 800L,
                   functional_forms = Functional_Forms)
save(Jointfit2_7s, file="~/chaojie/RData/IMP2/Jointfit2_7s.RData")

#------------------------------------Jointmodel  8------------------------------------#
fit_sys8 <- lme(sys ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio8 <- lme(chlratio ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fit_eGFR8 <- lme(eGFR ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit2_8s <- jm(fitcox, list(fit_sys8,fit_chlratio8,fit_eGFR8), time_var = "year", n_iter = 6000L, n_burnin = 800L,
                   functional_forms = Functional_Forms)
save(Jointfit2_8s, file="~/chaojie/RData/IMP2/Jointfit2_8s.RData")

#------------------------------------Jointmodel  9------------------------------------#
fit_sys9 <- lme(sys ~ year + age, data = longformat_patients_df, random = ~year | ID)
fit_chlratio9 <- lme(chlratio ~ year + age, data = longformat_patients_df, random = ~year | ID)
fit_eGFR9 <- lme(eGFR ~ year + age, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit2_9s <- jm(fitcox, list(fit_sys9,fit_chlratio9,fit_eGFR9), time_var = "year", n_iter = 6000L, n_burnin = 800L,
                   functional_forms = Functional_Forms)
save(Jointfit2_9s, file="~/chaojie/RData/IMP2/Jointfit2_9s.RData")

#------------------------------------Jointmodel  10------------------------------------#
fit_sys10 <- lme(sys ~ year + sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio10 <- lme(chlratio ~ year + sex, data = longformat_patients_df, random = ~year | ID)
fit_eGFR10 <- lme(eGFR ~ year + sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit2_10s <- jm(fitcox, list(fit_sys10,fit_chlratio10,fit_eGFR10), time_var = "year", n_iter = 6000L, n_burnin = 800L,
                    functional_forms = Functional_Forms)
save(Jointfit2_10s, file="~/chaojie/RData/IMP2/Jointfit2_10s.RData")

#------------------------------------Jointmodel  11------------------------------------#
fit_sys11 <- lme(sys ~ year * age, data = longformat_patients_df, random = ~year | ID)
fit_chlratio11 <- lme(chlratio ~ year * age, data = longformat_patients_df, random = ~year | ID)
fit_eGFR11 <- lme(eGFR ~ year * age, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit2_11s <- jm(fitcox, list(fit_sys11,fit_chlratio11,fit_eGFR11), time_var = "year", n_iter = 6000L, n_burnin = 800L,
                    functional_forms = Functional_Forms)
save(Jointfit2_11s, file="~/chaojie/RData/IMP2/Jointfit2_11s.RData")

#------------------------------------Jointmodel  12------------------------------------#
fit_sys12 <- lme(sys ~ year * sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio12 <- lme(chlratio ~ year * sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
summary(fitcox)
Jointfit2_12s <- jm(fitcox, list(fit_sys12,fit_chlratio12,fit_eGFR12), time_var = "year", n_iter = 6000L, n_burnin = 800L,
                    functional_forms = Functional_Forms)
save(Jointfit2_12s, file="~/chaojie/RData/IMP2/Jointfit2_12s.RData")

#----------------------------------------------------------------Imputation 3----------------------------------------------------------#
longformat_patients_df <- read.csv("longformat_patients_df_3.csv")%>%
  mutate(sex=factor(sex),smoking=factor(smoking),cvd=factor(cvd),death=factor(death),status=factor(status))

survival_df <- longformat_patients_df %>%
  filter(year==0) 

#------------------------------------Jointmodel  1------------------------------------#
fit_sys1 <- lme(sys ~ year + age + sex + year:age + year:sex + age:sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio1 <- lme(chlratio ~ year + age + sex + year:age + year:sex + age:sex, data = longformat_patients_df, random = ~year | ID)
fit_eGFR1 <- lme(eGFR ~ year + age + sex + year:age + year:sex + age:sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit3_1s <- jm(fitcox, list(fit_sys1,fit_chlratio1,fit_eGFR1), time_var = "year", n_iter = 6000L, n_burnin = 800L,
                   functional_forms = Functional_Forms)
save(Jointfit3_1s, file="~/chaojie/RData/IMP3/Jointfit3_1s.RData")

#------------------------------------Jointmodel  2------------------------------------#
fit_sys2 <- lme(sys ~ year + age + sex + year:age + year:sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio2 <- lme(chlratio ~ year + age + sex + year:age + year:sex, data = longformat_patients_df, random = ~year | ID)
fit_eGFR2 <- lme(eGFR ~ year + age + sex + year:age + year:sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit3_2s <- jm(fitcox, list(fit_sys2,fit_chlratio2,fit_eGFR2), time_var = "year", n_iter = 6000L, n_burnin = 800L,
                   functional_forms = Functional_Forms)
save(Jointfit3_2s, file="~/chaojie/RData/IMP3/Jointfit3_2s.RData")

#------------------------------------Jointmodel  3------------------------------------#
fit_sys3 <- lme(sys ~ year + age + sex + year:age + age:sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio3 <- lme(chlratio ~ year + age + sex + year:age + age:sex, data = longformat_patients_df, random = ~year | ID)
fit_eGFR3 <- lme(eGFR ~ year + age + sex + year:age + age:sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit3_3s <- jm(fitcox, list(fit_sys3,fit_chlratio3,fit_eGFR3), time_var = "year", n_iter = 6000L, n_burnin = 800L,
                   functional_forms = Functional_Forms)
save(Jointfit3_3s, file="~/chaojie/RData/IMP3/Jointfit3_3s.RData")

#------------------------------------Jointmodel  4------------------------------------#
fit_sys4 <- lme(sys ~ year + age + sex + year:sex + age:sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio4 <- lme(chlratio ~ year + age + sex + year:sex + age:sex, data = longformat_patients_df, random = ~year | ID)
fit_eGFR4 <- lme(eGFR ~ year + age + sex + year:sex + age:sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit3_4s <- jm(fitcox, list(fit_sys4,fit_chlratio4,fit_eGFR4), time_var = "year", n_iter = 6000L, n_burnin = 800L,
                   functional_forms = Functional_Forms)
save(Jointfit3_4s, file="~/chaojie/RData/IMP3/Jointfit3_4s.RData")

#------------------------------------Jointmodel  5------------------------------------#
fit_sys5 <- lme(sys ~ year + age + sex + year:age, data = longformat_patients_df, random = ~year | ID)
fit_chlratio5 <- lme(chlratio ~ year + age + sex + year:age, data = longformat_patients_df, random = ~year | ID)
fit_eGFR5 <- lme(eGFR ~ year + age + sex + year:age, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit3_5s <- jm(fitcox, list(fit_sys5,fit_chlratio5,fit_eGFR5), time_var = "year", n_iter = 6000L, n_burnin = 800L,
                   functional_forms = Functional_Forms)
save(Jointfit3_5s, file="~/chaojie/RData/IMP3/Jointfit3_5s.RData")

#------------------------------------Jointmodel  6------------------------------------#
fit_sys6 <- lme(sys ~ year + age + sex + year:sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio6 <- lme(chlratio ~ year + age + sex + year:sex, data = longformat_patients_df, random = ~year | ID)
fit_eGFR6 <- lme(eGFR ~ year + age + sex + year:sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit3_6s <- jm(fitcox, list(fit_sys6,fit_chlratio6,fit_eGFR6), time_var = "year", n_iter = 6000L, n_burnin = 800L,
                   functional_forms = Functional_Forms)
save(Jointfit3_6s, file="~/chaojie/RData/IMP3/Jointfit3_6s.RData")

#------------------------------------Jointmodel  7------------------------------------#
fit_sys7 <- lme(sys ~ year + age + sex + age:sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio7 <- lme(chlratio ~ year + age + sex + age:sex, data = longformat_patients_df, random = ~year | ID)
fit_eGFR7 <- lme(eGFR ~ year + age + sex + age:sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit3_7s <- jm(fitcox, list(fit_sys7,fit_chlratio7,fit_eGFR7), time_var = "year", n_iter = 6000L, n_burnin = 800L,
                   functional_forms = Functional_Forms)
save(Jointfit3_7s, file="~/chaojie/RData/IMP3/Jointfit3_7s.RData")

#------------------------------------Jointmodel  8------------------------------------#
fit_sys8 <- lme(sys ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio8 <- lme(chlratio ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fit_eGFR8 <- lme(eGFR ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit3_8s <- jm(fitcox, list(fit_sys8,fit_chlratio8,fit_eGFR8), time_var = "year", n_iter = 6000L, n_burnin = 800L,
                   functional_forms = Functional_Forms)
save(Jointfit3_8s, file="~/chaojie/RData/IMP3/Jointfit3_8s.RData")

#------------------------------------Jointmodel  9------------------------------------#
fit_sys9 <- lme(sys ~ year + age, data = longformat_patients_df, random = ~year | ID)
fit_chlratio9 <- lme(chlratio ~ year + age, data = longformat_patients_df, random = ~year | ID)
fit_eGFR9 <- lme(eGFR ~ year + age, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit3_9 <- jm(fitcox, list(fit_sys9,fit_chlratio9,fit_eGFR9), time_var = "year", n_iter = 6000L, n_burnin = 800L,
                  functional_forms = Functional_Forms)
save(Jointfit3_9s, file="~/chaojie/RData/IMP3/Jointfit3_9s.RData")

#------------------------------------Jointmodel  10------------------------------------#
fit_sys10 <- lme(sys ~ year + sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio10 <- lme(chlratio ~ year + sex, data = longformat_patients_df, random = ~year | ID)
fit_eGFR10 <- lme(eGFR ~ year + sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit3_10s <- jm(fitcox, list(fit_sys10,fit_chlratio10,fit_eGFR10), time_var = "year", n_iter = 6000L, n_burnin = 800L,
                    functional_forms = Functional_Forms)
save(Jointfit3_10s, file="~/chaojie/RData/IMP3/Jointfit3_10s.RData")

#------------------------------------Jointmodel  11------------------------------------#
fit_sys11 <- lme(sys ~ year * age, data = longformat_patients_df, random = ~year | ID)
fit_chlratio11 <- lme(chlratio ~ year * age, data = longformat_patients_df, random = ~year | ID)
fit_eGFR11 <- lme(eGFR ~ year * age, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit3_11s <- jm(fitcox, list(fit_sys11,fit_chlratio11,fit_eGFR11), time_var = "year", n_iter = 6000L, n_burnin = 800L,
                    functional_forms = Functional_Forms)
save(Jointfit3_11s, file="~/chaojie/RData/IMP3/Jointfit3_11s.RData")

#------------------------------------Jointmodel  12------------------------------------#
fit_sys12 <- lme(sys ~ year * sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio12 <- lme(chlratio ~ year * sex, data = longformat_patients_df, random = ~year | ID)
fit_eGFR12 <- lme(eGFR ~ year * sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
summary(fitcox)
Jointfit3_12s <- jm(fitcox, list(fit_sys12,fit_chlratio12,fit_eGFR12), time_var = "year", n_iter = 6000L, n_burnin = 800L,
                    functional_forms = Functional_Forms)
save(Jointfit3_12s, file="~/chaojie/RData/IMP3/Jointfit3_12s.RData")