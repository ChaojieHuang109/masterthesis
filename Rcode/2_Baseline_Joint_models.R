# improve the memory of the computer
memory.limit() 
memory.limit(35000)

library(dplyr) 
library(nlme)
library(lme4)
library(JMbayes2)
library(survival)
library(GLMMadaptive)

# These are baseline joint models based SCORE algorithm, under the current value parameterization
#----------------------------------------------------------------Imputation 1----------------------------------------------------------#
longformat_patients_df <- read.csv("~/chaojie/Data/longformat_patients_df_1.csv")%>%
  mutate(sex=factor(sex),smoking=factor(smoking),cvd=factor(cvd),death=factor(death),status=factor(status))

survival_df <- longformat_patients_df %>%
  filter(year==0) 

fit_sys1 <- lme(sys ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio1 <- lme(chlratio ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit_SCORE_1 <- jm(fitcox, list(fit_sys1,fit_chlratio1), time_var = "year", n_iter = 5000L, n_burnin = 700L)

#----------------------------------------------------------------Imputation 2----------------------------------------------------------#
longformat_patients_df <- read.csv("~/chaojie/Data/longformat_patients_df_2.csv")%>%
  mutate(sex=factor(sex),smoking=factor(smoking),cvd=factor(cvd),death=factor(death),status=factor(status))

survival_df <- longformat_patients_df %>%
  filter(year==0) 
fit_sys1 <- lme(sys ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio1 <- lme(chlratio ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit_SCORE_2 <- jm(fitcox, list(fit_sys1,fit_chlratio1), time_var = "year", n_iter = 5000L, n_burnin = 700L)

#----------------------------------------------------------------Imputation 3----------------------------------------------------------#
longformat_patients_df <- read.csv("~/chaojie/Data/longformat_patients_df_3.csv")%>%
  mutate(sex=factor(sex),smoking=factor(smoking),cvd=factor(cvd),death=factor(death),status=factor(status))

survival_df <- longformat_patients_df %>%
  filter(year==0) 
fit_sys1 <- lme(sys ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio1 <- lme(chlratio ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit_SCORE_3 <- jm(fitcox, list(fit_sys1,fit_chlratio1), time_var = "year", n_iter = 5000L, n_burnin = 700L)








# These are baseline joint models based on SCORE algorithm, under the current value and current slope parameterization
#----------------------------------------------------------------Imputation 1----------------------------------------------------------#
longformat_patients_df <- read.csv("~/chaojie/Data/longformat_patients_df_1.csv")%>%
  mutate(sex=factor(sex),smoking=factor(smoking),cvd=factor(cvd),death=factor(death),status=factor(status))

survival_df <- longformat_patients_df %>%
  filter(year==0) 

Functional_Forms <- list("sys" =~ value(sys) + slope(sys),
                         "chlratio" =~ value(chlratio) + slope(chlratio))

fit_sys1 <- lme(sys ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio1 <- lme(chlratio ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit_SCORE_1 <- jm(fitcox, list(fit_sys1,fit_chlratio1), time_var = "year", n_iter = 5000L, n_burnin = 700L,
                       functional_forms = Functional_Forms)

#----------------------------------------------------------------Imputation 2----------------------------------------------------------#
longformat_patients_df <- read.csv("~/chaojie/Data/longformat_patients_df_2.csv")%>%
  mutate(sex=factor(sex),smoking=factor(smoking),cvd=factor(cvd),death=factor(death),status=factor(status))

survival_df <- longformat_patients_df %>%
  filter(year==0)

Functional_Forms <- list("sys" =~ value(sys) + slope(sys),
                         "chlratio" =~ value(chlratio) + slope(chlratio))

fit_sys1 <- lme(sys ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio1 <- lme(chlratio ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit_SCORE_2 <- jm(fitcox, list(fit_sys1,fit_chlratio1), time_var = "year", n_iter = 5000L, n_burnin = 700L,
                       functional_forms = Functional_Forms)

#----------------------------------------------------------------Imputation 3----------------------------------------------------------#
longformat_patients_df <- read.csv("~/chaojie/Data/longformat_patients_df_3.csv")%>%
  mutate(sex=factor(sex),smoking=factor(smoking),cvd=factor(cvd),death=factor(death),status=factor(status))

survival_df <- longformat_patients_df %>%
  filter(year==0) 
fit_sys1 <- lme(sys ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fit_chlratio1 <- lme(chlratio ~ year + age + sex, data = longformat_patients_df, random = ~year | ID)
fitcox <- coxph(Surv(years, status2)~sex+age+smoking, data=survival_df)
Jointfit_SCORE_3 <- jm(fitcox, list(fit_sys1,fit_chlratio1), time_var = "year", n_iter = 5000L, n_burnin = 700L,
                       functional_forms = Functional_Forms)

