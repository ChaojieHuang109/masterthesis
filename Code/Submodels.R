library(dplyr) 
library(nlme)
library(lme4)
library(JMbayes2)
library(survival)
library(GLMMadaptive)
library(caTools)

#Load data
longformat_patients_df <- read.csv("/ANALYSE_AREA/P_INTG01/Chaojie/data/longformat_patients_df.csv",row.names = 1) %>%
  mutate(sex=factor(sex),smoking=factor(smoking),cvd=factor(cvd),death=factor(death),status=factor(status)) %>%
  mutate(status2=ifelse(status=="cvd",1,0)) %>%
  mutate(BMI=round(weight/(height/100)^2,2))

patients_ID <- longformat_patients_df %>%
  dplyr::select(ID) %>%
  group_by(ID) %>%
  slice(1)

# Set the seed for reproducibility
set.seed(123)

# Split the data into 80% training and 20% testing
split_ratio <- 0.8
split <- sample.split(patients_ID$ID, SplitRatio = split_ratio)

# Create the training and testing datasets
train_ID <- patients_ID[split, ]
test_ID <- patients_ID[!split, ]

longformat_patients_train <- longformat_patients_df %>%
  filter(ID %in% train_ID$ID)

longformat_patients_test <- longformat_patients_df %>%
  filter(ID %in% test_ID$ID)

#write.csv(longformat_patients_train,"/ANALYSE_AREA/P_INTG01/Chaojie/code_new/longformat_patients_train.csv")
#write.csv(longformat_patients_test,"/ANALYSE_AREA/P_INTG01/Chaojie/code_new/longformat_patients_test.csv")

survival_df <- longformat_patients_train %>% 
  filter(year==0)

#-------------------------------------------------------Survival Analysis-----------------------------------------------#
fitcox_1 <- coxph(Surv(years, status2)~sex+age+smoking+sys+dia+eGFR+chlratio+BMI, data=survival_df)
summary(fitcox_1)

fitcox_2 <- coxph(Surv(years, status2)~sex+age+smoking+sys+dia+eGFR+chl+BMI, data=survival_df)
summary(fitcox_2)

#-------------------------------------------------------Longitudinal Analysis-----------------------------------------------#
# systolic blood pressure
fitlme_sys1 <- lme(sys~year+age+sex+year:age+year:sex+age:sex,data=longformat_patients_train, random=~year|ID)
summary(fitlme_sys1)
fitlme_sys2 <- lme(sys~year+age+sex+year:age+year:sex,data=longformat_patients_train, random=~year|ID) 
summary(fitlme_sys2)
fitlme_sys3 <- lme(sys~year+age+sex+year:age+age:sex,data=longformat_patients_train, random=~year|ID) # the smallest AIC
summary(fitlme_sys3) 
fitlme_sys4 <- lme(sys~year+age+sex+year:sex+age:sex,data=longformat_patients_train, random=~year|ID)
summary(fitlme_sys4)
fitlme_sys5 <- lme(sys~year+age+sex+year:age,data=longformat_patients_train, random=~year|ID)
summary(fitlme_sys5) 
fitlme_sys6 <- lme(sys~year+age+sex+year:sex,data=longformat_patients_train, random=~year|ID)
summary(fitlme_sys6) 
fitlme_sys7 <- lme(sys~year+age+sex+age:sex,data=longformat_patients_train, random=~year|ID)
summary(fitlme_sys7)
fitlme_sys8 <- lme(sys~year+age,data=longformat_patients_train, random=~year|ID)
summary(fitlme_sys8)
fitlme_sys9 <- lme(sys~year+sex,data=longformat_patients_train, random=~year|ID)
summary(fitlme_sys9)
fitlme_sys10 <- lme(sys~age+sex,data=longformat_patients_train, random=~year|ID)
summary(fitlme_sys10)
fitlme_sys11 <- lme(sys~year*age,data=longformat_patients_train, random=~year|ID)
summary(fitlme_sys11)
fitlme_sys12 <- lme(sys~year*sex,data=longformat_patients_train, random=~year|ID)
summary(fitlme_sys12)

# List of model names
model_names <- paste0("fitlme_sys", 1:12)
# Calculate AIC values for each model
aic_values <- lapply(model_names, function(model_name) AIC(get(model_name)))
results <- data.frame(Model = model_names, AIC = unlist(aic_values))
# Rank AIC values in descending order
ranked_models <- results[order(results$AIC, decreasing = FALSE), ]
# Print the ranked AIC values
print(ranked_models)
#year+age+sex+year:age+age:sex   year+age+sex+year:age+year:sex+age:sex   year+age+sex+age:sex year+age+sex+year:sex+age:sex

# chl/hdl ratio
fitlme_chlratio1 <- lme(chlratio~year+age+sex+year:age+year:sex+age:sex,data=longformat_patients_train, random=~year|ID)
summary(fitlme_chlratio1)
fitlme_chlratio2 <- lme(chlratio~year+age+sex+year:age+year:sex,data=longformat_patients_train, random=~year|ID)
summary(fitlme_chlratio2)
fitlme_chlratio3 <- lme(chlratio~year+age+sex+year:age+age:sex,data=longformat_patients_train, random=~year|ID)  # the smallest AIC
summary(fitlme_chlratio3)
fitlme_chlratio4 <- lme(chlratio~year+age+sex+year:sex+age:sex,data=longformat_patients_train, random=~year|ID)
summary(fitlme_chlratio4)
fitlme_chlratio5 <- lme(chlratio~year+age+sex+year:age,data=longformat_patients_train, random=~year|ID)
summary(fitlme_chlratio5)
fitlme_chlratio6 <- lme(chlratio~year+age+sex+year:sex,data=longformat_patients_train, random=~year|ID)
summary(fitlme_chlratio6)
fitlme_chlratio7 <- lme(chlratio~year+age+sex+age:sex,data=longformat_patients_train, random=~year|ID)
summary(fitlme_chlratio7)
fitlme_chlratio8 <- lme(chlratio~year+age,data=longformat_patients_train, random=~year|ID)
summary(fitlme_chlratio8)
fitlme_chlratio9 <- lme(chlratio~year+sex,data=longformat_patients_train, random=~year|ID)
summary(fitlme_chlratio9)
fitlme_chlratio10 <- lme(chlratio~age+sex,data=longformat_patients_train, random=~year|ID)
summary(fitlme_chlratio10)
fitlme_chlratio11 <- lme(chlratio~year*age,data=longformat_patients_train, random=~year|ID)
summary(fitlme_chlratio11)
fitlme_chlratio12 <- lme(chlratio~year*sex,data=longformat_patients_train, random=~year|ID)
summary(fitlme_chlratio12)

# List of model names
model_names <- paste0("fitlme_chlratio", 1:12)
# Calculate AIC values for each model
aic_values <- lapply(model_names, function(model_name) AIC(get(model_name)))
results <- data.frame(Model = model_names, AIC = unlist(aic_values))
# Rank AIC values in descending order
ranked_models <- results[order(results$AIC, decreasing = FALSE), ]
# Print the ranked AIC values
print(ranked_models)

#year+age+sex+year:age+age:sex   year+age+sex+year:age+year:sex+age:sex   year+age+sex+age:sex year+age+sex+year:sex+age:sex
# hdl
fitlme_hdl1 <- lme(hdl ~ year + age + sex + year:age + year:sex + age:sex, data = longformat_patients_train, random = ~year | ID)
summary(fitlme_hdl1)
fitlme_hdl2 <- lme(hdl ~ year + age + sex + year:age + year:sex, data = longformat_patients_train, random = ~year | ID)
summary(fitlme_hdl2)
fitlme_hdl3 <- lme(hdl ~ year + age + sex + year:age + age:sex, data = longformat_patients_train, random = ~year | ID)  # the smallest AIC
summary(fitlme_hdl3)
fitlme_hdl4 <- lme(hdl ~ year + age + sex + year:sex + age:sex, data = longformat_patients_train, random = ~year | ID)
summary(fitlme_hdl4)
fitlme_hdl5 <- lme(hdl ~ year + age + sex + year:age, data = longformat_patients_train, random = ~year | ID)
summary(fitlme_hdl5)
fitlme_hdl6 <- lme(hdl ~ year + age + sex + year:sex, data = longformat_patients_train, random = ~year | ID)
summary(fitlme_hdl6)
fitlme_hdl7 <- lme(hdl ~ year + age + sex + age:sex, data = longformat_patients_train, random = ~year | ID)
summary(fitlme_hdl7)
fitlme_hdl8 <- lme(hdl ~ year + age, data = longformat_patients_train, random = ~year | ID)
summary(fitlme_hdl8)
fitlme_hdl9 <- lme(hdl ~ year + sex, data = longformat_patients_train, random = ~year | ID)
summary(fitlme_hdl9)
fitlme_hdl10 <- lme(hdl ~ age + sex, data = longformat_patients_train, random = ~year | ID)
summary(fitlme_hdl10)
fitlme_hdl11 <- lme(hdl ~ year * age, data = longformat_patients_train, random = ~year | ID)
summary(fitlme_hdl11)
fitlme_hdl12 <- lme(hdl ~ year * sex, data = longformat_patients_train, random = ~year | ID)
summary(fitlme_hdl12)

# List of model names
model_names <- paste0("fitlme_hdl", 1:12)
# Calculate AIC values for each model
aic_values <- lapply(model_names, function(model_name) AIC(get(model_name)))
results <- data.frame(Model = model_names, AIC = unlist(aic_values))
# Rank AIC values in descending order
ranked_models <- results[order(results$AIC, decreasing = FALSE), ]
# Print the ranked AIC values
print(ranked_models)







