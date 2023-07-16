library(mice)
library(dplyr)

patients_df_2011 <- read.csv("patients_df_2011.csv")
patients_df_2012a <- read.csv("patients_df_2012a.csv")
patients_df_2012b <- read.csv("patients_df_2012b.csv")
patients_df_2012c <- read.csv("patients_df_2012c.csv")
patients_df_2013a <- read.csv("patients_df_2013a.csv")
patients_df_2013b <- read.csv("patients_df_2013b.csv")
patients_df_2013c <- read.csv("patients_df_2013c.csv")
patients_df_2014a <- read.csv("patients_df_2014a.csv")
patients_df_2014b <- read.csv("patients_df_2014b.csv")
patients_df_2014c <- read.csv("patients_df_2014c.csv")
patients_df_2015a <- read.csv("patients_df_2015a.csv")
patients_df_2015b <- read.csv("patients_df_2015b.csv")
patients_df_2015c <- read.csv("patients_df_2015c.csv")
smoking_df <- read.csv("smoking_df.csv") %>%
  mutate(smoking=factor(smoking))

#Data Imputation
patients_df_2011_mice <- mice(patients_df_2011,m=5,method="pmm",maxit=50,seed=123)
patients_df_2012a_mice <- mice(patients_df_2012a, m = 5, method = "pmm", maxit = 50, seed = 123)
patients_df_2012b_mice <- mice(patients_df_2012b, m = 5, method = "pmm", maxit = 50, seed = 123)
patients_df_2012c_mice <- mice(patients_df_2012c, m = 5, method = "pmm", maxit = 50, seed = 123)
patients_df_2013a_mice <- mice(patients_df_2013a, m = 5, method = "pmm", maxit = 50, seed = 123)
patients_df_2013b_mice <- mice(patients_df_2013b, m = 5, method = "pmm", maxit = 50, seed = 123)
patients_df_2013c_mice <- mice(patients_df_2013c, m = 5, method = "pmm", maxit = 50, seed = 123)
patients_df_2014a_mice <- mice(patients_df_2014a, m = 5, method = "pmm", maxit = 50, seed = 123)
patients_df_2014b_mice <- mice(patients_df_2014b, m = 5, method = "pmm", maxit = 50, seed = 123)
patients_df_2014c_mice <- mice(patients_df_2014c, m = 5, method = "pmm", maxit = 50, seed = 123)
patients_df_2015a_mice <- mice(patients_df_2015a, m = 5, method = "pmm", maxit = 50, seed = 123)
patients_df_2015b_mice <- mice(patients_df_2015b, m = 5, method = "pmm", maxit = 50, seed = 123)
patients_df_2015c_mice <- mice(patients_df_2015c, m = 5, method = "pmm", maxit = 50, seed = 123)
smoking_df_mice <- mice(smoking_df, m = 5, method = "logreg", maxit = 50, seed = 123)

save(patients_df_2011_mice,patients_df_2012a_mice,patients_df_2012b_mice,patients_df_2012c_mice,
     patients_df_2013a_mice,patients_df_2013b_mice,patients_df_2013c_mice,
     patients_df_2014a_mice,patients_df_2014b_mice,patients_df_2014c_mice,
     patients_df_2015a_mice,patients_df_2015b_mice,patients_df_2015c_mice,smoking_df_mice, file="imputation.RData")

complete_data <- function(numbers) {
  for (number in numbers) {
    
    complete_var <- paste0("patients_df_2011_imp_", number)
    assign(complete_var, mice::complete(patients_df_2011_mice, action = number), envir = .GlobalEnv)
    
    for (year in c(2012, 2013, 2014, 2015)) {
      complete_var <- paste0("patients_df_", year, "a_imp_", number)
      assign(complete_var, mice::complete(get(paste0("patients_df_", year, "a_mice")), action = number), envir = .GlobalEnv)
      
      complete_var <- paste0("patients_df_", year, "b_imp_", number)
      assign(complete_var, mice::complete(get(paste0("patients_df_", year, "b_mice")), action = number), envir = .GlobalEnv)
      
      complete_var <- paste0("patients_df_", year, "c_imp_", number)
      assign(complete_var, mice::complete(get(paste0("patients_df_", year, "c_mice")), action = number), envir = .GlobalEnv)
    }
    
    complete_var <- paste0("smoking_df_imp_", number)
    assign(complete_var, mice::complete(smoking_df_mice, action = number), envir = .GlobalEnv)
  }
}

complete_data(numbers = c(1,2,3,4,5))

# Imputation 1
patients_selected_df_1 <- patients_df_2011_imp_1 %>%
  select(1:13)

patients_imputed_df_2011_1 <- merge(x=patients_df_2011_imp_1,y=smoking_df_imp_1, by= "ID", all.x = TRUE)  %>%
  select(ID,years,starts_with("status"),starts_with("cvd"),starts_with("death"),sex,age,height,smoking,ends_with("2011")) %>%
  rename_with(~ gsub("2011$", "", .), ends_with("2011")) %>%
  mutate(year=0)

patients_imputed_df_2012a_1 <- merge(x = patients_selected_df_1, y = smoking_df_imp_1, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2012a_imp_1, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2012a$", "", .), ends_with("2012a")) %>% 
  mutate(year=0.167)

patients_imputed_df_2012b_1 <- merge(x = patients_selected_df_1, y = smoking_df_imp_1, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2012b_imp_1, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2012b$", "", .), ends_with("2012b")) %>%
  mutate(year=0.5)

patients_imputed_df_2012c_1 <- merge(x = patients_selected_df_1, y = smoking_df_imp_1, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2012c_imp_1, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2012c$", "", .), ends_with("2012c")) %>%
  mutate(year=0.833)

patients_imputed_df_2013a_1 <- merge(x = patients_selected_df_1, y = smoking_df_imp_1, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2013a_imp_1, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2013a$", "", .), ends_with("2013a")) %>%
  mutate(year=1.167)

patients_imputed_df_2013b_1 <- merge(x = patients_selected_df_1, y = smoking_df_imp_1, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2013b_imp_1, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2013b$", "", .), ends_with("2013b")) %>%
  mutate(year=1.5)

patients_imputed_df_2013c_1 <- merge(x = patients_selected_df_1, y = smoking_df_imp_1, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2013c_imp_1, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2013c$", "", .), ends_with("2013c")) %>%
  mutate(year=1.833)

patients_imputed_df_2014a_1 <- merge(x = patients_selected_df_1, y = smoking_df_imp_1, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2014a_imp_1, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2014a$", "", .), ends_with("2014a")) %>%
  mutate(year=2.167)

patients_imputed_df_2014b_1 <- merge(x = patients_selected_df_1, y = smoking_df_imp_1, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2014b_imp_1, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2014b$", "", .), ends_with("2014b")) %>%
  mutate(year=2.5)

patients_imputed_df_2014c_1 <- merge(x = patients_selected_df_1, y = smoking_df_imp_1, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2014c_imp_1, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2014c$", "", .), ends_with("2014c")) %>%
  mutate(year=2.833)

patients_imputed_df_2015a_1 <- merge(x = patients_selected_df_1, y = smoking_df_imp_1, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2015a_imp_1, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2015a$", "", .), ends_with("2015a")) %>%
  mutate(year=3.167)

patients_imputed_df_2015b_1 <- merge(x = patients_selected_df_1, y = smoking_df_imp_1, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2015b_imp_1, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2015b$", "", .), ends_with("2015b")) %>%
  mutate(year=3.5)

patients_imputed_df_2015c_1 <- merge(x = patients_selected_df_1, y = smoking_df_imp_1, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2015c_imp_1, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2015c$", "", .), ends_with("2015c")) %>%
  mutate(year=3.833)

longformat_patients_df_1 <- rbind(patients_imputed_df_2011_1,patients_imputed_df_2012a_1,patients_imputed_df_2012b_1, patients_imputed_df_2012c_1, patients_imputed_df_2013a_1, 
                                  patients_imputed_df_2013b_1, patients_imputed_df_2013c_1, patients_imputed_df_2014a_1, patients_imputed_df_2014b_1, patients_imputed_df_2014c_1, 
                                  patients_imputed_df_2015a_1, patients_imputed_df_2015b_1, patients_imputed_df_2015c_1) %>%
  # Calculate chlratio(chl/hdl) 
  mutate(chlratio=chl/hdl) %>%
  # Calculate eGFR according to The CKD-EPI Equation
  mutate(eGFR=case_when(
    sex=="M" & cre<=0.7 ~ 144*(cre/0.7)^(-0.329)*(0.993)^age,
    sex=="M" & cre>0.7 ~ 144*(cre/0.7)^(-1.209)*(0.993)^age,
    sex=="F" & cre<=0.9 ~ 141*(cre/0.9)^(-0.411)*(0.993)^age,
    sex=="F" & cre>0.9 ~ 141*(cre/0.9)^(-1.209)*(0.993)^age,
    TRUE ~ NA_real_)) %>%
  mutate(status2=ifelse(status=="cvd",1,0)) %>%
  mutate(BMI=weight/(height/100)^2) %>%
  select(1:4,34,5:30,32:33,35,31) %>%
  # Round some numerical variable
  mutate_at(vars(14, 16:34), ~ round(., digits = 2)) %>%
  # Remove data record after some patients died or patients who were diagnosed with cvd
  filter(years >= year) %>%
  arrange(ID, year)


#Imputation 2
patients_selected_df_2 <- patients_df_2011_imp_2 %>%
  select(1:13)

patients_imputed_df_2011_2 <- merge(x=patients_df_2011_imp_2,y=smoking_df_imp_2, by= "ID", all.x = TRUE)  %>%
  select(ID,years,starts_with("status"),starts_with("cvd"),starts_with("death"),sex,age,height,smoking,ends_with("2011")) %>%
  rename_with(~ gsub("2011$", "", .), ends_with("2011")) %>%
  mutate(year=0)

patients_imputed_df_2012a_2 <- merge(x = patients_selected_df_2, y = smoking_df_imp_2, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2012a_imp_2, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2012a$", "", .), ends_with("2012a")) %>% 
  mutate(year=0.167)

patients_imputed_df_2012b_2 <- merge(x = patients_selected_df_2, y = smoking_df_imp_2, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2012b_imp_2, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2012b$", "", .), ends_with("2012b")) %>%
  mutate(year=0.5)

patients_imputed_df_2012c_2 <- merge(x = patients_selected_df_2, y = smoking_df_imp_2, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2012c_imp_2, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2012c$", "", .), ends_with("2012c")) %>%
  mutate(year=0.833)

patients_imputed_df_2013a_2 <- merge(x = patients_selected_df_2, y = smoking_df_imp_2, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2013a_imp_2, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2013a$", "", .), ends_with("2013a")) %>%
  mutate(year=1.167)

patients_imputed_df_2013b_2 <- merge(x = patients_selected_df_2, y = smoking_df_imp_2, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2013b_imp_2, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2013b$", "", .), ends_with("2013b")) %>%
  mutate(year=1.5)

patients_imputed_df_2013c_2 <- merge(x = patients_selected_df_2, y = smoking_df_imp_2, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2013c_imp_2, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2013c$", "", .), ends_with("2013c")) %>%
  mutate(year=1.833)

patients_imputed_df_2014a_2 <- merge(x = patients_selected_df_2, y = smoking_df_imp_2, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2014a_imp_2, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2014a$", "", .), ends_with("2014a")) %>%
  mutate(year=2.167)

patients_imputed_df_2014b_2 <- merge(x = patients_selected_df_2, y = smoking_df_imp_2, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2014b_imp_2, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2014b$", "", .), ends_with("2014b")) %>%
  mutate(year=2.5)

patients_imputed_df_2014c_2 <- merge(x = patients_selected_df_2, y = smoking_df_imp_2, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2014c_imp_2, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2014c$", "", .), ends_with("2014c")) %>%
  mutate(year=2.833)

patients_imputed_df_2015a_2 <- merge(x = patients_selected_df_2, y = smoking_df_imp_2, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2015a_imp_2, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2015a$", "", .), ends_with("2015a")) %>%
  mutate(year=3.167)

patients_imputed_df_2015b_2 <- merge(x = patients_selected_df_2, y = smoking_df_imp_2, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2015b_imp_2, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2015b$", "", .), ends_with("2015b")) %>%
  mutate(year=3.5)

patients_imputed_df_2015c_2 <- merge(x = patients_selected_df_2, y = smoking_df_imp_2, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2015c_imp_2, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2015c$", "", .), ends_with("2015c")) %>%
  mutate(year=3.833)

longformat_patients_df_2 <- rbind(patients_imputed_df_2011_2,patients_imputed_df_2012a_2,patients_imputed_df_2012b_2, patients_imputed_df_2012c_2, patients_imputed_df_2013a_2, 
                                  patients_imputed_df_2013b_2, patients_imputed_df_2013c_2, patients_imputed_df_2014a_2, patients_imputed_df_2014b_2, patients_imputed_df_2014c_2, 
                                  patients_imputed_df_2015a_2, patients_imputed_df_2015b_2, patients_imputed_df_2015c_2) %>%
  # Calculate chlratio(chl/hdl) 
  mutate(chlratio=chl/hdl) %>%
  # Calculate eGFR according to The CKD-EPI Equation
  mutate(eGFR=case_when(
    sex=="M" & cre<=0.7 ~ 144*(cre/0.7)^(-0.329)*(0.993)^age,
    sex=="M" & cre>0.7 ~ 144*(cre/0.7)^(-1.209)*(0.993)^age,
    sex=="F" & cre<=0.9 ~ 141*(cre/0.9)^(-0.411)*(0.993)^age,
    sex=="F" & cre>0.9 ~ 141*(cre/0.9)^(-1.209)*(0.993)^age,
    TRUE ~ NA_real_)) %>%
  mutate(status2=ifelse(status=="cvd",1,0)) %>%
  mutate(BMI=weight/(height/100)^2) %>%
  select(1:4,34,5:30,32:33,35,31) %>%
  # Round some numerical variable
  mutate_at(vars(14, 16:34), ~ round(., digits = 2)) %>%
  # Remove data record after some patients died or patients who were diagnosed with cvd
  filter(years >= year) %>%
  arrange(ID, year)

# Imputation 3
patients_selected_df_3 <- patients_df_2011_imp_3 %>%
  select(1:13)

patients_imputed_df_2011_3 <- merge(x=patients_df_2011_imp_3,y=smoking_df_imp_3, by= "ID", all.x = TRUE)  %>%
  select(ID,years,starts_with("status"),starts_with("cvd"),starts_with("death"),sex,age,height,smoking,ends_with("2011")) %>%
  rename_with(~ gsub("2011$", "", .), ends_with("2011")) %>%
  mutate(year=0)

patients_imputed_df_2012a_3 <- merge(x = patients_selected_df_3, y = smoking_df_imp_3, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2012a_imp_3, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2012a$", "", .), ends_with("2012a")) %>% 
  mutate(year=0.167)

patients_imputed_df_2012b_3 <- merge(x = patients_selected_df_3, y = smoking_df_imp_3, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2012b_imp_3, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2012b$", "", .), ends_with("2012b")) %>%
  mutate(year=0.5)

patients_imputed_df_2012c_3 <- merge(x = patients_selected_df_3, y = smoking_df_imp_3, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2012c_imp_3, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2012c$", "", .), ends_with("2012c")) %>%
  mutate(year=0.833)

patients_imputed_df_2013a_3 <- merge(x = patients_selected_df_3, y = smoking_df_imp_3, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2013a_imp_3, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2013a$", "", .), ends_with("2013a")) %>%
  mutate(year=1.167)

patients_imputed_df_2013b_3 <- merge(x = patients_selected_df_3, y = smoking_df_imp_3, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2013b_imp_3, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2013b$", "", .), ends_with("2013b")) %>%
  mutate(year=1.5)

patients_imputed_df_2013c_3 <- merge(x = patients_selected_df_3, y = smoking_df_imp_3, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2013c_imp_3, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2013c$", "", .), ends_with("2013c")) %>%
  mutate(year=1.833)

patients_imputed_df_2014a_3 <- merge(x = patients_selected_df_3, y = smoking_df_imp_3, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2014a_imp_3, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2014a$", "", .), ends_with("2014a")) %>%
  mutate(year=2.167)

patients_imputed_df_2014b_3 <- merge(x = patients_selected_df_3, y = smoking_df_imp_3, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2014b_imp_3, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2014b$", "", .), ends_with("2014b")) %>%
  mutate(year=2.5)

patients_imputed_df_2014c_3 <- merge(x = patients_selected_df_3, y = smoking_df_imp_3, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2014c_imp_3, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2014c$", "", .), ends_with("2014c")) %>%
  mutate(year=2.833)

patients_imputed_df_2015a_3 <- merge(x = patients_selected_df_3, y = smoking_df_imp_3, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2015a_imp_3, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2015a$", "", .), ends_with("2015a")) %>%
  mutate(year=3.167)

patients_imputed_df_2015b_3 <- merge(x = patients_selected_df_3, y = smoking_df_imp_3, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2015b_imp_3, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2015b$", "", .), ends_with("2015b")) %>%
  mutate(year=3.5)

patients_imputed_df_2015c_3 <- merge(x = patients_selected_df_3, y = smoking_df_imp_3, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2015c_imp_3, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2015c$", "", .), ends_with("2015c")) %>%
  mutate(year=3.833)

longformat_patients_df_3 <- rbind(patients_imputed_df_2011_3,patients_imputed_df_2012a_3,patients_imputed_df_2012b_3, patients_imputed_df_2012c_3, patients_imputed_df_2013a_3, 
                                  patients_imputed_df_2013b_3, patients_imputed_df_2013c_3, patients_imputed_df_2014a_3, patients_imputed_df_2014b_3, patients_imputed_df_2014c_3, 
                                  patients_imputed_df_2015a_3, patients_imputed_df_2015b_3, patients_imputed_df_2015c_3) %>%
  # Calculate chlratio(chl/hdl) 
  mutate(chlratio=chl/hdl) %>%
  # Calculate eGFR according to The CKD-EPI Equation
  mutate(eGFR=case_when(
    sex=="M" & cre<=0.7 ~ 144*(cre/0.7)^(-0.329)*(0.993)^age,
    sex=="M" & cre>0.7 ~ 144*(cre/0.7)^(-1.209)*(0.993)^age,
    sex=="F" & cre<=0.9 ~ 141*(cre/0.9)^(-0.411)*(0.993)^age,
    sex=="F" & cre>0.9 ~ 141*(cre/0.9)^(-1.209)*(0.993)^age,
    TRUE ~ NA_real_)) %>%
  mutate(status2=ifelse(status=="cvd",1,0)) %>%
  mutate(BMI=weight/(height/100)^2) %>%
  select(1:4,34,5:30,32:33,35,31) %>%
  # Round some numerical variable
  mutate_at(vars(14, 16:34), ~ round(., digits = 2)) %>%
  # Remove data record after some patients died or patients who were diagnosed with cvd
  filter(years >= year) %>%
  arrange(ID, year)

# Imputation 4
patients_selected_df_4 <- patients_df_2011_imp_4 %>%
  select(1:13)

patients_imputed_df_2011_4 <- merge(x=patients_df_2011_imp_4,y=smoking_df_imp_4, by= "ID", all.x = TRUE)  %>%
  select(ID,years,starts_with("status"),starts_with("cvd"),starts_with("death"),sex,age,height,smoking,ends_with("2011")) %>%
  rename_with(~ gsub("2011$", "", .), ends_with("2011")) %>%
  mutate(year=0)

patients_imputed_df_2012a_4 <- merge(x = patients_selected_df_4, y = smoking_df_imp_4, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2012a_imp_4, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2012a$", "", .), ends_with("2012a")) %>% 
  mutate(year=0.167)

patients_imputed_df_2012b_4 <- merge(x = patients_selected_df_4, y = smoking_df_imp_4, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2012b_imp_4, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2012b$", "", .), ends_with("2012b")) %>%
  mutate(year=0.5)

patients_imputed_df_2012c_4 <- merge(x = patients_selected_df_4, y = smoking_df_imp_4, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2012c_imp_4, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2012c$", "", .), ends_with("2012c")) %>%
  mutate(year=0.833)

patients_imputed_df_2013a_4 <- merge(x = patients_selected_df_4, y = smoking_df_imp_4, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2013a_imp_4, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2013a$", "", .), ends_with("2013a")) %>%
  mutate(year=1.167)

patients_imputed_df_2013b_4 <- merge(x = patients_selected_df_4, y = smoking_df_imp_4, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2013b_imp_4, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2013b$", "", .), ends_with("2013b")) %>%
  mutate(year=1.5)

patients_imputed_df_2013c_4 <- merge(x = patients_selected_df_4, y = smoking_df_imp_4, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2013c_imp_4, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2013c$", "", .), ends_with("2013c")) %>%
  mutate(year=1.833)

patients_imputed_df_2014a_4 <- merge(x = patients_selected_df_4, y = smoking_df_imp_4, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2014a_imp_4, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2014a$", "", .), ends_with("2014a")) %>%
  mutate(year=2.167)

patients_imputed_df_2014b_4 <- merge(x = patients_selected_df_4, y = smoking_df_imp_4, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2014b_imp_4, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2014b$", "", .), ends_with("2014b")) %>%
  mutate(year=2.5)

patients_imputed_df_2014c_4 <- merge(x = patients_selected_df_4, y = smoking_df_imp_4, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2014c_imp_4, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2014c$", "", .), ends_with("2014c")) %>%
  mutate(year=2.833)

patients_imputed_df_2015a_4 <- merge(x = patients_selected_df_4, y = smoking_df_imp_4, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2015a_imp_4, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2015a$", "", .), ends_with("2015a")) %>%
  mutate(year=3.167)

patients_imputed_df_2015b_4 <- merge(x = patients_selected_df_4, y = smoking_df_imp_4, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2015b_imp_4, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2015b$", "", .), ends_with("2015b")) %>%
  mutate(year=3.5)

patients_imputed_df_2015c_4 <- merge(x = patients_selected_df_4, y = smoking_df_imp_4, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2015c_imp_4, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2015c$", "", .), ends_with("2015c")) %>%
  mutate(year=3.833)

longformat_patients_df_4 <- rbind(patients_imputed_df_2011_4,patients_imputed_df_2012a_4,patients_imputed_df_2012b_4, patients_imputed_df_2012c_4, patients_imputed_df_2013a_4, 
                                  patients_imputed_df_2013b_4, patients_imputed_df_2013c_4, patients_imputed_df_2014a_4, patients_imputed_df_2014b_4, patients_imputed_df_2014c_4, 
                                  patients_imputed_df_2015a_4, patients_imputed_df_2015b_4, patients_imputed_df_2015c_4) %>%
  # Calculate chlratio(chl/hdl) 
  mutate(chlratio=chl/hdl) %>%
  # Calculate eGFR according to The CKD-EPI Equation
  mutate(eGFR=case_when(
    sex=="M" & cre<=0.7 ~ 144*(cre/0.7)^(-0.329)*(0.993)^age,
    sex=="M" & cre>0.7 ~ 144*(cre/0.7)^(-1.209)*(0.993)^age,
    sex=="F" & cre<=0.9 ~ 141*(cre/0.9)^(-0.411)*(0.993)^age,
    sex=="F" & cre>0.9 ~ 141*(cre/0.9)^(-1.209)*(0.993)^age,
    TRUE ~ NA_real_)) %>%
  mutate(status2=ifelse(status=="cvd",1,0)) %>%
  mutate(BMI=weight/(height/100)^2) %>%
  select(1:4,34,5:30,32:33,35,31) %>%
  # Round some numerical variable
  mutate_at(vars(14, 16:34), ~ round(., digits = 2)) %>%
  # Remove data record after some patients died or patients who were diagnosed with cvd
  filter(years >= year) %>%
  arrange(ID, year)

# Imputation 5
patients_selected_df_5 <- patients_df_2011_imp_5 %>%
  select(1:13)

patients_imputed_df_2011_5 <- merge(x=patients_df_2011_imp_5,y=smoking_df_imp_5, by= "ID", all.x = TRUE)  %>%
  select(ID,years,starts_with("status"),starts_with("cvd"),starts_with("death"),sex,age,height,smoking,ends_with("2011")) %>%
  rename_with(~ gsub("2011$", "", .), ends_with("2011")) %>%
  mutate(year=0)

patients_imputed_df_2012a_5 <- merge(x = patients_selected_df_5, y = smoking_df_imp_5, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2012a_imp_5, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2012a$", "", .), ends_with("2012a")) %>% 
  mutate(year=0.167)

patients_imputed_df_2012b_5 <- merge(x = patients_selected_df_5, y = smoking_df_imp_5, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2012b_imp_5, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2012b$", "", .), ends_with("2012b")) %>%
  mutate(year=0.5)

patients_imputed_df_2012c_5 <- merge(x = patients_selected_df_5, y = smoking_df_imp_5, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2012c_imp_5, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2012c$", "", .), ends_with("2012c")) %>%
  mutate(year=0.833)

patients_imputed_df_2013a_5 <- merge(x = patients_selected_df_5, y = smoking_df_imp_5, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2013a_imp_5, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2013a$", "", .), ends_with("2013a")) %>%
  mutate(year=1.167)

patients_imputed_df_2013b_5 <- merge(x = patients_selected_df_5, y = smoking_df_imp_5, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2013b_imp_5, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2013b$", "", .), ends_with("2013b")) %>%
  mutate(year=1.5)

patients_imputed_df_2013c_5 <- merge(x = patients_selected_df_5, y = smoking_df_imp_5, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2013c_imp_3, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2013c$", "", .), ends_with("2013c")) %>%
  mutate(year=1.833)

patients_imputed_df_2014a_5 <- merge(x = patients_selected_df_5, y = smoking_df_imp_5, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2014a_imp_5, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2014a$", "", .), ends_with("2014a")) %>%
  mutate(year=2.167)

patients_imputed_df_2014b_5 <- merge(x = patients_selected_df_5, y = smoking_df_imp_5, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2014b_imp_5, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2014b$", "", .), ends_with("2014b")) %>%
  mutate(year=2.5)

patients_imputed_df_2014c_5 <- merge(x = patients_selected_df_5, y = smoking_df_imp_5, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2014c_imp_5, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2014c$", "", .), ends_with("2014c")) %>%
  mutate(year=2.833)

patients_imputed_df_2015a_5 <- merge(x = patients_selected_df_5, y = smoking_df_imp_5, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2015a_imp_5, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2015a$", "", .), ends_with("2015a")) %>%
  mutate(year=3.167)

patients_imputed_df_2015b_5 <- merge(x = patients_selected_df_5, y = smoking_df_imp_5, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2015b_imp_5, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2015b$", "", .), ends_with("2015b")) %>%
  mutate(year=3.5)

patients_imputed_df_2015c_5 <- merge(x = patients_selected_df_5, y = smoking_df_imp_5, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2015c_imp_5, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2015c$", "", .), ends_with("2015c")) %>%
  mutate(year=3.833)

longformat_patients_df_5 <- rbind(patients_imputed_df_2011_5,patients_imputed_df_2012a_5,patients_imputed_df_2012b_5, patients_imputed_df_2012c_5, patients_imputed_df_2013a_5, 
                                  patients_imputed_df_2013b_5, patients_imputed_df_2013c_5, patients_imputed_df_2014a_5, patients_imputed_df_2014b_5, patients_imputed_df_2014c_5, 
                                  patients_imputed_df_2015a_5, patients_imputed_df_2015b_5, patients_imputed_df_2015c_5) %>%
  # Calculate chlratio(chl/hdl) 
  mutate(chlratio=chl/hdl) %>%
  # Calculate eGFR according to The CKD-EPI Equation
  mutate(eGFR=case_when(
    sex=="M" & cre<=0.7 ~ 144*(cre/0.7)^(-0.329)*(0.993)^age,
    sex=="M" & cre>0.7 ~ 144*(cre/0.7)^(-1.209)*(0.993)^age,
    sex=="F" & cre<=0.9 ~ 141*(cre/0.9)^(-0.411)*(0.993)^age,
    sex=="F" & cre>0.9 ~ 141*(cre/0.9)^(-1.209)*(0.993)^age,
    TRUE ~ NA_real_)) %>%
  mutate(status2=ifelse(status=="cvd",1,0)) %>%
  mutate(BMI=weight/(height/100)^2) %>%
  select(1:4,34,5:30,32:33,35,31) %>%
  # Round some numerical variable
  mutate_at(vars(14, 16:34), ~ round(., digits = 2)) %>%
  # Remove data record after some patients died or patients who were diagnosed with cvd
  filter(years >= year) %>%
  arrange(ID, year)

write.csv(longformat_patients_df_1,file="longformat_patients_df_1.csv",row.names = FALSE)
write.csv(longformat_patients_df_2,file="longformat_patients_df_2.csv",row.names = FALSE)
write.csv(longformat_patients_df_3,file="longformat_patients_df_3.csv",,row.names = FALSE)
write.csv(longformat_patients_df_4,file="longformat_patients_df_4.csv",,row.names = FALSE)
write.csv(longformat_patients_df_5,file="longformat_patients_df_5.csv",,row.names = FALSE)