# Load library
library(dplyr) 
library(haven)
library(tidyverse)
library(mice)

setwd("/ANALYSE_AREA/P_INTG01/Chaojie")

#------------------------------------------------------Data Processing------------------------------------------------------#
############Load Data###########
###Patients###
patients_file <- read.csv("/ANALYSE_AREA/P_INTG01/Chaojie/dataset/PATIENTS.csv")
patients <- patients_file %>%
  rename("ID"="CPROJECT","sex"="GESL") %>%
  # caculate the age of patients
  mutate(age=2011-GEBJR) %>%
  filter(JCG>=2011) %>%                                                              # Number:68426
  group_by(ID) %>%
  mutate(min_visit = min(JCG),max_visit = max(JCG)) %>%
  slice(1) %>%
  filter(min_visit<2012 & max_visit>2011) %>%
  select(ID,sex,age)
                                                                                     # Number:41035

###Measurements###
measurements_file <- read.csv("/ANALYSE_AREA/P_INTG01/Chaojie/dataset/MEASUREMENTS.csv") %>%
  rename("ID"="CPROJECT","test"="MEASURE_TEST","value"="MEASURE_VALUE") %>%
  filter(!test=="SMOKING STATUS") %>%
  mutate(MTWDAT=as.Date(MTWDAT, format="%d/%m/%Y")) %>%
  mutate(year=format(MTWDAT, "%Y")) %>%
  select(ID,year,test,value)

# SYSTOLIC BLOOD PRESSURE
systolic <- measurements_file %>% 
  filter(test=="SYSTOLIC BLOOD PRESSURE") %>%
  mutate(value=as.numeric(as.character(value))) %>%
  select(-test) %>%
  pivot_wider(names_from = year,names_sort = TRUE,values_from = value,values_fn=list(value=mean)) %>%
  setNames(c(names(.)[1], paste0("sys", names(.)[2:length(.)])))
# Number:47835

# DIASTOLIC BLOOD PRESSURE
diastolic <- measurements_file %>% 
  filter(test=="DIASTOLIC BLOOD PRESSURE") %>%
  mutate(value=as.numeric(as.character(value))) %>%
  select(-test) %>%
  pivot_wider(names_from = year,names_sort = TRUE,values_from = value,values_fn=list(value=mean)) %>%
  setNames(c(names(.)[1], paste0("dia", names(.)[2:length(.)])))
# Number:47835

# HEART BEAT
heartbeat <- measurements_file %>% 
  filter(test=="HEARTBEAT") %>%
  mutate(value=as.numeric(as.character(value))) %>%
  select(-test) %>%
  pivot_wider(names_from = year,names_sort = TRUE,values_from = value,values_fn=list(value=mean)) %>%
  setNames(c(names(.)[1], paste0("hbeat", names(.)[2:length(.)])))
# Number:20890

# WEIGHT
weight <- measurements_file %>% 
  filter(test=="WEIGHT") %>%
  mutate(value=as.numeric(as.character(value))) %>%
  select(-test) %>%
  pivot_wider(names_from = year,names_sort = TRUE,values_from = value,values_fn=list(value=mean)) %>%
  setNames(c(names(.)[1], paste0("weight", names(.)[2:length(.)])))
# Number:33466

# HEIGHT
height <- measurements_file %>% 
  filter(test=="HEIGHT") %>%
  mutate(value=as.numeric(as.character(value))) %>%
  select(-test) %>%
  #adjust height data and remove some height data which is unrealistic
  filter(value >= 50) %>%
  filter(year >= 2000) %>%
  pivot_wider(names_from = year,names_sort = TRUE,values_from = value,values_fn=list(value=mean)) %>%
  #Use the average height
  mutate(height=(select(., -ID) %>% rowMeans(na.rm=TRUE))) %>%
  select(ID,height)
# Number:26060

# Smoking
smoking <- read.csv("/ANALYSE_AREA/P_INTG01/Chaojie/dataset/MEASUREMENTS.csv") %>%
  data.frame(.) %>%
  rename("ID"="CPROJECT","year"="MTWDAT","test"="MEASURE_TEST","value"="MEASURE_VALUE") %>%
  filter(test=="SMOKING STATUS") %>%
  mutate(smoking=case_when(value=="non-smoker"~0,
                           value=="ex-smoker"~1,
                           value=="smoker"~1)) %>% #consider ex-smokers' physical status the same as the smokers'
  select(ID,year,smoking) %>%
  pivot_wider(names_from = year,names_sort = TRUE,values_from = smoking,values_fn = first) %>%
  setNames(c(names(.)[1], paste0("smoking", names(.)[2:length(.)]))) 
# Number:17164

###LABORATORY###
laboratory_file <- read.csv("/ANALYSE_AREA/P_INTG01/Chaojie/dataset/LABORATORY.csv") %>%
  rename("ID"="CPROJECT","test"="LAB_TEST","value"="LAB_VALUE") %>%
  mutate(LABDAT=as.character(LABDAT)) %>%
  mutate(LABDAT=as.Date(LABDAT, format = "%d%b%Y"))%>%
  mutate(year=format(LABDAT, "%Y")) %>%
  select(ID,year,test,value)

# CREATININE 肌酐
creatinine <- laboratory_file %>% 
  filter(test=="CREATININE") %>%
  select(-test) %>%
  pivot_wider(names_from = year,names_sort = TRUE,values_from = value,values_fn=list(value=mean)) %>%
  setNames(c(names(.)[1], paste0("cre", names(.)[2:length(.)])))
# Number:48046

# TRIGLYCERIDEN 甘油三酯
triglyceriden <- laboratory_file %>% 
  filter(test=="TRIGLYCERIDEN") %>%
  select(-test) %>%
  pivot_wider(names_from = year,names_sort = TRUE,values_from = value,values_fn=list(value=mean)) %>%
  setNames(c(names(.)[1], paste0("tri", names(.)[2:length(.)])))
# Number:48046

# TOTAL_CHOLESTEROL 总胆固醇
chl <- laboratory_file %>% 
  filter(test=="TOTAL_CHOLESTEROL") %>%
  select(-test) %>%
  pivot_wider(names_from = year,names_sort = TRUE,values_from = value,values_fn=list(value=mean)) %>%
  setNames(c(names(.)[1], paste0("chl", names(.)[2:length(.)])))
# Number:45970

# HDL_CHOLESTEROL （高密度脂蛋白）胆固醇
hdl <- laboratory_file %>% 
  filter(test=="HDL_CHOLESTEROL") %>%
  select(-test) %>%
  pivot_wider(names_from = year,names_sort = TRUE,values_from = value,values_fn=list(value=mean)) %>%
  setNames(c(names(.)[1], paste0("hdl", names(.)[2:length(.)])))
# Number:45666

# LDL_CHOLESTEROL （低密度脂蛋白）胆固醇
ldl <- laboratory_file %>% 
  filter(test=="LDL_CHOLESTEROL") %>%
  select(-test) %>%
  pivot_wider(names_from = year,names_sort = TRUE,values_from = value,values_fn=list(value=mean)) %>%
  setNames(c(names(.)[1], paste0("ldl", names(.)[2:length(.)])))
# Number:45666

# POTASSIUM 钾
K <- laboratory_file %>% 
  filter(test=="POTASSIUM") %>%
  select(-test) %>%
  pivot_wider(names_from = year,names_sort = TRUE,values_from = value,values_fn=list(value=mean)) %>%
  setNames(c(names(.)[1], paste0("K", names(.)[2:length(.)])))
# Number:39960

# URIC ACID 尿酸
uric_acid <- laboratory_file %>% 
  filter(test=="URIC ACID") %>%
  select(-test) %>%
  pivot_wider(names_from = year,names_sort = TRUE,values_from = value,values_fn=list(value=mean)) %>%
  setNames(c(names(.)[1], paste0("UA", names(.)[2:length(.)])))
# Number:41430

# GLUCOSE 血糖
glucose <- laboratory_file %>% 
  filter(test=="GLUCOSE") %>%
  select(-test) %>%
  pivot_wider(names_from = year,names_sort = TRUE,values_from = value,values_fn=list(value=mean)) %>%
  setNames(c(names(.)[1], paste0("glu", names(.)[2:length(.)])))
# Number:42281

# HEMOGLOBIN 血红蛋白
hemoglobin <- laboratory_file %>% 
  filter(test=="HEMOGLOBIN") %>%
  select(-test) %>%
  pivot_wider(names_from = year,names_sort = TRUE,values_from = value,values_fn=list(value=mean)) %>%
  setNames(c(names(.)[1], paste0("hb", names(.)[2:length(.)])))
# Number:49339

# HEMOGLOBIN_1AC 血红蛋白A1C
hba1c <- laboratory_file %>% filter(test=="HEMOGLOBIN_1AC") %>%
  select(-test) %>%
  pivot_wider(names_from = year,names_sort = TRUE,values_from = value,values_fn=list(value=mean)) %>%
  setNames(c(names(.)[1], paste0("hba1c", names(.)[2:length(.)])))
# Number:12882

# ALT 丙氨酸转氨酶 肝功能
alt <- laboratory_file %>% 
  filter(test=="ALT") %>%
  select(-test) %>%
  pivot_wider(names_from = year,names_sort = TRUE,values_from = value,values_fn=list(value=mean)) %>%
  setNames(c(names(.)[1], paste0("alt", names(.)[2:length(.)])))
# Number:46765

# AST 天冬氨酸转氨酶
ast <- laboratory_file %>% 
  filter(test=="AST") %>%
  select(-test) %>%
  pivot_wider(names_from = year,names_sort = TRUE,values_from = value,values_fn=list(value=mean)) %>%
  setNames(c(names(.)[1], paste0("ast", names(.)[2:length(.)])))
# Number:44072

# Merge data for all raw data
all_raw_data <- merge(x=patients, y=height,by="ID",all.x=TRUE) %>%
  
  #Add measurements  
  #Add weight
  merge(x=., y=weight,by="ID",all.x=TRUE) %>%
  #Add systolic blood pressure
  merge(x=., y=systolic,by="ID",all.x=TRUE) %>%
  #Add diastolic blood pressure
  merge(x=., y=diastolic,by="ID",all.x=TRUE) %>%
  #Add heartbeat
  merge(x=., y=heartbeat,by="ID",all.x=TRUE) %>%
  #Add smoking
  merge(x=., y=smoking,by="ID",all.x=TRUE) %>%
  
  #Add laboratories
  #Add Creatinine
  merge(x=., y=creatinine,by="ID",all.x=TRUE) %>%
  #Add  triglyceride
  merge(x=., y=triglyceriden,by="ID",all.x=TRUE) %>%
  #Add Total cholestrol
  merge(x=., y=chl,by="ID",all.x=TRUE) %>%
  #Add HDL cholestrol
  merge(x=., y=hdl,by="ID",all.x=TRUE) %>%
  #Add LDL cholestrol
  merge(x=., y=ldl,by="ID",all.x=TRUE) %>%
  #Add potassium
  merge(x=., y=K,by="ID",all.x=TRUE) %>%
  #Add uric acid
  merge(x=., y=uric_acid,by="ID",all.x=TRUE) %>%
  #Add blood glucose
  merge(x=., y=glucose,by="ID",all.x=TRUE) %>%
  #Add hemoglobin
  merge(x=., y=hemoglobin,by="ID",all.x=TRUE) %>%
  #Add hemoglobin a1c
  merge(x=., y=hba1c,by="ID",all.x=TRUE) %>%
  #Add alt
  merge(x=., y=alt,by="ID",all.x=TRUE) %>%
  #Add ast
  merge(x=., y=ast,by="ID",all.x=TRUE)

#write.csv(all_raw_data,"/ANALYSE_AREA/P_INTG01/Chaojie/data/all_raw_data.csv")

# Diagnoses
diagnoses_file <- read.csv("/ANALYSE_AREA/P_INTG01/Chaojie/dataset/DIAGNOSES.csv")
diagnoses <- diagnoses_file %>%
  rename("ID"="CPROJECT") %>%
  mutate("diagnose_date"=as.Date(as.character(.$DIAGNDATBEG),format="%d%b%Y")) %>%
  select(ID,diagnose_date,ICPCCODE)
# Get the diganose information of patients

# Nursing Home
nursing_home_file <- read.csv("/ANALYSE_AREA/P_INTG01/Chaojie/dataset/NURSING_HOME.csv")
nursing_home <- nursing_home_file %>%
  rename("ID"="SS00010","cost"="TOT_VERZORGINGSTEHUIZEN_R") %>%
  filter(cost > 0) %>%
  select(ID) %>%
  distinct(ID) %>%
  mutate(nursing_home="1")
# Get the ID of patients in nursing home

# Death
process_population_data <- function(years) {
  death <- data.frame(ID = character(), death_date = character(), stringsAsFactors = FALSE)
  
  for (year in years) {
    # Read population data
    population_file <- sprintf("/ANALYSE_AREA/P_INTG01/Chaojie/dataset/POPULATION%d.csv", year)
    population <- read.csv(population_file)
    
    # Process death data
    death_year <- population %>%
      rename("ID"="SS00010", "year"="PP0040A","month"="PP0040B") %>%
      filter(year > 0 & month > 0) %>%
      mutate("day" = 28) %>%
      mutate("death_date" = as.Date(paste(year,month,day,sep="-"),"%Y-%m-%d")) %>%
      select(ID, death_date)
    
    # Append to the overall death data
    death <- rbind(death, death_year)
  }
  
  return(death)
}
years <- c(2011, 2012, 2013, 2014, 2015)
death <- process_population_data(years)

#------------------------------------------------------Data Filtering------------------------------------------------------#
# Identify patients
# cvd list
cvd_list <- c("K74","K75","K76","K77","K78","K83","K86","K87","K89","K90","K91","K92")

# Select IDs of patients who were not diagnosed having cvd in 2011
diagnoses_ID_cvd <- diagnoses %>%
  mutate(cvd=ifelse(ICPCCODE %in% cvd_list,1,0)) %>%
  filter(diagnose_date<="2011-12-31" & diagnose_date>="2011-01-01") %>%
  filter(cvd==1) %>%
  arrange(ID,diagnose_date) %>%
  group_by(ID) %>%
  slice(1) %>%
  select(ID)
diagnoses_ID <- diagnoses %>%
  filter(diagnose_date<="2011-12-31" & diagnose_date>="2011-01-01") %>%
  arrange(ID,diagnose_date) %>%
  group_by(ID) %>%
  slice(1) %>%
  mutate(cvd=ifelse(ID %in% diagnoses_ID_cvd$ID,1,0)) %>%
  filter(cvd==0) %>%
  select(ID)
# Number:28076

diagnoses_cvd <- diagnoses %>%
  filter(diagnose_date<="2011-12-31" & diagnose_date>="2011-01-01") %>%
  arrange(ID,diagnose_date) %>%
  group_by(ID) %>%
  slice(1)
# Select IDs of patients who died in 2011
death_ID <- death %>%
  filter(death_date<="2011-12-31" & death_date>="2011-01-01")
# Number:644

# Identify target patients 
  # First identify patients who were not diagnosed with cvd in 2011
patients_data <- merge(x=diagnoses_ID,y=all_raw_data,by="ID",all=FALSE) %>%               # Number:26368
  
  # Remove patients died in 2011
  merge(x=.,y=death_ID,by="ID",all.x=TRUE) %>%
  mutate(death=case_when(is.na(death_date)==TRUE~0, is.na(death_date)==FALSE~1)) %>%      # Number:26247
  filter(death==0) %>%

  # Remove patients who lived at nursing home in 2011
  merge(x=., y=nursing_home,by="ID",all.x=TRUE) %>%
  mutate(nursing_home=case_when(is.na(nursing_home)==TRUE~0, is.na(nursing_home)==FALSE~1)) %>%
  filter(nursing_home==0) %>%                                                             # Number:26001
  
  #Select age >40
  filter(age>=40)  %>%                                                                    # Number:23421
  select(-death_date,-death,-nursing_home)

write.csv(patients_data,"/ANALYSE_AREA/P_INTG01/Chaojie/data/patients_raw_data.csv")

#------------------------------------------------------Data Imputation------------------------------------------------------#
# data setup for imputation
df_exclude_smoking <- patients_data %>%
  select(-starts_with("smoking")) %>%
  select(-ends_with("2000"),-ends_with("2001"),-ends_with("2002"),-ends_with("2003"),-ends_with("2004"),-ends_with("2005"),
         -ends_with("2006"),-ends_with("2007"),-ends_with("2008"),-ends_with("2009"),-ends_with("2010"))

df_smoking <- patients_data %>%
  select(ID,starts_with("smoking")) %>%
  select(-ends_with("2000"),-ends_with("2001"),-ends_with("2002"),-ends_with("2003"),-ends_with("2004"),-ends_with("2005"),-ends_with("2006"))

#write.csv(df_smoking,"/ANALYSE_AREA/P_INTG01/Chaojie/data/df_smoking.csv")
#write.csv(df_exclude_smoking,"/ANALYSE_AREA/P_INTG01/Chaojie/data/df_exclude_smoking.csv")

# data imputation
# for predictor variables excluding , use the average value
# Random seed is 123
df_ex_smoking_mice <- mice(df_exclude_smoking,m=5,method="pmm",maxit=10,seed=123)
df_ex_smoking_imp <- mice::complete(df_ex_smoking_mice,action=1)
df_ex_smoking_imp_long <- mice::complete(df_ex_smoking_mice,action="long")

# Convert numerical format to factor format
df_smoking[,2:ncol(df_smoking)] <- lapply(df_smoking[,2:ncol(df_smoking)], as.factor)

df_smoking_mice <- mice(df_smoking,m=5,method="logreg",maxit=10,seed=123)
df_smoking_imp <- mice::complete(df_smoking_mice,action=1)
df_smoking_imp_long <- mice::complete(df_smoking_mice,action="long")

#Combine imputed data
imputed_data <- merge(x=df_ex_smoking_imp, y=df_smoking_imp, by="ID")
imputed_data_long <- cbind(df_ex_smoking_imp_long, df_smoking_imp_long[,4:12]) 

#write.csv(imputed_data,"/ANALYSE_AREA/P_INTG01/Chaojie/data/imputed_data.csv")
#write.csv(imputed_data_long,"/ANALYSE_AREA/P_INTG01/Chaojie/data/imputed_data_long.csv")

imputed_data <- read.csv("/ANALYSE_AREA/P_INTG01/Chaojie/data/imputed_data.csv",row.names = 1)
imputed_data_long <- read.csv("/ANALYSE_AREA/P_INTG01/Chaojie/data/imputed_data_long.csv",row.names = 1)

#------------------------------------------------------Data Process------------------------------------------------------#
# Follow up target patients from 2012 to 2015
# Check whether target patients die or not
diagnoses_1215_cvd <- diagnoses %>%
  mutate(cvd=ifelse(ICPCCODE %in% cvd_list,1,0)) %>%
  filter(diagnose_date<="2015-12-31" & diagnose_date>="2012-01-01") %>%
  filter(cvd==1) %>%
  arrange(ID,diagnose_date) %>%
  group_by(ID) %>%
  slice(1) %>%
  rename("cvd_date"="diagnose_date") %>%
  select(ID,cvd_date)
# Number:6792

# Check whether target patients dead or not from 2012 to 2015
death_1215 <- filter(death, death_date <= "2015-12-31" & death_date >= "2012-01-01") 
# Number:3493

# Modify patients data for further analysis
patients_df <- imputed_data %>%
  merge(x=., y=diagnoses_1215_cvd, by="ID",all.x=TRUE) %>%
  merge(x=., y=death_1215,by="ID", all.x=TRUE) %>%
  
  # Modify cvd
  mutate(cvd=case_when(is.na(cvd_date)==TRUE~0, is.na(cvd_date)==FALSE~1)) %>%
  mutate(cvd_date=if_else(is.na(cvd_date),as.Date("2015-12-31"),as.Date(cvd_date))) %>%
  mutate(cvd_time=round(difftime(cvd_date,as.Date("2012-01-01"),units = "days")/365,2)) %>%
  
  # Modify death
  mutate(death=case_when(is.na(death_date)==TRUE~0, is.na(death_date)==FALSE~1)) %>%
  mutate(death_date=if_else(is.na(death_date),as.Date("2015-12-31"),as.Date(death_date))) %>%
  mutate(death_time=round(difftime(death_date,as.Date("2012-01-01"),units = "days")/365,2)) %>%
  
  # Identify status
  mutate(status=case_when((cvd==0 & death==0)~"alive",
                          (cvd==1 & death==0)~"cvd",
                          (cvd==0 & death==1)~"dead",
                          (cvd==1 & death==1 & cvd_date < death_date)~"cvd",
                          (cvd==1 & death==1 & cvd_date >= death_date)~"dead")) %>%
  # Identify time
  mutate(status_date=case_when((cvd==0 & death==0)~cvd_date,
                         (cvd==1 & death==0)~cvd_date,
                         (cvd==0 & death==1)~death_date,
                         (cvd==1 & death==1 & cvd_date < death_date)~cvd_date,
                         (cvd==1 & death==1 & cvd_date >= death_date)~death_date)) %>%
  mutate(years=round(difftime(as.Date(status_date),as.Date("2012-01-01"),units = "days")/365,2)) %>%

# For smoking, handle it with special method
  mutate(smoking=rowSums(select(., 89:93))) %>%
  mutate(smoking=ifelse(smoking>2,1,0)) %>%
  select(-starts_with("smoking20"))
  
#write.csv(patients_df,"/ANALYSE_AREA/P_INTG01/Chaojie/data/patients_df.csv")

#------------------------------------------------------Data Converting------------------------------------------------------#
# write a function to generate patients' longformat data by year
generate_longformat_df <- function(data, years, year_mapping) {
  longformat_df <- list()
  
  for (i in seq_along(years)) {
    year <- years[i]
    year_suffix <- as.character(year)
    year_value <- year_mapping[i]
    
    longformat_df[[year_suffix]] <- data %>%
      select(ID, years, starts_with("status"), sex, age, height, smoking, ends_with(year_suffix),
             starts_with("cvd"), starts_with("death")) %>%
      rename_with(~gsub(year_suffix, "", .x), ends_with(year_suffix)) %>%
      # Calculate chlratio(chl/hdl) 
      mutate(chlrati=chl/hdl) %>%
      # Calculate eGFR according to The CKD-EPI Equation
      mutate(eGFR=case_when(
        sex=="M" & cre<=0.7 ~ 144*(cre/0.7)^(-0.329)*(0.993)^age,
        sex=="M" & cre>0.7 ~ 144*(cre/0.7)^(-1.209)*(0.993)^age,
        sex=="F" & cre<=0.9 ~ 141*(cre/0.9)^(-0.411)*(0.993)^age,
        sex=="F" & cre>0.9 ~ 141*(cre/0.9)^(-1.209)*(0.993)^age,
        TRUE ~ NA_real_
      )) %>%
      # Creat year varible to represents the year when measurements and laboratories were taken
      mutate(year = year_value) %>%
      select(ID, years, starts_with("status"), year, sex, age, height, weight, smoking,
             sys, dia, hbeat, eGFR, cre, tri, chl, hdl, chlratio, K, UA, glu, hb, hba1c,
             alt, ast, starts_with("cvd"), starts_with("death"))
  }
  
  return(longformat_df)
}

# Get patients' longformatdata by year
# Use year = 0,0.5,1.5,2.5,3.5 to represent year2011, year2012, year2013, year2014, year2015 respectively
years <- c(2011, 2012, 2013, 2014, 2015)
year_mapping <- c(0, 0.5, 1.5, 2.5, 3.5)
longformat_dfs <- generate_longformat_df(patients_df, years, year_mapping)

# Access individual data frames by year suffix
longformat2011_df <- longformat_dfs[["2011"]]
longformat2012_df <- longformat_dfs[["2012"]]
longformat2013_df <- longformat_dfs[["2013"]]
longformat2014_df <- longformat_dfs[["2014"]]
longformat2015_df <- longformat_dfs[["2015"]]

# Create the patients' longformat  data
longformat_patients_df<- do.call(rbind, list(longformat2011_df, longformat2012_df, longformat2013_df,longformat2014_df,longformat2015_df)) %>%
  # Remove data record after some patients died or patients who were diagnosed with cvd
  filter(years >= year) %>%
  arrange(ID,year) %>%
  # convert formats of some variables
  mutate(sex=ifelse(sex=="F", 0, 1)) %>%
  mutate_at(vars(sex,smoking,cvd,death,status), factor) %>%
  # Round numerical variables
  mutate_at(vars(8:9, 11:26), ~ round(., digits = 2))

#write.csv(longformat_patients_df,"/ANALYSE_AREA/P_INTG01/Chaojie/data/longformat_patients_df.csv")






