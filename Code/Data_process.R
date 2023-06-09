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
  group_by(CPROJECT) %>%
  slice(1)
patients <- patients_file %>%              # Number:68426
  rename("ID"="CPROJECT","sex"="GESL") %>%
  # caculate the age of patients
  mutate(age=2011-GEBJR) %>%
  filter(JCG>=2011) %>%
  mutate(min_visit = min(JCG),max_visit = max(JCG)) %>%
  filter(min_visit<2012 & max_visit>2011) %>% 
  group_by(ID) %>%
  slice(1) %>%
  select(ID,sex,age)
# Number:62578

###Measurements###
measurements_file_1215 <- read.csv("/ANALYSE_AREA/P_INTG01/Chaojie/dataset/MEASUREMENTS.csv") %>%
  rename("ID"="CPROJECT","test"="MEASURE_TEST","value"="MEASURE_VALUE") %>%
  filter(!test=="SMOKING STATUS") %>%
  mutate(MTWDAT=as.Date(MTWDAT, format="%d/%m/%Y")) %>%
  mutate(year=format(MTWDAT, "%Y")) %>%
  mutate(month=format(MTWDAT,"%m")) %>%
  filter(year >= 2012) %>%
  mutate(year = case_when(
    as.numeric(month) <= 4 ~ paste0(year, "a"),
    as.numeric(month) <= 8 ~ paste0(year, "b"),
    as.numeric(month) <= 12 ~ paste0(year, "c")
  )) %>%
  select(ID,year,test,value)

measurements_file_2011 <- read.csv("/ANALYSE_AREA/P_INTG01/Chaojie/dataset/MEASUREMENTS.csv") %>%
  rename("ID"="CPROJECT","test"="MEASURE_TEST","value"="MEASURE_VALUE") %>%
  filter(!test=="SMOKING STATUS") %>%
  mutate(MTWDAT=as.Date(MTWDAT, format="%d/%m/%Y")) %>%
  mutate(year=format(MTWDAT, "%Y")) %>%
  mutate(month=format(MTWDAT,"%m")) %>%
  filter(year == 2011) %>%
  select(ID,year,test,value)

# SYSTOLIC BLOOD PRESSURE
systolic_1215 <- measurements_file_1215 %>% 
  filter(test=="SYSTOLIC BLOOD PRESSURE") %>%
  mutate(value=as.numeric(as.character(value))) %>%
  select(-test) %>%
  pivot_wider(names_from = year,names_sort = TRUE,values_from = value,values_fn=list(value=mean)) %>%
  setNames(c(names(.)[1], paste0("sys", names(.)[2:length(.)])))

systolic_2011 <- measurements_file_2011 %>% 
  filter(test=="SYSTOLIC BLOOD PRESSURE") %>%
  mutate(value=as.numeric(as.character(value))) %>%
  select(-test) %>%
  pivot_wider(names_from = year,names_sort = TRUE,values_from = value,values_fn=list(value=mean)) %>%
  setNames(c(names(.)[1], paste0("sys", names(.)[2:length(.)])))

systolic <- merge(x=systolic_2011,y=systolic_1215,by="ID",all=TRUE)

# DIASTOLIC BLOOD PRESSURE
diastolic_2011 <- measurements_file_2011 %>% 
  filter(test=="DIASTOLIC BLOOD PRESSURE") %>%
  mutate(value=as.numeric(as.character(value))) %>%
  select(-test) %>%
  pivot_wider(names_from = year,names_sort = TRUE,values_from = value,values_fn=list(value=mean)) %>%
  setNames(c(names(.)[1], paste0("dia", names(.)[2:length(.)])))

diastolic_1215 <- measurements_file_1215 %>% 
  filter(test=="DIASTOLIC BLOOD PRESSURE") %>%
  mutate(value=as.numeric(as.character(value))) %>%
  select(-test) %>%
  pivot_wider(names_from = year,names_sort = TRUE,values_from = value,values_fn=list(value=mean)) %>%
  setNames(c(names(.)[1], paste0("dia", names(.)[2:length(.)])))

diastolic <- merge(x=diastolic_2011,y=diastolic_1215,by="ID",all=TRUE)

# HEART BEAT
heartbeat_2011 <- measurements_file_2011 %>% 
  filter(test=="HEARTBEAT") %>%
  mutate(value=as.numeric(as.character(value))) %>%
  select(-test) %>%
  pivot_wider(names_from = year,names_sort = TRUE,values_from = value,values_fn=list(value=mean)) %>%
  setNames(c(names(.)[1], paste0("hbeat", names(.)[2:length(.)])))

heartbeat_1215 <- measurements_file_1215 %>% 
  filter(test=="HEARTBEAT") %>%
  mutate(value=as.numeric(as.character(value))) %>%
  select(-test) %>%
  pivot_wider(names_from = year,names_sort = TRUE,values_from = value,values_fn=list(value=mean)) %>%
  setNames(c(names(.)[1], paste0("hbeat", names(.)[2:length(.)])))

heartbeat <- merge(x=heartbeat_2011,y=heartbeat_1215,by="ID",all=TRUE)

# WEIGHT
weight_2011 <- measurements_file_2011 %>% 
  filter(test=="WEIGHT") %>%
  mutate(value=as.numeric(as.character(value))) %>%
  select(-test) %>%
  pivot_wider(names_from = year,names_sort = TRUE,values_from = value,values_fn=list(value=mean)) %>%
  setNames(c(names(.)[1], paste0("weight", names(.)[2:length(.)])))

weight_1215 <- measurements_file_1215 %>% 
  filter(test=="WEIGHT") %>%
  mutate(value=as.numeric(as.character(value))) %>%
  select(-test) %>%
  pivot_wider(names_from = year,names_sort = TRUE,values_from = value,values_fn=list(value=mean)) %>%
  setNames(c(names(.)[1], paste0("weight", names(.)[2:length(.)])))

weight <- merge(x=weight_2011,y=weight_1215,by="ID",all=TRUE)

# HEIGHT
height <-  read.csv("/ANALYSE_AREA/P_INTG01/Chaojie/dataset/MEASUREMENTS.csv") %>%
  data.frame(.) %>%
  rename("ID"="CPROJECT","test"="MEASURE_TEST","value"="MEASURE_VALUE") %>%
  filter(test=="HEIGHT") %>%
  #adjust height data and remove some height data which is unrealistic
  mutate(MTWDAT=as.Date(MTWDAT, format="%d/%m/%Y")) %>%
  mutate(year=format(MTWDAT, "%Y")) %>%
  mutate(value=as.numeric(as.character(value))) %>%
  filter(value >= 50) %>%
  filter(year >= 2000) %>%
  select(ID,year,value) %>%
  pivot_wider(names_from = year,names_sort = TRUE,values_from = value,values_fn=list(value=mean)) %>%
  #Use the average height
  mutate(height=(select(., -ID) %>% rowMeans(na.rm=TRUE))) %>%
  select(ID,height)

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
  setNames(c(names(.)[1], paste0("smoking", names(.)[2:length(.)]))) %>% 
  mutate(smoking=ifelse(rowMeans(.[2:17], na.rm = TRUE)==0,0,1)) %>%
  select(ID,smoking)

###LABORATORY###
laboratory_file_2011 <- read.csv("/ANALYSE_AREA/P_INTG01/Chaojie/dataset/LABORATORY.csv") %>%
  rename("ID"="CPROJECT","test"="LAB_TEST","value"="LAB_VALUE") %>%
  mutate(LABDAT=as.character(LABDAT)) %>%
  mutate(LABDAT=as.Date(LABDAT, format = "%d%b%Y"))%>%
  mutate(year=format(LABDAT, "%Y")) %>%
  mutate(month=format(LABDAT,"%m")) %>%
  filter(year == 2011) %>%
  select(ID,year,test,value)

laboratory_file_1215 <- read.csv("/ANALYSE_AREA/P_INTG01/Chaojie/dataset/LABORATORY.csv") %>%
  rename("ID"="CPROJECT","test"="LAB_TEST","value"="LAB_VALUE") %>%
  mutate(LABDAT=as.character(LABDAT)) %>%
  mutate(LABDAT=as.Date(LABDAT, format = "%d%b%Y"))%>%
  mutate(year=format(LABDAT, "%Y")) %>%
  mutate(month=format(LABDAT,"%m")) %>%
  filter(year >= 2012) %>%
  mutate(year = case_when(
    as.numeric(month) <= 4 ~ paste0(year, "a"),
    as.numeric(month) <= 8 ~ paste0(year, "b"),
    as.numeric(month) <= 12 ~ paste0(year, "c")
  )) %>%
  select(ID,year,test,value)

# CREATININE 肌酐
creatinine_2011 <- laboratory_file_2011 %>% 
  filter(test=="CREATININE") %>%
  select(-test) %>%
  pivot_wider(names_from = year,names_sort = TRUE,values_from = value,values_fn=list(value=mean)) %>%
  setNames(c(names(.)[1], paste0("cre", names(.)[2:length(.)])))

creatinine_1215 <- laboratory_file_1215 %>% 
  filter(test=="CREATININE") %>%
  select(-test) %>%
  pivot_wider(names_from = year,names_sort = TRUE,values_from = value,values_fn=list(value=mean)) %>%
  setNames(c(names(.)[1], paste0("cre", names(.)[2:length(.)])))

creatinine <- merge(x=creatinine_2011,y=creatinine_1215,by="ID",all=TRUE)

# TRIGLYCERIDEN 甘油三酯
triglyceriden_2011 <- laboratory_file_2011 %>% 
  filter(test == "TRIGLYCERIDEN") %>%
  select(-test) %>%
  pivot_wider(names_from = year, names_sort = TRUE, values_from = value, values_fn = list(value = mean)) %>%
  setNames(c(names(.)[1], paste0("tri", names(.)[2:length(.)])))

triglyceriden_1215 <- laboratory_file_1215 %>% 
  filter(test == "TRIGLYCERIDEN") %>%
  select(-test) %>%
  pivot_wider(names_from = year, names_sort = TRUE, values_from = value, values_fn = list(value = mean)) %>%
  setNames(c(names(.)[1], paste0("tri", names(.)[2:length(.)])))

triglyceriden <- merge(x = triglyceriden_2011, y = triglyceriden_1215, by = "ID", all = TRUE)

# TOTAL_CHOLESTEROL 总胆固醇
chl_2011 <- laboratory_file_2011 %>% 
  filter(test=="TOTAL_CHOLESTEROL") %>%
  select(-test) %>%
  pivot_wider(names_from = year,names_sort = TRUE,values_from = value,values_fn=list(value=mean)) %>%
  setNames(c(names(.)[1], paste0("chl", names(.)[2:length(.)])))

chl_1215 <- laboratory_file_1215 %>% 
  filter(test=="TOTAL_CHOLESTEROL") %>%
  select(-test) %>%
  pivot_wider(names_from = year,names_sort = TRUE,values_from = value,values_fn=list(value=mean)) %>%
  setNames(c(names(.)[1], paste0("chl", names(.)[2:length(.)])))

chl <- merge(x=chl_2011,y=chl_1215,by="ID",all=TRUE)

# HDL_CHOLESTEROL （高密度脂蛋白）胆固醇
hdl_2011 <- laboratory_file_2011 %>% 
  filter(test == "HDL_CHOLESTEROL") %>%
  select(-test) %>%
  pivot_wider(names_from = year, names_sort = TRUE, values_from = value, values_fn = list(value = mean)) %>%
  setNames(c(names(.)[1], paste0("hdl", names(.)[2:length(.)])))

hdl_1215 <- laboratory_file_1215 %>% 
  filter(test == "HDL_CHOLESTEROL") %>%
  select(-test) %>%
  pivot_wider(names_from = year, names_sort = TRUE, values_from = value, values_fn = list(value = mean)) %>%
  setNames(c(names(.)[1], paste0("hdl", names(.)[2:length(.)])))

hdl <- merge(x = hdl_2011, y = hdl_1215, by = "ID", all = TRUE)

# LDL_CHOLESTEROL （低密度脂蛋白）胆固醇
ldl_2011 <- laboratory_file_2011 %>% 
  filter(test == "LDL_CHOLESTEROL") %>%
  select(-test) %>%
  pivot_wider(names_from = year, names_sort = TRUE, values_from = value, values_fn = list(value = mean)) %>%
  setNames(c(names(.)[1], paste0("ldl", names(.)[2:length(.)])))

ldl_1215 <- laboratory_file_1215 %>% 
  filter(test == "LDL_CHOLESTEROL") %>%
  select(-test) %>%
  pivot_wider(names_from = year, names_sort = TRUE, values_from = value, values_fn = list(value = mean)) %>%
  setNames(c(names(.)[1], paste0("ldl", names(.)[2:length(.)])))

ldl <- merge(x = ldl_2011, y = ldl_1215, by = "ID", all = TRUE)

# POTASSIUM 钾
K_2011 <- laboratory_file_2011 %>% 
  filter(test == "POTASSIUM") %>%
  select(-test) %>%
  pivot_wider(names_from = year, names_sort = TRUE, values_from = value, values_fn = list(value = mean)) %>%
  setNames(c(names(.)[1], paste0("K", names(.)[2:length(.)])))

K_1215 <- laboratory_file_1215 %>% 
  filter(test == "POTASSIUM") %>%
  select(-test) %>%
  pivot_wider(names_from = year, names_sort = TRUE, values_from = value, values_fn = list(value = mean)) %>%
  setNames(c(names(.)[1], paste0("K", names(.)[2:length(.)])))

K <- merge(x = K_2011, y = K_1215, by = "ID", all = TRUE)

# URIC ACID 尿酸
UA_2011 <- laboratory_file_2011 %>% 
  filter(test == "URIC ACID") %>%
  select(-test) %>%
  pivot_wider(names_from = year, names_sort = TRUE, values_from = value, values_fn = list(value = mean)) %>%
  setNames(c(names(.)[1], paste0("UA", names(.)[2:length(.)])))

UA_1215 <- laboratory_file_1215 %>% 
  filter(test == "URIC ACID") %>%
  select(-test) %>%
  pivot_wider(names_from = year, names_sort = TRUE, values_from = value, values_fn = list(value = mean)) %>%
  setNames(c(names(.)[1], paste0("UA", names(.)[2:length(.)])))

UA <- merge(x = UA_2011, y = UA_1215, by = "ID", all = TRUE)

# GLUCOSE 血糖
glu_2011 <- laboratory_file_2011 %>% 
  filter(test == "GLUCOSE") %>%
  select(-test) %>%
  pivot_wider(names_from = year, names_sort = TRUE, values_from = value, values_fn = list(value = mean)) %>%
  setNames(c(names(.)[1], paste0("glu", names(.)[2:length(.)])))

glu_1215 <- laboratory_file_1215 %>% 
  filter(test == "GLUCOSE") %>%
  select(-test) %>%
  pivot_wider(names_from = year, names_sort = TRUE, values_from = value, values_fn = list(value = mean)) %>%
  setNames(c(names(.)[1], paste0("glu", names(.)[2:length(.)])))

glu <- merge(x = glu_2011, y = glu_1215, by = "ID", all = TRUE)

# HEMOGLOBIN 血红蛋白
hemoglobin_2011 <- laboratory_file_2011 %>% 
  filter(test == "HEMOGLOBIN") %>%
  select(-test) %>%
  pivot_wider(names_from = year, names_sort = TRUE, values_from = value, values_fn = list(value = mean)) %>%
  setNames(c(names(.)[1], paste0("hb", names(.)[2:length(.)])))

hemoglobin_1215 <- laboratory_file_1215 %>% 
  filter(test == "HEMOGLOBIN") %>%
  select(-test) %>%
  pivot_wider(names_from = year, names_sort = TRUE, values_from = value, values_fn = list(value = mean)) %>%
  setNames(c(names(.)[1], paste0("hb", names(.)[2:length(.)])))

hemoglobin <- merge(x = hemoglobin_2011, y = hemoglobin_1215, by = "ID", all = TRUE)

# HEMOGLOBIN_1AC 血红蛋白A1C
hemoglobin_1ac_2011 <- laboratory_file_2011 %>% 
  filter(test == "HEMOGLOBIN_1AC") %>%
  select(-test) %>%
  pivot_wider(names_from = year, names_sort = TRUE, values_from = value, values_fn = list(value = mean)) %>%
  setNames(c(names(.)[1], paste0("hb1ac", names(.)[2:length(.)])))

hemoglobin_1ac_1215 <- laboratory_file_1215 %>% 
  filter(test == "HEMOGLOBIN_1AC") %>%
  select(-test) %>%
  pivot_wider(names_from = year, names_sort = TRUE, values_from = value, values_fn = list(value = mean)) %>%
  setNames(c(names(.)[1], paste0("hb1ac", names(.)[2:length(.)])))

hemoglobin_1ac <- merge(x = hemoglobin_1ac_2011, y = hemoglobin_1ac_1215, by = "ID", all = TRUE)

# ALT 丙氨酸转氨酶 肝功能
alt_2011 <- laboratory_file_2011 %>% 
  filter(test == "ALT") %>%
  select(-test) %>%
  pivot_wider(names_from = year, names_sort = TRUE, values_from = value, values_fn = list(value = mean)) %>%
  setNames(c(names(.)[1], paste0("alt", names(.)[2:length(.)])))

alt_1215 <- laboratory_file_1215 %>% 
  filter(test == "ALT") %>%
  select(-test) %>%
  pivot_wider(names_from = year, names_sort = TRUE, values_from = value, values_fn = list(value = mean)) %>%
  setNames(c(names(.)[1], paste0("alt", names(.)[2:length(.)])))

alt <- merge(x = alt_2011, y = alt_1215, by = "ID", all = TRUE)

# AST 天冬氨酸转氨酶
ast_2011 <- laboratory_file_2011 %>% 
  filter(test == "AST") %>%
  select(-test) %>%
  pivot_wider(names_from = year, names_sort = TRUE, values_from = value, values_fn = list(value = mean)) %>%
  setNames(c(names(.)[1], paste0("ast", names(.)[2:length(.)])))

ast_1215 <- laboratory_file_1215 %>% 
  filter(test == "AST") %>%
  select(-test) %>%
  pivot_wider(names_from = year, names_sort = TRUE, values_from = value, values_fn = list(value = mean)) %>%
  setNames(c(names(.)[1], paste0("ast", names(.)[2:length(.)])))

ast <- merge(x = ast_2011, y = ast_1215, by = "ID", all = TRUE)

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
  merge(x=., y=UA,by="ID",all.x=TRUE) %>%
  #Add blood glucose
  merge(x=., y=glu,by="ID",all.x=TRUE) %>%
  #Add hemoglobin
  merge(x=., y=hemoglobin,by="ID",all.x=TRUE) %>%
  #Add hemoglobin a1c
  merge(x=., y=hemoglobin_1ac,by="ID",all.x=TRUE) %>%
  #Add alt
  merge(x=., y=alt,by="ID",all.x=TRUE) %>%
  #Add ast
  merge(x=., y=ast,by="ID",all.x=TRUE)

all_raw_data <- all_raw_data[rowSums(is.na(all_raw_data[, 4:ncol(all_raw_data)])) < (ncol(all_raw_data) - 3), ] # Number 49555

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
patients_data <- merge(x=diagnoses_ID,y=all_raw_data,by="ID",all=FALSE) %>%               # Number:26036
  
  # Remove patients died in 2011
  merge(x=.,y=death_ID,by="ID",all.x=TRUE) %>%
  mutate(death=case_when(is.na(death_date)==TRUE~0, is.na(death_date)==FALSE~1)) %>%      # Number:25816
  filter(death==0) %>%
  
  # Remove patients who lived at nursing home in 2011
  merge(x=., y=nursing_home,by="ID",all.x=TRUE) %>%
  mutate(nursing_home=case_when(is.na(nursing_home)==TRUE~0, is.na(nursing_home)==FALSE~1)) %>%
  filter(nursing_home==0) %>%                                                             # Number:25559
  
  #Select age >40
  filter(age>=40)  %>%                                                                    # Number:23248
  select(-death_date,-death,-nursing_home)

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
patients_df <- patients_data %>%
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
  mutate(years=round(difftime(as.Date(status_date),as.Date("2012-01-01"),units = "days")/365,2))
write.csv(patients_df,"/ANALYSE_AREA/P_INTG01/Chaojie/Project/patients_raw_data.csv", row.names = FALSE)

# Filter data by year for data imputation
# Filter data of year 2011 for data imputation
patients_df_2011 <- patients_df %>%
  select(ID,years,starts_with("status"),starts_with("cvd"),starts_with("death"),sex,age,height,ends_with("2011"))
write.csv(patients_df_2011,"/ANALYSE_AREA/P_INTG01/Chaojie/Project/patients_df_2011.csv", row.names = FALSE)

# Filter smoking data for imputation (binomial variable)
smoking_df <- patients_df %>%
  select(ID,smoking)
write.csv(smoking_df,"/ANALYSE_AREA/P_INTG01/Chaojie/Project/smoking_df.csv", row.names = FALSE)

# Write a function for filtering data from 2012 to 2015
filter_and_assign_data <- function(data, years) {
  for (year in years) {
    filtered_data <- data %>%
      select(ID, ends_with(year)) %>%
      filter(rowSums(is.na(.[, 2:ncol(.)])) < (ncol(.) - 1))
    
    assign(paste0("patients_df_", year), filtered_data, envir = .GlobalEnv)
  }
}

years <- c("2012a", "2012b", "2012c", "2013a", "2013b", "2013c", "2014a", "2014b", "2014c", "2015a", "2015b", "2015c")
filtered_data <- filter_and_assign_data(patients_df, years)

#write.csv(patients_df_2012a, "/ANALYSE_AREA/P_INTG01/Chaojie/Project/patients_df_2012a.csv", row.names = FALSE)
#write.csv(patients_df_2012b, "/ANALYSE_AREA/P_INTG01/Chaojie/Project/patients_df_2012b.csv", row.names = FALSE)
#write.csv(patients_df_2012c, "/ANALYSE_AREA/P_INTG01/Chaojie/Project/patients_df_2012c.csv", row.names = FALSE)
#write.csv(patients_df_2013a, "/ANALYSE_AREA/P_INTG01/Chaojie/Project/patients_df_2013a.csv", row.names = FALSE)
#write.csv(patients_df_2013b, "/ANALYSE_AREA/P_INTG01/Chaojie/Project/patients_df_2013b.csv", row.names = FALSE)
#write.csv(patients_df_2013c, "/ANALYSE_AREA/P_INTG01/Chaojie/Project/patients_df_2013c.csv", row.names = FALSE)
#write.csv(patients_df_2014a, "/ANALYSE_AREA/P_INTG01/Chaojie/Project/patients_df_2014a.csv", row.names = FALSE)
#write.csv(patients_df_2014b, "/ANALYSE_AREA/P_INTG01/Chaojie/Project/patients_df_2014b.csv", row.names = FALSE)
#write.csv(patients_df_2014c, "/ANALYSE_AREA/P_INTG01/Chaojie/Project/patients_df_2014c.csv", row.names = FALSE)
#write.csv(patients_df_2015a, "/ANALYSE_AREA/P_INTG01/Chaojie/Project/patients_df_2015a.csv", row.names = FALSE)
#write.csv(patients_df_2015b, "/ANALYSE_AREA/P_INTG01/Chaojie/Project/patients_df_2015b.csv", row.names = FALSE)
#write.csv(patients_df_2015c, "/ANALYSE_AREA/P_INTG01/Chaojie/Project/patients_df_2015c.csv", row.names = FALSE)

#------------------------------------------------------Data Imputation------------------------------------------------------#
# for numerical variables
patients_df_2011_mice <- mice(patients_df_2011,m=5,method="pmm",maxit=50,seed=123)
patients_df_2011_imp <- mice::complete(patients_df_2011_mice,action=1)
patients_df_2011_imp_long <- mice::complete(patients_df_2011_mice,action="long")

patients_df_2012a_mice <- mice(patients_df_2012a, m = 5, method = "pmm", maxit = 50, seed = 123)
patients_df_2012a_imp <- mice::complete(patients_df_2012a_mice, action = 1)
patients_df_2012a_imp_long <- mice::complete(patients_df_2012a_mice, action = "long")

patients_df_2012b_mice <- mice(patients_df_2012b, m = 5, method = "pmm", maxit = 50, seed = 123)
patients_df_2012b_imp <- mice::complete(patients_df_2012b_mice, action = 1)
patients_df_2012b_imp_long <- mice::complete(patients_df_2012b_mice, action = "long")

patients_df_2012c_mice <- mice(patients_df_2012c, m = 5, method = "pmm", maxit = 50, seed = 123)
patients_df_2012c_imp <- mice::complete(patients_df_2012c_mice, action = 1)
patients_df_2012c_imp_long <- mice::complete(patients_df_2012c_mice, action = "long")

patients_df_2013a_mice <- mice(patients_df_2013a, m = 5, method = "pmm", maxit = 50, seed = 123)
patients_df_2013a_imp <- mice::complete(patients_df_2013a_mice, action = 1)
patients_df_2013a_imp_long <- mice::complete(patients_df_2013a_mice, action = "long")

patients_df_2013b_mice <- mice(patients_df_2013b, m = 5, method = "pmm", maxit = 50, seed = 123)
patients_df_2013b_imp <- mice::complete(patients_df_2013b_mice, action = 1)
patients_df_2013b_imp_long <- mice::complete(patients_df_2013b_mice, action = "long")

patients_df_2013c_mice <- mice(patients_df_2013c, m = 5, method = "pmm", maxit = 50, seed = 123)
patients_df_2013c_imp <- mice::complete(patients_df_2013c_mice, action = 1)
patients_df_2013c_imp_long <- mice::complete(patients_df_2013c_mice, action = "long")

patients_df_2014a_mice <- mice(patients_df_2014a, m = 5, method = "pmm", maxit = 50, seed = 123)
patients_df_2014a_imp <- mice::complete(patients_df_2014a_mice, action = 1)
patients_df_2014a_imp_long <- mice::complete(patients_df_2014a_mice, action = "long")

patients_df_2014b_mice <- mice(patients_df_2014b, m = 5, method = "pmm", maxit = 50, seed = 123)
patients_df_2014b_imp <- mice::complete(patients_df_2014b_mice, action = 1)
patients_df_2014b_imp_long <- mice::complete(patients_df_2014b_mice, action = "long")

patients_df_2014c_mice <- mice(patients_df_2014c, m = 5, method = "pmm", maxit = 50, seed = 123)
patients_df_2014c_imp <- mice::complete(patients_df_2014c_mice, action = 1)
patients_df_2014c_imp_long <- mice::complete(patients_df_2014c_mice, action = "long")

patients_df_2015a_mice <- mice(patients_df_2015a, m = 5, method = "pmm", maxit = 50, seed = 123)
patients_df_2015a_imp <- mice::complete(patients_df_2015a_mice, action = 1)
patients_df_2015a_imp_long <- mice::complete(patients_df_2015a_mice, action = "long")

patients_df_2015b_mice <- mice(patients_df_2015b, m = 5, method = "pmm", maxit = 50, seed = 123)
patients_df_2015b_imp <- mice::complete(patients_df_2015b_mice, action = 1)
patients_df_2015b_imp_long <- mice::complete(patients_df_2015b_mice, action = "long")

patients_df_2015c_mice <- mice(patients_df_2015c, m = 5, method = "pmm", maxit = 50, seed = 123)
patients_df_2015c_imp <- mice::complete(patients_df_2015c_mice, action = 1)
patients_df_2015c_imp_long <- mice::complete(patients_df_2015c_mice, action = "long")

# Special variable smoking (binary)
smoking_df <- patients_df %>%
  select(ID,smoking) %>%
  mutate(smoking=factor(smoking))

smoking_df_mice <- mice(smoking_df, m = 5, method = "logreg", maxit = 50, seed = 123)
smoking_df_imp <- mice::complete(smoking_df_mice, action = 1)
smoking_df_imp_long <- mice::complete(smoking_df_mice, action = "long")

# Modify all imputed data
patients_selected_df <- patients_df_2011_imp %>%
  select(1:13)

patients_imputed_df_2011 <- merge(x=patients_df_2011_imp,y=smoking_df_imp, by= "ID", all.x = TRUE)  %>%
  select(ID,years,starts_with("status"),starts_with("cvd"),starts_with("death"),sex,age,height,smoking,ends_with("2011")) %>%
  rename_with(~ gsub("2011$", "", .), ends_with("2011")) %>%
  mutate(year=0)

patients_imputed_df_2012a <- merge(x = patients_selected_df, y = smoking_df_imp, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2012a_imp, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2012a$", "", .), ends_with("2012a")) %>% 
  mutate(year=0.167)

patients_imputed_df_2012b <- merge(x = patients_selected_df, y = smoking_df_imp, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2012b_imp, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2012b$", "", .), ends_with("2012b")) %>%
  mutate(year=0.5)

patients_imputed_df_2012c <- merge(x = patients_selected_df, y = smoking_df_imp, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2012c_imp, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2012c$", "", .), ends_with("2012c")) %>%
  mutate(year=0.833)

patients_imputed_df_2013a <- merge(x = patients_selected_df, y = smoking_df_imp, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2013a_imp, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2013a$", "", .), ends_with("2013a")) %>%
  mutate(year=1.167)

patients_imputed_df_2013b <- merge(x = patients_selected_df, y = smoking_df_imp, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2013b_imp, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2013b$", "", .), ends_with("2013b")) %>%
  mutate(year=1.5)

patients_imputed_df_2013c <- merge(x = patients_selected_df, y = smoking_df_imp, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2013c_imp, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2013c$", "", .), ends_with("2013c")) %>%
  mutate(year=1.833)

patients_imputed_df_2014a <- merge(x = patients_selected_df, y = smoking_df_imp, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2014a_imp, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2014a$", "", .), ends_with("2014a")) %>%
  mutate(year=2.167)

patients_imputed_df_2014b <- merge(x = patients_selected_df, y = smoking_df_imp, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2014b_imp, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2014b$", "", .), ends_with("2014b")) %>%
  mutate(year=2.5)

patients_imputed_df_2014c <- merge(x = patients_selected_df, y = smoking_df_imp, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2014c_imp, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2014c$", "", .), ends_with("2014c")) %>%
  mutate(year=2.833)

patients_imputed_df_2015a <- merge(x = patients_selected_df, y = smoking_df_imp, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2015a_imp, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2015a$", "", .), ends_with("2015a")) %>%
  mutate(year=3.167)

patients_imputed_df_2015b <- merge(x = patients_selected_df, y = smoking_df_imp, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2015b_imp, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2015b$", "", .), ends_with("2015b")) %>%
  mutate(year=3.5)

patients_imputed_df_2015c <- merge(x = patients_selected_df, y = smoking_df_imp, by = "ID", all.x = TRUE) %>%
  merge(x = ., y = patients_df_2015c_imp, by = "ID", all.y = TRUE) %>%
  rename_with(~ gsub("2015c$", "", .), ends_with("2015c")) %>%
  mutate(year=3.833)

# Get longformat dataset for joint models
longformat_patients_df <- rbind(patients_imputed_df_2011,patients_imputed_df_2012a,patients_imputed_df_2012b, patients_imputed_df_2012c, patients_imputed_df_2013a, 
                                patients_imputed_df_2013b, patients_imputed_df_2013c, patients_imputed_df_2014a, patients_imputed_df_2014b, patients_imputed_df_2014c, 
                                patients_imputed_df_2015a, patients_imputed_df_2015b, patients_imputed_df_2015c) %>%
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
  # Calculate BMI
  mutate(BMI=weight/(height/100)^2) %>%
  select(1:4,34,5:30,32:33,35,31) %>%
  # Round some numerical variable
  mutate_at(vars(14, 16:34), ~ round(., digits = 2)) %>%
  # Remove data record after some patients died or patients who were diagnosed with cvd
  filter(years >= year) %>%
  arrange(ID, year)

# write.csv(longformat_patients_df,"/ANALYSE_AREA/P_INTG01/Chaojie/Project/longformat_patients_df.csv",row.names = FALSE)