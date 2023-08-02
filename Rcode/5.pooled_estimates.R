# Data Pooling #
library(dplyr)
options(scipen = 1000)
#--------------------------------------------Baseline Jointmodel  value--------------------------------------------#
remove(Jointfit_SCORE2_3)

summary_SCORE1 <- summary(Jointfit_SCORE2_1)
summary_SCORE2 <- summary(Jointfit_SCORE2_2)
summary_SCORE3 <- summary(Jointfit_SCORE2_3)

sur_1 <- summary_SCORE1$Survival
sur_2 <- summary_SCORE2$Survival
sur_3 <- summary_SCORE3$Survival

outcome1_1 <- summary_SCORE1$Outcome1
outcome1_2 <- summary_SCORE2$Outcome1
outcome1_3 <- summary_SCORE3$Outcome1

outcome2_1 <- summary_SCORE1$Outcome2
outcome2_2 <- summary_SCORE2$Outcome2
outcome2_3 <- summary_SCORE3$Outcome2

matrix <- c(0,0,0,0,0)
sur <- data.frame(matrix)%>%
  mutate(Mean=(as.numeric(sur_1[,1]) + as.numeric(sur_2[,1]) + as.numeric(sur_3[,1]))/3) %>%
  mutate(HR=exp(Mean)) %>%
  mutate(U_bar=(as.numeric(sur_1[,2]) + as.numeric(sur_2[,2]) + as.numeric(sur_3[,2]))/3) %>%
  mutate(B=((as.numeric(sur_1[,1])-Mean)^2+(as.numeric(sur_2[,1])-Mean)^2+(as.numeric(sur_3[,1])-Mean)^2)/2) %>%
  mutate(StDev=U_bar+B+B/3) %>%
  mutate(low=(as.numeric(sur_1[,3]) + as.numeric(sur_2[,3]) + as.numeric(sur_3[,3]))/3) %>%
  mutate(high=(as.numeric(sur_1[,4]) + as.numeric(sur_2[,4]) + as.numeric(sur_3[,4]))/3) %>%
  mutate(rhat=(as.numeric(sur_1[,6]) + as.numeric(sur_2[,6]) + as.numeric(sur_3[,6]))/3) %>%
  select(-matrix,-U_bar,-B) %>%
  `rownames<-`(c("sexM","age","smoking1","value(sys)","value(chlratio)")) %>%
  round(4)
  
outcome1 <- data.frame(matrix) %>%
  mutate(Mean=(as.numeric(outcome1_1[,1]) + as.numeric(outcome1_2[,1]) + as.numeric(outcome1_3[,1]))/3) %>%
  mutate(U_bar=(as.numeric(outcome1_1[,2]) + as.numeric(outcome1_2[,2]) + as.numeric(outcome1_3[,2]))/3) %>%
  mutate(B=((as.numeric(outcome1_1[,1])-Mean)^2+(as.numeric(outcome1_2[,1])-Mean)^2+(as.numeric(outcome1_3[,1])-Mean)^2)/2) %>%
  mutate(StDev=U_bar+B+B/3) %>%
  mutate(low=(as.numeric(outcome1_1[,3]) + as.numeric(outcome1_2[,3]) + as.numeric(outcome1_3[,3]))/3) %>%
  mutate(high=(as.numeric(outcome1_1[,4]) + as.numeric(outcome1_2[,4]) + as.numeric(outcome1_3[,4]))/3) %>%
  mutate(rhat=(as.numeric(outcome1_1[,6]) + as.numeric(outcome1_2[,6]) + as.numeric(outcome1_3[,6]))/3) %>%
  select(-matrix,-U_bar,-B) %>%
  `rownames<-`(c("Intercept","year","sexM","age","sigma")) %>%
  round(4)
  
outcome2 <- data.frame(matrix) %>%
  mutate(Mean=(as.numeric(outcome2_1[,1]) + as.numeric(outcome2_2[,1]) + as.numeric(outcome2_3[,1]))/3) %>%
  mutate(U_bar=(as.numeric(outcome2_1[,2]) + as.numeric(outcome2_2[,2]) + as.numeric(outcome2_3[,2]))/3) %>%
  mutate(B=((as.numeric(outcome2_1[,1])-Mean)^2+(as.numeric(outcome2_2[,1])-Mean)^2+(as.numeric(outcome2_3[,1])-Mean)^2)/2) %>%
  mutate(StDev=U_bar+B+B/3) %>%
  mutate(low=(as.numeric(outcome2_1[,3]) + as.numeric(outcome2_2[,3]) + as.numeric(outcome2_3[,3]))/3) %>%
  mutate(high=(as.numeric(outcome2_1[,4]) + as.numeric(outcome2_2[,4]) + as.numeric(outcome2_3[,4]))/3) %>%
  mutate(rhat=(as.numeric(outcome2_1[,6]) + as.numeric(outcome2_2[,6]) + as.numeric(outcome2_3[,6]))/3) %>%
  select(-matrix,-U_bar,-B) %>%
  `rownames<-`(c("Intercept","year","sexM","age","sigma")) %>%
  round(4)
csv_SCORE <- bind_rows(sur,outcome1,outcome2)
write.csv(csv_SCORE, file="csv_SCORE.csv")

#--------------------------------------------Baseline Jointmodel  value+slope -------------------------------------------#
remove(Jointfit_SCORE2_3)

summary_SCORE1 <- summary(Jointfit_SCORE2_s1)
summary_SCORE2 <- summary(Jointfit_SCORE2_s2)
summary_SCORE3 <- summary(Jointfit_SCORE2_s3)

sur_1 <- summary_SCORE1$Survival
sur_2 <- summary_SCORE2$Survival
sur_3 <- summary_SCORE3$Survival

outcome1_1 <- summary_SCORE1$Outcome1
outcome1_2 <- summary_SCORE2$Outcome1
outcome1_3 <- summary_SCORE3$Outcome1

outcome2_1 <- summary_SCORE1$Outcome2
outcome2_2 <- summary_SCORE2$Outcome2
outcome2_3 <- summary_SCORE3$Outcome2

matrix <- c(0,0,0,0,0,0,0)
sur <- data.frame(matrix)%>%
  mutate(Mean=(as.numeric(sur_1[,1]) + as.numeric(sur_2[,1]) + as.numeric(sur_3[,1]))/3) %>%
  mutate(HR=exp(Mean)) %>%
  mutate(U_bar=(as.numeric(sur_1[,2]) + as.numeric(sur_2[,2]) + as.numeric(sur_3[,2]))/3) %>%
  mutate(B=((as.numeric(sur_1[,1])-Mean)^2+(as.numeric(sur_2[,1])-Mean)^2+(as.numeric(sur_3[,1])-Mean)^2)/2) %>%
  mutate(StDev=U_bar+B+B/3) %>%
  mutate(low=(as.numeric(sur_1[,3]) + as.numeric(sur_2[,3]) + as.numeric(sur_3[,3]))/3) %>%
  mutate(high=(as.numeric(sur_1[,4]) + as.numeric(sur_2[,4]) + as.numeric(sur_3[,4]))/3) %>%
  mutate(rhat=(as.numeric(sur_1[,6]) + as.numeric(sur_2[,6]) + as.numeric(sur_3[,6]))/3) %>%
  select(-matrix,-U_bar,-B) %>%
  `rownames<-`(c("sexM","age","smoking1","value(sys)","slope(sys)","value(chlratio)","slope(chlratio)")) %>%
  round(4)

matrix <- c(0,0,0,0,0)
outcome1 <- data.frame(matrix) %>%
  mutate(Mean=(as.numeric(outcome1_1[,1]) + as.numeric(outcome1_2[,1]) + as.numeric(outcome1_3[,1]))/3) %>%
  mutate(U_bar=(as.numeric(outcome1_1[,2]) + as.numeric(outcome1_2[,2]) + as.numeric(outcome1_3[,2]))/3) %>%
  mutate(B=((as.numeric(outcome1_1[,1])-Mean)^2+(as.numeric(outcome1_2[,1])-Mean)^2+(as.numeric(outcome1_3[,1])-Mean)^2)/2) %>%
  mutate(StDev=U_bar+B+B/3) %>%
  mutate(low=(as.numeric(outcome1_1[,3]) + as.numeric(outcome1_2[,3]) + as.numeric(outcome1_3[,3]))/3) %>%
  mutate(high=(as.numeric(outcome1_1[,4]) + as.numeric(outcome1_2[,4]) + as.numeric(outcome1_3[,4]))/3) %>%
  mutate(rhat=(as.numeric(outcome1_1[,6]) + as.numeric(outcome1_2[,6]) + as.numeric(outcome1_3[,6]))/3) %>%
  select(-matrix,-U_bar,-B) %>%
  `rownames<-`(c("Intercept","year","sexM","age","sigma")) %>%
  round(4)

outcome2 <- data.frame(matrix) %>%
  mutate(Mean=(as.numeric(outcome2_1[,1]) + as.numeric(outcome2_2[,1]) + as.numeric(outcome2_3[,1]))/3) %>%
  mutate(U_bar=(as.numeric(outcome2_1[,2]) + as.numeric(outcome2_2[,2]) + as.numeric(outcome2_3[,2]))/3) %>%
  mutate(B=((as.numeric(outcome2_1[,1])-Mean)^2+(as.numeric(outcome2_2[,1])-Mean)^2+(as.numeric(outcome2_3[,1])-Mean)^2)/2) %>%
  mutate(StDev=U_bar+B+B/3) %>%
  mutate(low=(as.numeric(outcome2_1[,3]) + as.numeric(outcome2_2[,3]) + as.numeric(outcome2_3[,3]))/3) %>%
  mutate(high=(as.numeric(outcome2_1[,4]) + as.numeric(outcome2_2[,4]) + as.numeric(outcome2_3[,4]))/3) %>%
  mutate(rhat=(as.numeric(outcome2_1[,6]) + as.numeric(outcome2_2[,6]) + as.numeric(outcome2_3[,6]))/3) %>%
  select(-matrix,-U_bar,-B) %>%
  `rownames<-`(c("Intercept","year","sexM","age","sigma")) %>%
  round(4)
csv_SCORE_s <- bind_rows(sur,outcome1,outcome2)
write.csv(csv_SCORE_s, file="csv_SCORE_s.csv")

#----------------------------------------------------New Jointmodel with eGFR(value and value+slope)----------------------------------------------#
#----------------------value--------------------#
remove(Jointfit3_6)
summary1 <- summary(Jointfit1_6)
summary2 <- summary(Jointfit2_6)
summary3 <- summary(Jointfit3_6)

sur_1 <- summary1$Survival
sur_2 <- summary2$Survival
sur_3 <- summary3$Survival

outcome1_1 <- summary1$Outcome1
outcome1_2 <- summary2$Outcome1
outcome1_3 <- summary3$Outcome1

outcome2_1 <- summary1$Outcome2
outcome2_2 <- summary2$Outcome2
outcome2_3 <- summary3$Outcome2

outcome3_1 <- summary1$Outcome3
outcome3_2 <- summary2$Outcome3
outcome3_3 <- summary3$Outcome3

matrix <- c(0,0,0,0,0,0)
sur <- data.frame(matrix)%>%
  mutate(Mean=(as.numeric(sur_1[,1]) + as.numeric(sur_2[,1]) + as.numeric(sur_3[,1]))/3) %>%
  mutate(HR=exp(Mean)) %>%
  mutate(U_bar=(as.numeric(sur_1[,2]) + as.numeric(sur_2[,2]) + as.numeric(sur_3[,2]))/3) %>%
  mutate(B=((as.numeric(sur_1[,1])-Mean)^2+(as.numeric(sur_2[,1])-Mean)^2+(as.numeric(sur_3[,1])-Mean)^2)/2) %>%
  mutate(StDev=U_bar+B+B/3) %>%
  mutate(low=(as.numeric(sur_1[,3]) + as.numeric(sur_2[,3]) + as.numeric(sur_3[,3]))/3) %>%
  mutate(high=(as.numeric(sur_1[,4]) + as.numeric(sur_2[,4]) + as.numeric(sur_3[,4]))/3) %>%
  mutate(rhat=(as.numeric(sur_1[,6]) + as.numeric(sur_2[,6]) + as.numeric(sur_3[,6]))/3) %>%
  select(-matrix,-U_bar,-B) %>%
  `rownames<-`(c("sexM","age","smoking1","value(sys)","value(chlratio)","value(eGFR)")) %>%
  round(4)

matrix <- c(0,0,0,0,0,0)
outcome1 <- data.frame(matrix) %>%
  mutate(Mean=(as.numeric(outcome1_1[,1]) + as.numeric(outcome1_2[,1]) + as.numeric(outcome1_3[,1]))/3) %>%
  mutate(U_bar=(as.numeric(outcome1_1[,2]) + as.numeric(outcome1_2[,2]) + as.numeric(outcome1_3[,2]))/3) %>%
  mutate(B=((as.numeric(outcome1_1[,1])-Mean)^2+(as.numeric(outcome1_2[,1])-Mean)^2+(as.numeric(outcome1_3[,1])-Mean)^2)/2) %>%
  mutate(StDev=U_bar+B+B/3) %>%
  mutate(low=(as.numeric(outcome1_1[,3]) + as.numeric(outcome1_2[,3]) + as.numeric(outcome1_3[,3]))/3) %>%
  mutate(high=(as.numeric(outcome1_1[,4]) + as.numeric(outcome1_2[,4]) + as.numeric(outcome1_3[,4]))/3) %>%
  mutate(rhat=(as.numeric(outcome1_1[,6]) + as.numeric(outcome1_2[,6]) + as.numeric(outcome1_3[,6]))/3) %>%
  select(-matrix,-U_bar,-B) %>%
  `rownames<-`(c("Intercept","year","age","sexM","year:sexM","sigma")) %>%
  round(4)

outcome2 <- data.frame(matrix) %>%
  mutate(Mean=(as.numeric(outcome2_1[,1]) + as.numeric(outcome2_2[,1]) + as.numeric(outcome2_3[,1]))/3) %>%
  mutate(U_bar=(as.numeric(outcome2_1[,2]) + as.numeric(outcome2_2[,2]) + as.numeric(outcome2_3[,2]))/3) %>%
  mutate(B=((as.numeric(outcome2_1[,1])-Mean)^2+(as.numeric(outcome2_2[,1])-Mean)^2+(as.numeric(outcome2_3[,1])-Mean)^2)/2) %>%
  mutate(StDev=U_bar+B+B/3) %>%
  mutate(low=(as.numeric(outcome2_1[,3]) + as.numeric(outcome2_2[,3]) + as.numeric(outcome2_3[,3]))/3) %>%
  mutate(high=(as.numeric(outcome2_1[,4]) + as.numeric(outcome2_2[,4]) + as.numeric(outcome2_3[,4]))/3) %>%
  mutate(rhat=(as.numeric(outcome2_1[,6]) + as.numeric(outcome2_2[,6]) + as.numeric(outcome2_3[,6]))/3) %>%
  select(-matrix,-U_bar,-B) %>%
  `rownames<-`(c("Intercept","year","age","sexM","year:sexM","sigma")) %>%
  round(4)

outcome3 <- data.frame(matrix) %>%
  mutate(Mean=(as.numeric(outcome3_1[,1]) + as.numeric(outcome3_2[,1]) + as.numeric(outcome3_3[,1]))/3) %>%
  mutate(U_bar=(as.numeric(outcome3_1[,2]) + as.numeric(outcome3_2[,2]) + as.numeric(outcome3_3[,2]))/3) %>%
  mutate(B=((as.numeric(outcome3_1[,1])-Mean)^2+(as.numeric(outcome3_2[,1])-Mean)^2+(as.numeric(outcome3_3[,1])-Mean)^2)/2) %>%
  mutate(StDev=U_bar+B+B/3) %>%
  mutate(low=(as.numeric(outcome3_1[,3]) + as.numeric(outcome3_2[,3]) + as.numeric(outcome3_3[,3]))/3) %>%
  mutate(high=(as.numeric(outcome3_1[,4]) + as.numeric(outcome3_2[,4]) + as.numeric(outcome3_3[,4]))/3) %>%
  mutate(rhat=(as.numeric(outcome3_1[,6]) + as.numeric(outcome3_2[,6]) + as.numeric(outcome3_3[,6]))/3) %>%
  select(-matrix,-U_bar,-B) %>%
  `rownames<-`(c("Intercept","year","age","sexM","year:sexM","sigma")) %>%
  round(4)
csv_jointfit6 <- bind_rows(sur,outcome1,outcome2,outcome3)
write.csv(csv_jointfit6, file="csv_Jointfit6.csv")
#-----------------------value+slope----Jointfit1s----------------#
options(scipen = 1000)

summary1 <- summary(Jointfit1_1s)
summary2 <- summary(Jointfit2_1s)
summary3 <- summary(Jointfit3_1s)

sur_1 <- summary1$Survival
sur_2 <- summary2$Survival
sur_3 <- summary3$Survival

outcome1_1 <- summary1$Outcome1
outcome1_2 <- summary2$Outcome1
outcome1_3 <- summary3$Outcome1

outcome2_1 <- summary1$Outcome2
outcome2_2 <- summary2$Outcome2
outcome2_3 <- summary3$Outcome2

outcome3_1 <- summary1$Outcome3
outcome3_2 <- summary2$Outcome3
outcome3_3 <- summary3$Outcome3

matrix <- c(0,0,0,0,0,0,0,0,0)
sur <- data.frame(matrix)%>%
  mutate(Mean=(as.numeric(sur_1[,1]) + as.numeric(sur_2[,1]) + as.numeric(sur_3[,1]))/3) %>%
  mutate(HR=exp(Mean)) %>%
  mutate(U_bar=(as.numeric(sur_1[,2]) + as.numeric(sur_2[,2]) + as.numeric(sur_3[,2]))/3) %>%
  mutate(B=((as.numeric(sur_1[,1])-Mean)^2+(as.numeric(sur_2[,1])-Mean)^2+(as.numeric(sur_3[,1])-Mean)^2)/2) %>%
  mutate(StDev=U_bar+B+B/3) %>%
  mutate(low=(as.numeric(sur_1[,3]) + as.numeric(sur_2[,3]) + as.numeric(sur_3[,3]))/3) %>%
  mutate(high=(as.numeric(sur_1[,4]) + as.numeric(sur_2[,4]) + as.numeric(sur_3[,4]))/3) %>%
  mutate(rhat=(as.numeric(sur_1[,6]) + as.numeric(sur_2[,6]) + as.numeric(sur_3[,6]))/3) %>%
  select(-matrix,-U_bar,-B) %>%
  `rownames<-`(c("sexM","age","smoking1","value(sys)","slope(sys)","value(chlratio)","slope(chlratio)","value(eGFR)","slope(eGFR)")) %>%
  round(4)

matrix <- c(0,0,0,0,0,0,0,0)
outcome1 <- data.frame(matrix) %>%
  mutate(Mean=(as.numeric(outcome1_1[,1]) + as.numeric(outcome1_2[,1]) + as.numeric(outcome1_3[,1]))/3) %>%
  mutate(U_bar=(as.numeric(outcome1_1[,2]) + as.numeric(outcome1_2[,2]) + as.numeric(outcome1_3[,2]))/3) %>%
  mutate(B=((as.numeric(outcome1_1[,1])-Mean)^2+(as.numeric(outcome1_2[,1])-Mean)^2+(as.numeric(outcome1_3[,1])-Mean)^2)/2) %>%
  mutate(StDev=U_bar+B+B/3) %>%
  mutate(low=(as.numeric(outcome1_1[,3]) + as.numeric(outcome1_2[,3]) + as.numeric(outcome1_3[,3]))/3) %>%
  mutate(high=(as.numeric(outcome1_1[,4]) + as.numeric(outcome1_2[,4]) + as.numeric(outcome1_3[,4]))/3) %>%
  mutate(rhat=(as.numeric(outcome1_1[,6]) + as.numeric(outcome1_2[,6]) + as.numeric(outcome1_3[,6]))/3) %>%
  select(-matrix,-U_bar,-B) %>%
  `rownames<-`(c("Intercept","year","age","sexM","year:age","year:sexM","age:sexM","sigma")) %>%
  round(4)

outcome2 <- data.frame(matrix) %>%
  mutate(Mean=(as.numeric(outcome2_1[,1]) + as.numeric(outcome2_2[,1]) + as.numeric(outcome2_3[,1]))/3) %>%
  mutate(U_bar=(as.numeric(outcome2_1[,2]) + as.numeric(outcome2_2[,2]) + as.numeric(outcome2_3[,2]))/3) %>%
  mutate(B=((as.numeric(outcome2_1[,1])-Mean)^2+(as.numeric(outcome2_2[,1])-Mean)^2+(as.numeric(outcome2_3[,1])-Mean)^2)/2) %>%
  mutate(StDev=U_bar+B+B/3) %>%
  mutate(low=(as.numeric(outcome2_1[,3]) + as.numeric(outcome2_2[,3]) + as.numeric(outcome2_3[,3]))/3) %>%
  mutate(high=(as.numeric(outcome2_1[,4]) + as.numeric(outcome2_2[,4]) + as.numeric(outcome2_3[,4]))/3) %>%
  mutate(rhat=(as.numeric(outcome2_1[,6]) + as.numeric(outcome2_2[,6]) + as.numeric(outcome2_3[,6]))/3) %>%
  select(-matrix,-U_bar,-B) %>%
  `rownames<-`(c("Intercept","year","age","sexM","year:age","year:sexM","age:sexM","sigma")) %>%
  round(4)

outcome3 <- data.frame(matrix) %>%
  mutate(Mean=(as.numeric(outcome3_1[,1]) + as.numeric(outcome3_2[,1]) + as.numeric(outcome3_3[,1]))/3) %>%
  mutate(U_bar=(as.numeric(outcome3_1[,2]) + as.numeric(outcome3_2[,2]) + as.numeric(outcome3_3[,2]))/3) %>%
  mutate(B=((as.numeric(outcome3_1[,1])-Mean)^2+(as.numeric(outcome3_2[,1])-Mean)^2+(as.numeric(outcome3_3[,1])-Mean)^2)/2) %>%
  mutate(StDev=U_bar+B+B/3) %>%
  mutate(low=(as.numeric(outcome3_1[,3]) + as.numeric(outcome3_2[,3]) + as.numeric(outcome3_3[,3]))/3) %>%
  mutate(high=(as.numeric(outcome3_1[,4]) + as.numeric(outcome3_2[,4]) + as.numeric(outcome3_3[,4]))/3) %>%
  mutate(rhat=(as.numeric(outcome3_1[,6]) + as.numeric(outcome3_2[,6]) + as.numeric(outcome3_3[,6]))/3) %>%
  select(-matrix,-U_bar,-B) %>%
  `rownames<-`(c("Intercept","year","age","sexM","year:age","year:sexM","age:sexM","sigma")) %>%
  round(4)

csv_Jointfit1s <- bind_rows(sur,outcome1,outcome2,outcome3)
write.csv(csv_Jointfit1s, file="csv_Jointfit1s.csv")




