





rm(list=ls())
#Chi-Square
#install.packages("MASS")
library(MASS)
setwd("E:/Amy/ISU/Research/Analysis/R_work_all_Data")

tbl<-read.csv("Cleptoparasite_Chi_Square.csv")
col1<-c(1,4,1,5,1,1,1,4,2,2,3,1,1,1,1,1,2,1,1,1,1,1,1)
col2<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
probs<-cbind(col1,col2)
chisq.test(tbl)

Pearson's Chi-squared test

data:  tbl
X-squared = 38, df = 22, p-value = 0.01832



tbl<-read.csv("Guild_Contingency_Table.csv")
chisq.test(tbl)

Pearson's Chi-squared test

data:  tbl
X-squared = 14.476, df = 18, p-value = 0.6976


