# 22nd August 2017 

library(tidyr)
library(stringr)
library(tidyverse)
dir<-"/Users/dorothybishop/Dropbox/ERCadvanced/project SCT analysis/SCT_ASD_analysis/Project_files/Data/"
redcap.data<-read.csv(paste0(dir,"SCTData_DATA_2017-08-22_0939.csv"))

table(redcap.data$trisomy,redcap.data$asd)

whytested<-table(redcap.data$referral_from,redcap.data$why_tested)
colnames(whytested)<-c('Mat age','medical','behav','neurodev','famhist','dk')
rownames(whytested)<-c('NHS','other')
whytested

whentested<-table(redcap.data$pre_postnatal_diag,redcap.data$why_tested)
colnames(whentested)<-c('Mat age','medical','behav','neurodev','famhist','dk')
rownames(whentested)<-c('prenatal','postnatal')
whentested

checkprepost<-table(redcap.data$age_diag,redcap.data$pre_postnatal_diag)
checkprepost

