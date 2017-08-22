# 22nd August 2017 
# Does regression analysis for predicting SRS from age, Trisomy and pre-post.
#Also creates and writes to file the means and SD for individual scales

#setwd("~/Dropbox/ERCadvanced/project SCT analysis/SCT_ASD_analysis/Project_files/Analysis")

  
library(tidyr)
library(stringr)
library(tidyverse)
dir<-"/Users/dorothybishop/Dropbox/ERCadvanced/project SCT analysis/SCT_ASD_analysis/Project_files/Data/"
mydata<-read.csv(paste0(dir,"SCTData_DATA_2017-08-22_0939.csv"))

#deal with missing data
for (mycol in 242:318){
  mymiss=which(mydata[,mycol]>900)
  mydata[mymiss,mycol]=NA
} 
prenatals<-which(mydata$pre_postnatal_diag==0)
postnatals<-which(mydata$pre_postnatal_diag==1)
mydata$trisomy <- factor(mydata$trisomy,
                         levels = c(1,2,3),
                         labels = c("XXX", "XXY", "XYY")) 
mydata$asd<-factor(mydata$asd,
                         levels=c(0,1,2),
                         labels=c("None","Suspected","Diagnosed"))
mydata$pre_postnatal_diag<-factor(mydata$pre_postnatal_diag,
                   levels=c(0,1),
                   labels=c("Prenatal","Postnatal"))

colnames(mydata)[5]='Diagnosis'
colnames(mydata)[4]='Trisomy'
colnames(mydata)[2]<-'Age'
colnames(mydata)[253]<-'SRS'
lmmdata<-select(mydata,Age,Trisomy,Diagnosis,SRS,socaw_ss,soccog_ss,soccomm_ss,socmot_ss,autfeat_ss)

summary(lm(SRS ~ Trisomy+Diagnosis+Age, data = mydata))

#do a table of means etc for each of the SRS subscales
require(psych)

nicetab<-data.frame(matrix(rep(NA,12*7),nrow=12))

colnames(nicetab)<-c('Diagnosis','.','Prenatal','.','.','Postnatal','.')
nicetab[1:12,1]<-c('Trisomy','N','Social Aware','SD',
                   'Social Cognition','SD','Social Comm','SD',
                   'Social Motiv','SD','Autistic Features','SD')
nicetab[1,2:7]<-c('XXX','XXY','XYY','XXX','XXY','XYY')

mypre<-lmmdata[prenatals,]
mypost<-lmmdata[postnatals,]

for (k in 1:2){
bigtab1<-describeBy(mypre[5:9], mypre$Trisomy)
myoffset<-0
if (k==2){bigtab1<-describeBy(mypost[5:9], mypost$Trisomy)
myoffset<-3 }
for (i in 1:5){
  for (j in 1:3){
    b=bigtab1[[j]]
    nicetab[2,j+1+myoffset]<-b[2,2]
    myrow<-(i-1)*2+3
    nicetab[myrow,(j+1+myoffset)]<-round(b[i,3],1)
    nicetab[(myrow+1),(j+1+myoffset)]<-round(b[i,4],2)
  }
}
}

write.csv(nicetab,file=paste0(dir,'nicetab_srs.csv'),row.names=FALSE)