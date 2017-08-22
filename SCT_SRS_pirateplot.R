# 22nd August 2017 

#setwd("~/Dropbox/ERCadvanced/project SCT analysis/SCT_ASD_analysis/Project_files/Analysis")

  
library(tidyr)
library(stringr)
library(tidyverse)
library(yarrr)
dir<-"/Users/dorothybishop/Dropbox/ERCadvanced/project SCT analysis/SCT_ASD_analysis/Project_files/Data/"
mydata<-read.csv(paste0(dir,"SCTData_DATA_2017-08-22_0939.csv"))

for (mycol in 242:318){
  mymiss=which(mydata[,mycol]>900)
  mydata[mymiss,mycol]=NA
} 
mydata$trisomy <- factor(mydata$trisomy,
                         levels = c(1,2,3),
                         labels = c("XXX", "XXY", "XYY")) 
mydata$asd<-factor(mydata$asd,
                         levels=c(0,1,2),
                         labels=c("None","Suspected","Diagnosed"))
mydata$pre_postnatal_diag<-factor(mydata$pre_postnatal_diag,
                   levels=c(0,1),
                   labels=c("Prenatal","Postnatal"))

datacols=ncol(mydata)
mydata$hiSRS=0 #add new column for those with high SRS
hiscore=which(mydata$srs_t_score>75) #cutoff for autism
mydata$hiSRS[hiscore]=1
nascore=which(is.na(mydata$srs_t_score))
mydata$hiSRS[nascore]=NA
colnames(mydata)[5]='Diagnosis'
colnames(mydata)[4]='Trisomy'

nonauts<-which(mydata$aut==0)
autfeat<-which(mydata$aut==1)
auts<-which(mydata$aut==2)

attach(mydata)
#png(file="mygraphic2.png",width=400,height=350)
pirateplot(formula = mydata$srs_t_score~ Trisomy + Diagnosis,
           point.o = .5,
           bar.o=.0,
           inf.o=.2,
           bean.o=.5,
           jitter=.2,
           data = mydata,
           ylab='T-score',
           ylim=c(30,100),
           main="SRS total")
#dev.off()
detach(mydata)

# 
# plot(mydata$gcc, mydata$srs_t_score, 
#      main = "Scatterplot with the pony palette",
#      pch = 21, 
#      col=c(1:10)[mydata$Diagnosis],
#      bty = "n", 
#      cex = 2,
#      legend(x="topright", legend = levels(mydata$Diagnosis)))
# 
# plot(mydata$gcc, mydata$srs_t_score,, pch=c(1:10)[mydata$Diagnosis],col=c(1:3)[mydata$Trisomy])
# 
# table(mydata$Trisomy,mydata$Diagnosis,mydata$hiSRS)
