# Have coded information from parental telephone interview (held on N drive, xls files) into
# numeric codes compatible with redcap.
# This script just reads latest redcap file to estabish which record_ids to use, then
# selects columns from the xls file to create a new leaner file that can be imported
# into redcap.

# Ran this on 22nd August 2017 and imported the data, and then exported redcap as csv.
# Have deleted the telephone interview files from this computer, as they contain sensitive information.


#before running this, need to create version of xlsx workbook without security
library(readxl)
library(tidyr)
library(stringr)
library(tidyverse)
dir<-"/Users/dorothybishop/Dropbox/ERCadvanced/project SCT analysis/SCT_ASD_analysis/Project_files/Data/"
xlswb <- paste0(dir,"Telephone interview for import.xlsx")
 for (j in 1:7)  {                                      
  wb <- read_excel(xlswb,sheet=j)           #load workbook

    if (j==1) dataset<-wb else dataset<-cbind(dataset,wb)   #append to previous
    
 }

mydata<-dataset[order(dataset$Code),]
redcap.data<-read.csv(paste0(dir,"SCTData_DATA_2017-08-22_0727.csv"))

dataset$Code<-as.integer(dataset$Code)
mydata<-data.frame(dataset[order(dataset$Code),])

#delete from dataset those not found in redcap.data
a<-redcap.data$record_id
mydata<-filter(mydata,Code %in% a)
#now create the columns for needed information
keepcols<-c(1,19,25,32,35,36,28,29,30,39,42,46,80,81,83,88,94,109,115)
nudata<-mydata[,keepcols]
colnames(nudata)[1]<-'record_id'
nudata$record_id<-as.character(nudata$record_id)
w<-which(mydata$Code<100)
nudata$record_id[w]<-paste0("0",nudata$record_id)
ww<-which(mydata$Code<10)
nudata$record_id[ww]<-paste0("0",nudata$record_id)

#Add column indicating whether NHS referral (1) or other (2)
# This information came from an accruals sheet that listed source of referral: on Ndrive
nhscases<-c(70, 212, 213, 214, 215, 218, 221, 222, 223, 225, 229, 231,
            233, 234, 237, 239, 240, 241, 247, 251, 252, 254, 255, 257,
            258, 264, 265, 268, 270, 273, 274, 276, 283, 285, 297, 304,
            306, 307, 310, 318, 319, 321, 322, 323, 327, 328, 329, 331,
            334, 335, 336, 337, 340, 342, 346, 348, 351, 352, 355, 357,
            358, 360, 362, 365, 367, 368, 369)
nhsnum<-which(nudata$record_id %in% nhscases)
nudata$referral_from<-2
nudata$referral_from[nhsnum]<-1
nudata$mo_educ<-round(nudata$mo_educ,0)

colnames(nudata)[14]<-'asd' #was capitalised,won't work with redcap

nudata$fa_educ[is.na(nudata$fa_educ)] <- 9
nudata$mo_educ[is.na(nudata$mo_educ)] <- 9
nudata$schooling[is.na(nudata$schooling)] <- 9
nudata$first_words[is.na(nudata$first_words)] <- 9
nudata$lang_concerns[is.na(nudata$lang_concerns)] <- 9
nudata$why_tested[is.na(nudata$why_tested)] <- 9
nudata$fh_langprobs[is.na(nudata$fh_langprobs)] <- 9
nudata$asd[is.na(nudata$asd)] <- 9

write.csv(nudata, file = "telephone_interview_for_redcap.csv",row.names=FALSE)

#Later tweak of age_diag
w<-which(redcap.data$age_diag==0)
redcap.data$age_diag[w]<- -1 #truly prenatal
xx<-c(45, 58,88,120,139)
redcap.data$age_diag[xx]<-0 #tested at birth bcs concerns re baby

s<-select(redcap.data,record_id,age_diag)
#again need to ensure record_id format as characters
s$record_id<-as.character(s$record_id)
w<-which(redcap.data$record_id<100)
s$record_id[w]<-paste0("0",s$record_id[w])
ww<-which(redcap.data$record_id<10)
s$record_id[ww]<-paste0("0",s$record_id[ww])

write.csv(s,file='agediag_update.csv',row.names=FALSE)
