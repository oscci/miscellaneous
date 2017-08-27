#pilot data analysis and tentative analysis script for the learning task
setwd("~/Dropbox/SL and WM paper/Revision/Pilot_Data_Revised task/V5")
#required packages
options(scipen=999)
#useful ref: http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html
#convergence problems: https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html
#also: https://stats.stackexchange.com/questions/242109/model-failed-to-converge-warning-in-lmer

#library(devtools)
#install_github(repo = "RDDtools", username = "MatthieuStigler", subdir = "RDDtools")
#install.packages("optimx")
library(RDDtools)
library(optimx)
library(lme4)
library(ggplot2)
library(doBy)
library(gridExtra)
library(tidyverse)
library(RColorBrewer)


breakblock<-c(7,8)
nsets<-5
nblocks<-10
phase1.start<-1
phase1.end<-nsets*(breakblock[1]-1)
phase2.start<-phase1.end+1
phase2.end<-nsets*(breakblock[2])
phase3.start<-phase2.end+1
phase3.end<-nsets*nblocks

phase1.range<-c(phase1.start:phase1.end)
phase2.range<-c(phase2.start:phase2.end)
phase3.range<-c(phase3.start:phase3.end)

#substitute windows for quartz if nonmac
if(.Platform$OS.type=="windows") {
  quartz<-function() windows()
} 

namelist=c('cathy','chris','dan','harriat','megan','sarah','yaling' )
nsubs<-length(namelist)

main.data<-data.frame(ID=factor(), SetInd=integer(), Type=integer(), TargetRT=integer())
lmsummarycoefs<-data.frame(matrix(NA,7*25,nrow=7))

for (i in 1:nsubs){
  
  myname=namelist[i]
  mycsv=paste(myname,".csv",sep="")
  mydata= read.csv(mycsv)  # read csv file 
  #get rid of RTs of inaccurate responses and any RT greater than 3000 ms:replace with NA. 
  Rwdata=mydata
  rawdata=Rwdata[ ,c(1,10,12,26)]
  #in the original analysis the following section will be removed 
  
  #######this bit ensures inaccurate becomes, so not eliminated in next section, and at least RT can be extracted. 
  rawdata$TargetRT[rawdata$Type==19 & rawdata$TargetRT>3000]<-2999
  ##############
  rawdata$TargetRT[rawdata$TargetACC==0]<-NA
  rawdata$TargetRT[rawdata$TargetRT<-199]<-NA #include anticipations up to -200
  rawdata$TargetRT[rawdata$TargetRT>2000]<-2000 
  
  RWdata<-rawdata
  
  #rename the types so median can be taken for each block 
  RWdata$Type[RWdata$Type==1]<- "Adj_D"
  RWdata$Type[RWdata$Type==2]<- "Adj_D"
  
  RWdata$Type[RWdata$Type==3]<- "Adj_P"
  RWdata$Type[RWdata$Type==4]<- "Adj_P"

  RWdata$Type[RWdata$Type==5]<- "Non_D"
  RWdata$Type[RWdata$Type==6]<- "Non_D"

  RWdata$Type[RWdata$Type==7]<- "rand"
  RWdata$Type[RWdata$Type==8]<- "rand"
  
  #RWdata$ID<-substring(RWdata$ID,1,2)
  RWdata$Type<-as.factor(RWdata$Type)
  RWdata$Type<-factor(RWdata$Type,levels=c("rand", "Adj_D", "Adj_P", "Non_D"))
  
  detaildata<- summaryBy(TargetRT ~ SetInd+Type,  data=RWdata,
                         FUN=c(min), na.rm=TRUE)
  #NB only 2 points to consider, so now taking minimum, not median
  # ah, but NB for Adj_Prob there are 4 points...
  
  detaildata$ID<-rep(RWdata$ID[4],length(detaildata[,1]))
  
  names(detaildata)<-c("SetInd", "Type", "TargetRT", "ID")
  

  #do linear regression for individual data to check against lmer
  colref<-1
  lmsummarycoefs[i,1]<-i
  thistype<-c('rand','Adj_D','Adj_P','Non_D')
  phasestart<-1;phaseend<-phase1.end
  for (myphase in 1:3){
    if(myphase==2) {phasestart<-phase2.start; phaseend<-phase2.end}
    if(myphase==3) {phasestart<-phase3.start; phaseend<-phase3.end}
    for (mytype in 1:4){
      colref<-colref+1
      colnames(lmsummarycoefs)[colref]<-paste0(thistype[mytype],'.b',myphase)
      colnames(lmsummarycoefs)[(colref+12)]<-paste0(thistype[mytype],'.a',myphase)
      mytemp<-filter(detaildata,Type==thistype[mytype],SetInd>(phasestart-1),SetInd<(phaseend+1))
      tempmodel<-lm(TargetRT ~ SetInd, data = mytemp) 
      lmsummarycoefs[i,colref]<-coef(tempmodel)[2] #slopes
      lmsummarycoefs[i,(colref+12)]<-coef(tempmodel)[1] #intercepts
     

    }
  }


   ggplot(detaildata, aes(x = SetInd, y = TargetRT,color=Type))  +
    geom_line()+ggtitle(namelist[i]) #plot this subject
  
  main.data<-rbind(main.data,detaildata)
  
}
#add coeffs for reg discontinuity, just for learning/random phases
for (i in 1:nsubs){
startcol<-17 #set so that this will append columns to 26 and beyond
for (mytype in 1:4){
  mytemp<-filter(main.data,ID==namelist[i],Type==thistype[mytype],SetInd<41)
  # using the RDDtools package
  RDD.temp<-RDDdata(y=mytemp$TargetRT,x=mytemp$SetInd,cutpoint=31)
  reg_para <- RDDreg_lm(RDDobject = RDD.temp, order = 1) #this is just linear: for higher order can increase
  startcol<-startcol+9
  endcol<-startcol+7
  lmsummarycoefs[i,startcol:endcol]<-reg_para$coefficients
  s<-summary(reg_para)$coefficients[2,4]#sig of slope diff for the two phases
  lmsummarycoefs[i,endcol+1]<-s
  colnames(lmsummarycoefs)[startcol:endcol]<-paste0(thistype[mytype],'.',names(reg_para$coefficients))
  colnames(lmsummarycoefs)[endcol+1]<-paste0(thistype[mytype],'_p.diff')
 }
}
write.csv(lmsummarycoefs, file = "lm_discont_coeffs_linear.csv")

main.data$ID <- as.factor(main.data$ID)
main.data$log_TargetRT<-log(main.data$TargetRT+200) #to allow for anticipatory responses
main.data<-data.frame(main.data)

summarytable<-summaryBy(TargetRT~SetInd+Type,data=main.data,FUN=c(mean),na.rm=TRUE)

ggplot(summarytable,aes(x = SetInd, y = TargetRT.mean,color=Type))  +
  geom_line()

ggplot(data=test_data_long,
       aes(x=date, y=value, colour=variable)) +
  geom_line()
################################################################################################################################################
  
main.data2<-main.data  

main.data2$phase1<-ifelse(main.data2$SetInd %in% phase1.range,1,0)
main.data2$phase2<-ifelse(main.data2$SetInd %in% phase2.range,1,0) #block where seq broken
main.data2$phase3<-ifelse(main.data2$SetInd %in% phase3.range,1,0) #block where seq restored

main.data2$SetInd_c<-scale(main.data2$SetInd) #? to centre this variable

# bp1=unique(main.data2$SetInd_c)[30] #cutpoint 1 - original values - I think wrong - see below, shld be == not []
# bp2=unique(main.data2$SetInd_c)[40] #cutpoint 2

myseq<-scale(seq(1,nblocks*nsets))
bp1<-myseq[phase2.start]-.01 #cutpoint1 on centred set index, nudged down so won't exclude sets with exact match of value
bp2<-myseq[phase3.start]-.01 #cutpoint2 on centred set index

#functions to determine if value x is in phase1, phase2 or phase3
#So this ignores values for sets not in this phase (sets to zero), but has all other phases scaled
#so that they show increment across phase
b1 <- function(x, bp1) ifelse(x < bp1, bp1 + x, 0) #NB altered by DB from bp1-x
b2 <- function(x, bp1, bp2) ifelse(x >= bp1 & x < bp2, x - bp1, 0)
b3 <- function(x, bp2) ifelse(x < bp2, 0, x - bp2)

#compute log RT
main.data2$log_TargetRT_2<-main.data2$log_TargetRT
#add b1, b2, b3 to main.data rather than computing during regression
#(also helps understand these functions by seeing how they relate to set)
main.data2$b1<-b1(main.data2$SetInd_c,bp1)
main.data2$b2<-b2(main.data2$SetInd_c,bp1,bp2)
main.data2$b3<-b3(main.data2$SetInd_c,bp2)
#Paul original model 
# mod1e2 <- lmer(log_TargetRT_2 ~ Type + b1(SetInd_c, bp1) + b2(SetInd_c, bp1,bp2) 
#                + b3(SetInd_c,bp2) + Type*b1(SetInd_c, bp1) + Type*b2(SetInd_c, bp1,bp2) 
#                + Type*b3(SetInd_c, bp2) 
#                +(b1(SetInd_c, bp1) + b3(SetInd_c, bp2)| ID)
#                +(0+broke0+broke1+broke2|ID), data = main.data2, 
#                REML = TRUE,control = lmerControl(optimizer = "optimx", calc.derivs = TRUE, 
#                                                  optCtrl = list(method = "nlminb"))) 
# I think from stackexchange advice above, that failure of convergence is because 
# b1 and b3 are specified as both fixed and random effects

# DB version with simplified terms (predefinied b1,b2,b3), also includes random effect for b2 as well as b1 and b3
# and uses raw RT - advantage of that is that slopes/intercepts are in meaningful units
# While it is the case that distribution of RT left-skewed, this is bcs subset of trials speed up:
# not sure we should remove that effect? With raw RT it does converge with tips below (from CRAN)


#Adopted suggestions for improving convergence from here: https://cran.r-project.org/web/packages/lme4/vignettes/lmerperf.html

#b1,b2,b3 are the centred set indices for phases 1, 2 and 3, and so are quantitative
#and so reflect the slope
#whereas phase1, phase2 and phase3 are just 0 or 1, so reflect overall mean diffs
mod.1e2 <- lmer(TargetRT ~ Type*b1 + Type*b2 + Type*b3 #nb will automatically do main effects as well
               #+(b1 +b2+ b3| ID)
               +(0+phase1*Type+phase2*Type+phase3*Type|ID), data = main.data2, 
               REML = TRUE,control = lmerControl(optimizer = "optimx", 
                   calc.derivs = FALSE, optCtrl = list(method = "nlminb",
                  starttests = FALSE, kkt = FALSE))) 
mod.1esum<-summary(mod.1e2) 
my.ranef<-ranef(mod.1e2) #random effects
write.csv(my.ranef[1], file = "mod1e_randp_effects.csv")

#This raises questions: a) results look more sensible and less noisy if interactions with phase are included
# rather than b1, b2, b3. This picks up overall mean RT difference from random for whole phase, rather than slope
# b) Paul's model had only b1 and b3 interaction terms in random effects. Was this deliberate? Is b2 a comparison?
# If so, should we code it like Type, as a factor, with b2 as first term? I will try that!

  main.data2$p123<-as.factor(main.data2$phase1+2*main.data2$phase2+3*main.data2$phase3)
  levels(main.data2$p123)<-c('Learn','Break','Final')
  main.data2$p123<-factor(main.data2$p123,levels=c("Break","Learn","Final"))#reorder factor levels
  main.data2$allb<-main.data2$SetInd_c
  w<-which(main.data2$p123=='Break')
  main.data2$allb[w]<-main.data2$b2[w]
  w2<-which(main.data2$p123=='Final')
  main.data2$allb[w2]<-main.data2$b3[w2]
 
# #Now redo model with phase as a factor (p123) - converges but not sure this is sensible.
# mod.2 <- lmer(TargetRT ~ Type*allb*p123
#                 +(0+p123*allb*Type|ID), data = main.data2,
#                REML = TRUE,control = lmerControl(optimizer = "optimx",
#                calc.derivs = FALSE, optCtrl = list(method = "nlminb",
#                 starttests = FALSE, kkt = FALSE)))
#  mod.2.sum<-summary(mod.2) 
#  my.ranef<-ranef(mod.2) #random effects


#Exploring the model by gradually adding terms - start with LM with no random effect
#This is identical to LMM except for random terms, yet b2 and b3 interactions are v different
#They are much more sensible in the LMM (ie random effect is ns there, and final phase sig)
  #Presumably this is because this treats all data as if from same source and ignores subject effects
mod.lm <- lm(TargetRT ~  Type*b1 + Type*b2 + Type*b3, data = main.data2, 
               REML = TRUE,control = lmerControl(optimizer = "optimx", 
                                                 calc.derivs = FALSE, optCtrl = list(method = "nlminb",
                                                                                     starttests = FALSE, kkt = FALSE)))
 summary(mod.lm)
 
 #what happens if b and phase terms exchanged as fixed/random?
 #Interesting :we get 'fixed-effect model matrix is rank deficient so dropping 4 columns / coefficients'
 #These are the phase 3 terms
 #But interesting to see Bolker advice on fixed/random:
 #'Treating factors with small numbers of levels as random will in the best case 
 # lead to very small and/or imprecise estimates of random effects'
 # Which would mean that phase should NOT be regarded as random effect

 mod.2e <- lmer(TargetRT ~ Type*phase1 + Type*phase3  #nb will automatically do main effects as well
                 +(0+b1*Type+b2*Type+b3*Type|ID), data = main.data2, 
                 REML = TRUE,control = lmerControl(optimizer = "optimx", calc.derivs = TRUE, optCtrl = list(method = "nlminb")))                               
 mod.2esum<-summary(mod.2e)$coefficients
 my.ranef2<-ranef(mod.2e) #random effects
 write.csv(my.ranef2[1], file = "mod.2e_randb_effects.csv")
 write.csv(mod.2esum,file="mod.2e.fixed.coeffs.csv")

 #Redo with p123 factor - is this the same? - YES!
 #Sensible result but note message:
 #Model failed to converge with max|grad| = 0.0229797 (tol = 0.002, component 1)
 #Parameters or bounds appear to have different scalings.
 #I think this refers to fact that there are 6 blocks for Learning but only 2 for Break and Final
 
 mod.2p <- lmer(TargetRT~ Type*p123  #nb will automatically do main effects as well
                +(0+b1*Type+b2*Type+b3*Type|ID), data = main.data2, 
                REML = TRUE,control = lmerControl(optimizer = "optimx", calc.derivs = TRUE, optCtrl = list(method = "nlminb")))                               
 mod.2psum<-summary(mod.2p)$coefficients 
 my.ranef2p<-ranef(mod.2p) #random effects
 write.csv(my.ranef2p[1], file = "mod.2p_randb_effects.csv")
 write.csv(mod.2psum,file="mod.2p.fixed.coeffs.csv")
 
 #Redo with logRT factor - compare regression diagnostics
 #This actually does worse - one degenerate Hessian! And I think reg diagnostics similar?
 mod.2pL <- lmer(log_TargetRT_2  ~ Type*p123  #nb will automatically do main effects as well
                +(0+b1*Type+b2*Type+b3*Type|ID), data = main.data2, 
                REML = TRUE,control = lmerControl(optimizer = "optimx", calc.derivs = TRUE, optCtrl = list(method = "nlminb")))                               
 mod.2psumL<-summary(mod.2pL)$coefficients 
 my.ranef2pL<-ranef(mod.2pL) #random effects
 write.csv(my.ranef2pL[1], file = "mod.2pL_randb_effects.csv")
 write.csv(mod.2psumL,file="mod.2pL.fixed.coeffs.csv")
  
##########################################################################################  
  #regression diagnostics
 quartz()
#  png(filename="merplot2.png")
grid.arrange(  
plot(mod.2pL,type=c("p","smooth")),

plot(mod.2pL,sqrt(abs(resid(.)))~fitted(.),
                  type=c("p","smooth"),ylab=expression(sqrt(abs(resid)))),

plot(mod.2pL,resid(.,type="pearson")~SetInd_c, type=c("p","smooth")))
#dev.off()

quartz()
#png(filename="merplot3.png")
qqnorm(resid(mod.2p))
qqline(resid(mod.2p)) # shape due to using whole numbers 1:40.

hist(resid(mod.2p),100)
#dev.off()
###########################################################################################

 
 
 ##########################################################################################
 #Simulating set for prediction - modified by DB to have phase as factor
startdat<-select(main.data2,ID,Type,SetInd,SetInd_c,p123,b1,b2,b3)
startdat$SetInd<-as.integer(startdat$SetInd)
newdat1d<-startdat[startdat$SetInd<31,]
newdat2d<-startdat[startdat$SetInd>30,]
newdat2d<-newdat2d[newdat2d$SetInd<41,]
newdat3d<-startdat[startdat$SetInd>40,]


 
 #plots for lmer
#NB - these illustrate why slope alone is not a good index of learning; see case of Yaling;
# she learned Det_Adj in first block, and then stayed flat - so learning indicated more by
# overall difference from random than by slope

# png(filename="merplot1.png")
 quartz(width=10,height=3)
 ggplot(main.data2, aes(x = SetInd_c, y = TargetRT,color=Type)) + 
   geom_point(alpha=0.35) + 
   geom_vline(aes(xintercept = bp1), color = 'grey', size = 1, linetype = 'dashed') + 
   geom_vline(aes(xintercept = bp2), color = 'grey', size = 1, linetype = 'dashed') +
   geom_line(data=newdat1d,aes(y=predict(mod.2p,newdata=newdat1d)),size = .75)+
   geom_line(data=newdat2d,aes(y=predict(mod.2p,newdata=newdat2d)),size = .75)+
   geom_line(data=newdat3d,aes(y=predict(mod.2p,newdata=newdat3d)),size = .75)+
   theme_bw()+facet_grid(~ID)+ scale_fill_brewer(palette="Set1")+
   theme(legend.position = "top",strip.text=element_text(size=12),axis.text=element_text(size=12),axis.title=element_text(size=12,face="bold"))
 #dev.off()
 
 #RDD bits - just playing with these
# https://github.com/MatthieuStigler/RDDtools
 # mytemp<-filter(main.data,Type=='Adj_D',SetInd<41)
 # 
 # RDD.AdjD<-RDDdata(y=mytemp$TargetRT,x=mytemp$SetInd,cutpoint=31,)
 # summary(RDD.AdjD)
 # plot(RDD.AdjD)
 # reg_para <- RDDreg_lm(RDDobject = RDD.AdjD, order = 4)
 # reg_para
 # plot(reg_para)
 # #simple local regression, using the Imbens and Kalyanaraman 2012 bandwidth:
 # bw_ik <- RDDbw_IK(RDD.AdjD)
 # reg_nonpara <- RDDreg_np(RDDobject = RDD.AdjD, bw = bw_ik)
 # print(reg_nonpara)
 # plot(reg_nonpara)
 # plotPlacebo(reg_nonpara)
 # 
 # firstbit<-filter(main.data,SetInd<41)
 # RDD.all<-RDDdata(y=firstbit$TargetRT,x=firstbit$SetInd,covar=firstbit$Type,cutpoint=31)
 # covarTest_mean(RDD.all, bw = 0.3)
 
 #save objects for use in power program
 saveRDS(mod.2p, "mod.2p.rds")
 saveRDS(main.data2,"maindata2.rds")