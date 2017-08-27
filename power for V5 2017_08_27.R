library(lme4)
library(plyr)

mod.2p<-readRDS("mod.2p.rds")
main.data2<-readRDS("maindata2.rds")

  N=30
  nsim<-1 
  
  
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
  
  simdat<-expand.grid(Type=factor(1:4),SetInd=unique(main.data2$SetInd),
                      ID=factor(1:N)) 
  
  #Recode into familiar factors for Kuppu/DB hypotheses.
  #expdat$Type<-as.numeric(expdat$Type)
  simdat$Type<-car::recode(simdat$Type,"1='rand';2='Adj_D';3='Adj_P';4='Non_D'")
   simdat$Type<-factor(simdat$Type,levels=c("rand","Adj_D","Adj_P", "Non_D"))
  
  simdat$p123<-1 #default
  w<-which(simdat$SetInd>phase1.end)
  simdat$p123[w]<-2
  ww<-which(simdat$SetInd>phase2.end)
  simdat$p123[ww]<-3
  simdat$p123<-as.factor(simdat$p123)
  levels(simdat$p123)<-c('Learn','Break','Final')
  simdat$p123<-factor(simdat$p123,levels=c("Break","Learn","Final"))#reorder factor levels
  
  simdat$SetInd_c<-scale(simdat$SetInd)
  simdat$b1<-simdat$SetInd_c
  simdat$b1[w]<-0
  simdat$b1[ww]<-0
  simdat$b2<-0
  simdat$b2[w]<-simdat$SetInd_c[w]
  simdat$b2[ww]<-0
  simdat$b3<-0
  simdat$b3[ww]<-simdat$SetInd_c[ww]
  simdat$SetInd_c<-as.factor(simdat$SetInd_c)
  
  #Extract parameters for simulating data. (artificially reduce the effects of setInd and Type to see if power is working. Are the estimates reasonable)
    # newparams_rd <- list(
    # beta = getME(mod1d2,"beta"), #fixed effects (intercept, Block, TPPR)
    # theta = getME(mod1d2, "theta"),#Random slope
    # sigma = getME(mod1d2, "sigma"))#random slope variance
    
    newparams_rd <- list(
    beta = getME(mod.2p,"beta"), #fixed effects (intercept, Block, TPPR)
    theta = getME(mod.2p, "theta"),#Random slope
    sigma = 0.5*getME(mod.2p, "sigma"))
    
bp1=unique(simdat$SetInd_c)[phase2.start] #cutpoint 1
bp2=unique(simdat$SetInd_c)[phase3.start] #cutpoint 2
#

  
  # Simulate:
  
  # ss <- simulate(~ Type + b1(SetInd_c, bp1) + b2(SetInd_c, bp1,bp2) + b3(SetInd_c,bp2) 
  #                + Type*b1(SetInd_c, bp1) + Type*b2(SetInd_c, bp1,bp2) + Type*b3(SetInd_c, bp2) 
  #                +(b1(SetInd_c, bp1) + b3(SetInd_c, bp2)| ID)
  #                +(0+phase0+phase1+phase2|ID), nsim = nsim, newdata = simdat, 
  #                newparams = newparams_rd, family=gaussian, re.form=~0, allow.new.levels=TRUE)
 ss <- simulate(~ Type*p123   #nb will automatically do main effects as well
                 +(0+b1*Type+b2*Type+b3*Type|ID), sim = nsim, newdata = simdat, 
                newparams = newparams_rd, family=gaussian, re.form=~0, allow.new.levels=TRUE)

  #####################################################################
  
     
    simdat$TargetRT <- ss[, 1]
 
 simdat20<-simdat[as.integer(simdat$ID)<21,]
   new1<- lmer(TargetRT ~ Type*p123  #nb will automatically do main effects as well
                 +(0+b1*Type+b2*Type+b3*Type|ID), data = simdat20, 
                 REML = TRUE,control = lmerControl(optimizer = "optimx", calc.derivs = TRUE,
                                                   optCtrl = list(method = "nlminb")))     
   
   newsum<-summary(new1)
   newranef<-ranef(new1)
  
  ###########################################################
  
  library(RColorBrewer)
  library(ggplot2)
  
  newdat1d<-expand.grid(Type=unique(simdat$Type),SetInd_c=unique(simdat$SetInd_c)[1:24],ID=unique(simdat$ID))
newdat2d<-expand.grid(Type=unique(simdat$Type),SetInd_c=unique(simdat$SetInd_c)[25:32],ID=unique(simdat$ID))
newdat3d<-expand.grid(Type=unique(simdat$Type),SetInd_c=unique(simdat$SetInd_c)[33:40],ID=unique(simdat$ID))

newdat1d$phase0<-ifelse(newdat1d$SetInd %in% unique(main.data2$SetInd_c)[1:24],1,0)
newdat1d$phase1<-ifelse(newdat1d$SetInd %in% unique(main.data2$SetInd_c)[25:32],1,0) 
newdat1d$phase2<-ifelse(newdat1d$SetInd %in% unique(main.data2$SetInd_c)[33:40],1,0) 

newdat2d$phase0<-ifelse(newdat2d$SetInd %in% unique(main.data2$SetInd_c)[1:24],1,0) 
newdat2d$phase1<-ifelse(newdat2d$SetInd %in% unique(main.data2$SetInd_c)[25:32],1,0) 
newdat2d$phase2<-ifelse(newdat2d$SetInd %in% unique(main.data2$SetInd_c)[33:40],1,0) 

newdat3d$phase0<-ifelse(newdat3d$SetInd %in% unique(main.data2$SetInd_c)[1:24],1,0)
newdat3d$phase1<-ifelse(newdat3d$SetInd %in% unique(main.data2$SetInd_c)[25:32],1,0) 
newdat3d$phase2<-ifelse(newdat3d$SetInd %in% unique(main.data2$SetInd_c)[33:40],1,0) 

  ggplot(simdat, aes(x = SetInd_c, y = log_TargetRT,color=Type)) + 
  geom_point(alpha=0.35) + 
  geom_vline(aes(xintercept = 0.3896708), color = 'grey', size = 1, linetype = 'dashed') + 
  geom_vline(aes(xintercept = 1.082419), color = 'grey', size = 1, linetype = 'dashed') +
  geom_line(data=newdat1d,aes(y=predict(new1,newdata=newdat1d)),size = .75)+
    geom_line(data=newdat2d,aes(y=predict(new1,newdata=newdat2d)),size = .75)+
    geom_line(data=newdat3d,aes(y=predict(new1,newdata=newdat3d)),size = .75)+
  theme_bw()+facet_wrap(~ID)+ scale_fill_brewer(palette="Set1")+
  theme(legend.position = "top",strip.text=element_text(size=12),axis.text=element_text(size=12),axis.title=element_text(size=12,face="bold"))

