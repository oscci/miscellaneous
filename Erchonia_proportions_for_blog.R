

require(meta)
require(ggplot2)
mydf<- read.csv('Erchonia_proportions4.csv')

mydf$RR<-NA
mydf$SMD<-NA
mydf$SMDse<-NA
mydf$pT <- mydf$NgoodoutcomeT/mydf$Ntreated
mydf$pC <- mydf$NgoodoutcomeC/mydf$Ncontrol
mydf$pTse <-(mydf$pT*(1-mydf$pT))/sqrt(mydf$Ntreated)

mydf$pCse <-(mydf$pC*(1-mydf$pC))/sqrt(mydf$Ncontrol)
for (i in 1:nrow(mydf)){
  A=mydf$NgoodoutcomeT[i]
  B=mydf$Ntreated[i]
  C=mydf$NgoodoutcomeC[i]
  D=mydf$Ncontrol[i]
  
  META3 <- metabin(A,B,C,D ,sm="OR") #odds ratio and SMD via meta
  mydf$SMD[i] <- META3$TE*sqrt(3)/pi
  mydf$SMDse[i] <- sqrt(META3$seTE*META3$seTE*3/pi/pi)
  mydf$RR[i]<-(A/B)/(C/D)
  
}

mylongdat<-rbind(mydf[,1:16],mydf[,1:16])
mylongdat$Group <-'Treated'
mylongdat$Group[15:28]<-'Control'
mylongdat$Proportion <- mylongdat$pT
mylongdat$SE<-mylongdat$pTse
mylongdat$Proportion[15:28]<-mylongdat$pC[15:28]
mylongdat$SE[15:28]<-mylongdat$pCse[15:28]



mylongdat$mymin<-mylongdat$Proportion-mylongdat$SE
mylongdat$mymax<-mylongdat$Proportion+mylongdat$SE
mylongdat$SMD<-round(mylongdat$SMD,2)
mylongdat$SMDtext<-as.character(mylongdat$SMD)
mylongdat$SMDtext[15:28]<-''
p <- ggplot(mylongdat, aes(x=Proportion, y=label, fill=Group)) + 
  geom_bar(stat="identity",position=position_dodge())+
  #geom_text(aes(label = SMDtext,hjust=-.5)) +
  ggtitle("Erchonia trials")+
  xlab("Proportion improved")+
  geom_errorbar(aes(xmin=Proportion-SE, xmax=Proportion+SE), width=.2, position=position_dodge(.9))+
  theme(axis.title.y=element_blank() #remove y axis title
 )
p + theme(legend.position="top")

ggsave('proportionsErchonia.jpg',width=9,height=6)
