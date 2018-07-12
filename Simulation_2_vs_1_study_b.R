#-------------------------------------------------------------------------
# Given a certain N participants, better to do one study or two?
#-------------------------------------------------------------------------

library(doBy)
library(tidyverse)
# disable scientific notation in the output to make it easy to read
options(scipen=999) #http://stackoverflow.com/questions/25946047/how-to-prevent-scientific-notation-in-r

myNs<-c(24,48,96,192) #Total available sample *per group* in single 2 group study
myES<-c(0,0.2,.4,.6,.8) #effect size for difference between groups (you can try changing this)
allalpha <-c(.05,.005)
n.effects<-length(myES)
n.totalsample <- length(myNs)
n.alpha<-length(allalpha)
n.sims<-1000 #number of times to run the simulation

datadone<-0 #loop over data creation bit if large files already made
if(datadone==0){

mydf<-data.frame(matrix(NA,nrow=n.sims,ncol=10))
colnames(mydf)<-c('totN.per.gp','effsize','alpha','p.full','p.half1','p.half2','sigfull',
                  'sig.half1','sig.half2','replic.12')
  thisrow<-0
  for (a in 1:n.alpha){
    my.alpha<-allalpha[a]
  
    for (j in 1:n.totalsample){
  myN<-myNs[j]

for (i in 1:n.effects){ 
  myeff<-myES[i] 
  for (k in 1:n.sims){
    
    thisrow<-thisrow+1
    myvectorA<-rnorm(n = myN, mean = 0, sd = 1) 
    myvectorB<-rnorm(n = myN, mean = myeff, sd = 1)
    myvectorA1<-rnorm(n = myN/2, mean = 0, sd = 1) 
    myvectorB1<-rnorm(n = myN/2, mean = myeff, sd = 1)
    myvectorA2<-rnorm(n = myN/2, mean = 0, sd = 1) 
    myvectorB2<-rnorm(n = myN/2, mean = myeff, sd = 1)
    myt0 <- t.test(myvectorA,myvectorB,alternative='less') 
    myt1 <- t.test(myvectorA1,myvectorB1,alternative='less') 
    myt2 <- t.test(myvectorA2,myvectorB2,alternative='less') 
    mydf[thisrow,1]<-myN
    mydf[thisrow,2]<-myeff
    mydf[thisrow,3]<-my.alpha
    mydf[thisrow,4]<-myt0$p.value
    mydf[thisrow,5]<-myt1$p.value
    mydf[thisrow,6]<-myt2$p.value
    mydf[thisrow,7:9]<-0
    if(myt0$p.value<my.alpha){mydf[thisrow,7]<-1}
    if(myt1$p.value<my.alpha){mydf[thisrow,8]<-1}
    if(myt2$p.value<my.alpha){mydf[thisrow,9]<-1}
    mydf[thisrow,10]<-1 #will overwrite with zero unless both are 1
    mydf[thisrow,10]<-min(mydf[thisrow,8],mydf[thisrow,9]) #both < .05

       } #end of inner loop
      } 
    }
}#end of outer loop
write.table(mydf,paste0('simulate_single_rep_',n.sims,'sim.csv'))
}#start here if datadone==1

if(datadone==1){
 mydf<-read.csv('simulate_single_rep_1000sim.csv',sep=' ') #may need to change this if different parametrs
}

  #Assuming we're just comparing first 2 alpha levels in a plot: may want to change this
 mydf1<-filter(mydf,alpha==allalpha[1])
 mydf2<-filter(mydf,alpha==allalpha[2])
 sumtable1<- summaryBy(sigfull+sig.half1+replic.12 ~ effsize*totN.per.gp, data = mydf1,
                      FUN = function(x) { c(N = sum(x)/n.sims) } )
 sumtable2<- summaryBy(sigfull+sig.half1+replic.12 ~ effsize*totN.per.gp, data = mydf2,
                       FUN = function(x) { c(N = sum(x)/n.sims) } )

 #Now plot power function
 png(paste0("plot_power_comparison.png"), width=600, height=600,res=100)
 plot(sumtable1$sigfull.N~sumtable1$effsize,type='n',
      xlab='Effect size (d)', ylab='power',
      main='Power for single study vs split replication')
 palette("default")
 for (n in 1:n.totalsample){
   w<-which(sumtable1$totN.per.gp==myNs[n]) #loop through each sample size
   tempbit1 <- sumtable1[w,]
   w<-which(sumtable2$totN.per.gp==myNs[n]) #loop through each sample size
   tempbit2 <- sumtable2[w,]
   lines(tempbit1$sigfull.N~tempbit1$effsize,lty=1,col=n)
   lines(tempbit2$sigfull.N~tempbit2$effsize,lty=2,col=n)
   lines(tempbit1$replic.12.N~tempbit1$effsize,lty=3,col=n) 
   
 }
 abline(h=.8,lty=3) #Horizontal line to show .8 power
 legend("topleft", inset=.05, title="N per group (single)",
        c('24','48','96','192'),lty=1,col=1:4,cex=.8)
 legend("bottomright", inset=.05, title="Significance criterion",
        c('one study .05',paste0('one study ',allalpha[2]),'replication .05'),lty=1:3,col=1,cex=.8)
 dev.off()
 
 

