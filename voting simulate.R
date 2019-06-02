#Voting simulation, by DVM Bishop, 2nd June 2019

#Problem: if I have 24 candidates and 12 voters, each of whom can select N candidates,
#I want 6 people selected.
#What should N be to maximise chances of a clear result - assuming everyone just selects 
#N people, unranked, and I just count the number of votes and pick the top scorers.

#The ideal outcome would be if we could put a cutoff on the distribution of total votes achieved
#that would pick out exactly 6.

mydf<-data.frame(matrix(nrow=23, ncol=6))
colnames(mydf)<-c('run','Ncands','Nvoters','Nwanted','Nselect','Success')
ncands<-24
mycands<-1:ncands
nvoters<-12
nwanted<-6
nsim<-5000 #n runs of simulation at each size
thisrow<-0
for (nselect in 2:(ncands-1)){
  thisrow<-thisrow+1
 success<-0
  for (nsim in 1:nsim){
    mycount<-as.vector(rep(0,ncands)) #initialise count for each candidate
    for (i in 1:nvoters){
      thisvote<-sample(mycands,nselect,replace=FALSE)#assume random selection
      mycount[thisvote]<-mycount[thisvote]+1
    }
    myresults<-table(mycount)
    tlen<-length(myresults)
    topcount<-0
    for (t in tlen:1) {#start with most votes
      topcount<-topcount+myresults[t]
      if(topcount==nwanted){ #run where we get exactly nwanted
        success<-success+1
      }
    }
    
  }
  mydf[thisrow,]<-c(thisrow,ncands,nvoters,nwanted,nselect,success/nsim)
}
plot(mydf$Nselect,mydf$Success,type='l',xlab='N selected',ylab='p.success')