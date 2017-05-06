
# DVM Bishop 6th May 2017
# Question: what is probability of two independent datasets being closely similar?

# The data in question have been taken from figures using R package digitize
options(scipen=999)
library(scales) #for comma output

# Means, SDs and Ns from 2011 paper
 Means2011 <- c(-0.2826087,-0.05, 0.10869565)
 Ns2011<-c(10,37,27)
 SDs2011<- c(0.481216156, 0.357031701, 0.621279108) 
 #SDs computed by subtracting upper error bar from mean to get SE, then multiplying by sqrt(N)
 
 # Means, SDs and Ns from 2016 paper
 Means2016<- c(-0.27538462, -0.05230769, 0.14307692)
 Ns2016<-c(7, 39,55)
 SDs2016<-c(0.325630922, 0.345876841, 0.376514687)
 
 meandiffs<-abs(Means2011-Means2016) 
 SDdiffs<-abs(SDs2011-SDs2016)
 
# simulate random normal data for each datapoint in 2016 study using mean/SD from 2011
nsims<-10000
myprob<-c(NA,NA,NA) #initialise vector for probabilities for mean similarity
myprob1<-myprob #and vector for probabilities for SD similarity
allsimdat<-NA
for (i in 1:3){
  mycount=0; mycount1<-0 #initialise counter
for (j in 1:nsims) {
 
   simdat<-rnorm(Ns2011[i],Means2011[i],SDs2011[i])
   simmean<-mean(simdat)
   simSD<-sd(simdat)
   thisdiff<-abs(simmean-Means2016[i])
   if(thisdiff<meandiffs[i])
     {mycount<-mycount+1}
   thatdiff<-abs(simSD-SDs2016[i])
   if(thatdiff<SDdiffs[i])
   {mycount1<-mycount1+1}
   if(i==1){
   allsimdat<-c(allsimdat,simdat)#for extra check on simulation
   }
}
  myprob[i]<-mycount/nsims
  myprob1[i]<-mycount1/nsims
}
allprobmeans<-myprob[1]*myprob[2]*myprob[3]
allprobSDs<-myprob1[1]*myprob1[2]*myprob1[3]
overallprob<-allprobmeans*allprobSDs
paste('Overall probability of the data:', overallprob)
paste('This degree of agreement would be seen in one in',comma(round(1000/overallprob,0)),'experiments')
