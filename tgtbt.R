
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
nsims<-100
myprobm<-c(NA,NA,NA) #initialise vector for probabilities for mean similarity
myprobs<-myprobm #and vector for probabilities for SD similarity
thiscor<-myprobm

totsim<-matrix(rep(NA,(nsims*2)),nrow=nsims)
for (i in 1:3){
  mycountm=0; mycounts<-0 #initialise counter

    for (j in 1:nsims){
      
    simdat<-rnorm(Ns2016[i],Means2011[i],SDs2011[i]) #simulate one group, 1 expt
    totsim[j,1]<-mean(simdat)
    totsim[j,2]<-sd(simdat)
    thisdiff<-abs(totsim[j,1]-Means2016[i])
    if(thisdiff<meandiffs[i])
    {mycountm<-mycountm+1}
    thatdiff<-abs(totsim[j,2]-SDs2016[i])
    if(thatdiff<SDdiffs[i])
    {mycounts<-mycounts+1}
  }
  myprobm[i]<-mycountm/nsims
  myprobs[i]<-mycounts/nsims
  myc<-cor.test(totsim[,1],totsim[,2])
  thiscor[i]<-myc$estimate #check whether mean and SD are correlated (they aren't)
}

#Stouffer function from Uri Simonsohn
stouffer=function(pp)  sum(qnorm(pp),na.rm=TRUE)/sqrt(sum(!is.na(pp)))

overallprob<-pnorm(stouffer(c(myprobm,myprobs)))
paste('Overall probability of the data:', overallprob)
paste('This degree of agreement would be seen in one in',comma(round(1/overallprob,0)),'experiments')
