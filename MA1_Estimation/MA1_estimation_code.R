source("DaviesHarte.R")
source("ar_acvs.R")

###############################################
### simulate a dataset of size n=200 from AR(1)
###  with phi=0.8

n<-200
data<- arima.sim(n = n, list(ar = c(0.2), sd = sqrt(1)))



model<- arima(data,c(1,0,0))  #fit an AR(1) model to the data

#### save the estimates
phi<-model$coef[1]
sephi<- model$var.coef[1,1]^0.5
sigma<- model$sigma2



B<-1000  # number of bootstrap replications

####bootstrap starts
#### 1. simulate from an AR(1) model - parametric bootstrap

t1<- NULL
for (i in 1:B) {
  bootdata<- arima.sim(n = 200, list(ar = c(phi), sd = sqrt(sigma)))
  modelboot<- arima(bootdata,c(1,0,0))
  phiboot<-modelboot$coef[1]
  t1<-c(t1,phiboot)
}
sd(t1)



#####  2. Davies Harte simulation
t2<- NULL
for (i in 1:B) {
  Dbootdata<- Davies.Harte.sim(n, ar.acvs, rho=phi)
  modelboot2<- arima(Dbootdata,c(1,0,0))
  phiboot2<-modelboot2$coef[1]
  t2<-c(t2,phiboot2)
}
sd(t2)



######  3. Bootstrap with fixed block size
#### fixed blocks, of size 8
len<-8
## create 25 blocks of 8 observations
##
createblocks<-matrix(data, n/len, len, byrow=TRUE)

t3<-NULL
for (i in 1:B ) {
  ind<-sample(1:25,25,replace=TRUE)  ## select rows, i.e. blocks
  newseries<-   createblocks[ind,]  #creates matrix of block samples according to ind
  newseries<- as.vector(t(newseries))  # the default is by column so I transpose to make it by row
  modelboot3<- arima(newseries,c(1,0,0)) #fits AR(1) model
  phiboot3<-modelboot3$coef[1]
  t3<-c(t3,phiboot3)
}
sd(t3)




######  4. overlapping blocks of size 8
len<-8
### create the blocks WITH cycling
temp<-c((1:n),1:(len-1))
blocks<-1:len
for ( i in 2:n) {
  newblock<- temp[i:(i+(len-1))]
  blocks<- rbind(blocks,newblock)
}


####
t4<-NULL
for (i in 1:B ) {
  ind<-sample(1:dim(blocks)[1], ceiling(n/len), replace=TRUE)  ## select rows, i.e. blocks
  ind
  newseries<-   blocks[ind,]
  newseries<- as.vector(t(newseries))  # the default is by column so I transpose to make it by row
  newdata<-data[newseries]
  newdata<-newdata[1:n]
  modelboot4<- arima(newdata,c(1,0,0))
  phiboot4<-modelboot4$coef[1]
  t4<-c(t4,phiboot4)
}
sd(t4)




####percentile 95% conf intervals

quantile(t1,prob=c(0.025,0.975))
quantile(t2,prob=c(0.025,0.975))
quantile(t3,prob=c(0.025,0.975))
quantile(t4,prob=c(0.025,0.975))
### note that true value is 0.80