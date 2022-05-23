MA1_estimation_fct <- function(N, theta_actual, B, CIleft, CIright,BBB)
{
  ###############################################
  ### simulate a dataset of size n=200 from MA(1)
  ###  with theta=0.8
  
  
  
  n<-N
  data<- arima.sim(n = n, list(ma = c(theta_actual), sd = sqrt(1)))
  
  
  
  model<- arima(data,c(0,0,1))  #fit an AR(1) model to the data
  
  #### save the estimates
  theta<-model$coef[1]
  setheta<- model$var.coef[1,1]^0.5
  sigma<- model$sigma2
  
  
  
  B<-B  # number of bootstrap replications
  
  ####bootstrap starts
  #### 1. simulate from an AR(1) model - parametric bootstrap
  
  t1<- NULL
  for (i in 1:B) {
    
    bootdata<- arima.sim(n = 200, list(ma = c(theta), sd = sqrt(sigma)))
    modelboot<- arima(bootdata,c(0,0,1))
    thetaboot<-modelboot$coef[1]
    t1<-c(t1,thetaboot)
  }
  sd(t1)
  
  
  
  #####  2. Davies Harte simulation
  t2<- NULL
  for (i in 1:B) {
    
    Dbootdata<- Davies.Harte.sim(n, ma.acvs, rho=theta)
    modelboot2<- arima(Dbootdata,c(0,0,1))
    thetaboot2<-modelboot2$coef[1]
    t2<-c(t2,thetaboot2)
  }
  sd(t2)
  
  
  
  ######  3. Bootstrap with fixed block size
  #### fixed blocks, of size 8
  len<-BBB
  ## create 25 blocks of 8 observations
  ##
  createblocks<-matrix(data, n/len, len, byrow=TRUE)
  
  t3<-NULL
  for (i in 1:B ) {
    
    ind<-sample(1:n/len,n/len,replace=TRUE)  ## select rows, i.e. blocks
    newseries<-   createblocks[ind,]  #creates matrix of block samples
    newseries<- as.vector(t(newseries))  # the default is by column so I
    modelboot3<- arima(newseries,c(0,0,1)) #fits AR(1) model
    thetaboot3<-modelboot3$coef[1]
    t3<-c(t3,thetaboot3)
  }
  sd(t3)
  
  
  
  
  ######  4. overlapping blocks of size 8
  len<-BBB
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
    ind<-sample(1:dim(blocks)[1], ceiling(n/len), replace=TRUE)  ##
    newseries<-   blocks[ind,]
    newseries<- as.vector(t(newseries))  # the default is by column so I
    newdata<-data[newseries]
    newdata<-newdata[1:n]
    modelboot4<- arima(newdata,c(0,0,1))
    thetaboot4<-modelboot4$coef[1]
    t4<-c(t4,thetaboot4)
  }
  sd(t4)
  
  
  
  
  ####percentile 95% conf intervals
  
  Q1 = quantile(t1,prob=c(CIleft,CIright),names = FALSE)
  Q2 = quantile(t2,prob=c(CIleft,CIright),names = FALSE)
  Q3 = quantile(t3,prob=c(CIleft,CIright),names = FALSE)
  Q4 = quantile(t4,prob=c(CIleft,CIright),names = FALSE)
  
  
  
  b<-data.frame(theta = theta_actual,theta_hat_1 = mean(t1),T1L = Q1[1],T1U =
                  Q1[2],theta_hat_2 = mean(t2),T2L = Q2[1],T2U = Q2[2],theta_hat_3 =
                  mean(t3),T3L = Q3[1],T3U = Q3[2],theta_hat_4 = mean(t4),T4L = Q4[1],T4U =
                  Q4[2])
  
  
  return (b)
}