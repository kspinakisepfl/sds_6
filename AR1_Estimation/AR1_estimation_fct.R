AR1_estimation_fct <- function(N, phi_actual, B, CIleft, CIright,BBB)
{
  
  ###############################################
  ### simulate a dataset of size n=200 from AR(1)
  ###  with phi=0.8

  

  
  n<-N
  data<- arima.sim(n = n, list(ar = c(phi_actual), sd = sqrt(1)))
  
  
  
  model<- arima(data,c(1,0,0))  #fit an AR(1) model to the data
  
  #### save the estimates
  phi<-model$coef[1]
  sephi<- model$var.coef[1,1]^0.5
  sigma<- model$sigma2
  
  
  
  B<-B  # number of bootstrap replications
  
  ####bootstrap starts
  #### 1. simulate from an AR(1) model - parametric bootstrap
  
  t1<- NULL
  t1 <- foreach (i = 1:B, .combine = 'c') %dopar% {
    
    bootdata<- arima.sim(n = n, list(ar = c(phi), sd = sqrt(sigma)))
    modelboot<- arima(bootdata,c(1,0,0))
    phiboot<-modelboot$coef[1]
  }
  sd(t1)
  
  
  
  #####  2. Davies Harte simulation
  t2<- NULL
  t2 <- foreach (i = 1:B, .combine = 'c') %dopar% {
    source("DaviesHarte.R", local = TRUE)
    source("ar_acvs.R", local = TRUE)

    Dbootdata<- Davies.Harte.sim(n, ar.acvs, rho=phi)
    modelboot2<- arima(Dbootdata,c(1,0,0))
    phiboot2<-modelboot2$coef[1]
  }
  sd(t2)
  
  
  
  ######  3. Bootstrap with fixed block size
  #### fixed blocks, of size 8
  len<-BBB
  ## create 25 blocks of 8 observations
  ##
  createblocks<-matrix(data, n/len, len, byrow=TRUE)
  
  t3<-NULL
  t3 <- foreach (i = 1:B, .combine = 'c') %dopar% {
    
    ind<-sample(1:n/len,n/len,replace=TRUE)  ## select rows, i.e. blocks
    newseries<-   createblocks[ind,]  #creates matrix of block samples according to ind
    newseries<- as.vector(t(newseries))  # the default is by column so I transpose to make it by row
    modelboot3<- arima(newseries,c(1,0,0)) #fits AR(1) model
    phiboot3<-modelboot3$coef[1]
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
  t4 <- foreach (i = 1:B, .combine = 'c') %dopar% {
    ind<-sample(1:dim(blocks)[1], ceiling(n/len), replace=TRUE)  ## select rows, i.e. blocks
    newseries<-   blocks[ind,]
    newseries<- as.vector(t(newseries))  # the default is by column so I transpose to make it by row
    newdata<-data[newseries]
    newdata<-newdata[1:n]
    modelboot4<- arima(newdata,c(1,0,0))
    phiboot4<-modelboot4$coef[1]
  }
  sd(t4)
  
  
  
  
  ####percentile 95% conf intervals
  
  Q1 = quantile(t1,prob=c(CIleft,CIright),names = FALSE)
  Q2 = quantile(t2,prob=c(CIleft,CIright),names = FALSE)
  Q3 = quantile(t3,prob=c(CIleft,CIright),names = FALSE)
  Q4 = quantile(t4,prob=c(CIleft,CIright),names = FALSE)
  


  
  
  b<-data.frame(phi = phi_actual,phi_hat_1 = mean(t1),T1L = Q1[1],T1U = Q1[2],phi_hat_2 = mean(t2),T2L = Q2[1],T2U = Q2[2],phi_hat_3 = mean(t3),T3L = Q3[1],T3U = Q3[2],phi_hat_4 = mean(t4),T4L = Q4[1],T4U = Q4[2])
  
  
  return (b)
}