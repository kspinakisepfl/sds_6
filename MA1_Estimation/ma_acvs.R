ma.acvs<-function (lag.max, rho, sigma2=1)
  {
    lags<-0:lag.max
    c(1,rho/(1+rho^2),rep(0,lag.max-1))
  }