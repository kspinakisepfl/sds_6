##### we need this to simulate AR from Davies-Harte algorithm
ar.acvs <- function (lag.max, rho, sigma2=1)
{
  lags<-0:lag.max
  sigma2*(rho^lags)
}

##### example
# t<-0
# for (i in 1:1000) {
#   z <- Davies.Harte.sim(2000, ar.acvs, rho=0.75)
#   t<- t+ acf(z)$acf/1000
# }
