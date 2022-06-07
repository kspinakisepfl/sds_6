#rm(list=ls())

source("bs_mean_fct.R")
source("myRbootmeanMOD.R")

R <- 10000
ITERS <- 100
stock <- matrix(data = NA, nrow = ITERS, ncol = 2)
CIcalc <- matrix(data = NA, nrow = ITERS, ncol = 2)



temp <- 0

population<-rnorm(n = 1000, mean = 80, sd = 10)
temp <- myRbootmeanMOD(population, R)

stock[1,1] <- temp$TL
stock[1,2] <- temp$TU


for (i in 1:(ITERS-1)){

  population<-rnorm(n = 1000, mean = 80, sd = 10)
  temp <- myRbootmeanMOD(population, R)
  stock[(i+1),1] <- temp$TL
  stock[(i+1),2] <- temp$TU
  print(i)
}



for (i in 1:ITERS){

  CIcalc[i,1] <- sum(stock[1:i,1]) / i
  CIcalc[i,2] <- sum(stock[1:i,2]) / i
}


plotframe <-data.frame(TL = CIcalc[,1], TU = CIcalc[,2])


ggplot() + 
  geom_smooth(data = plotframe, aes(x = c(1:nrow(plotframe)), y = TL), colour = "darkblue", size  = 1) +
  geom_smooth(data = plotframe, aes(x = c(1:nrow(plotframe)), y = TU), colour = "red", size  = 1) +
  geom_abline(slope = 0, intercept = 80, size = 1, colour = "black") +
  xlab(TeX(r"(Iteration)")) + 
  ylab(TeX(r"($\hat{\mu}$)"))



save(bs_mean, file = "Results_mean.RData")




