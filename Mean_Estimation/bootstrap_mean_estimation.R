#rm(list=ls())

source("bs_mean_fct.R")
source("myRbootmean.R")

R <- 10000

population<-rnorm(n = 1000, mean = -100, sd = 10)
bs_mean <- myRbootmean(population,R,-100)

for (k in c(-80, -50, -25, 0, 25, 50, 80, 100)){
  
  population<-rnorm(n = 1000, mean = k, sd = 10)
  bs_mean <- rbind(bs_mean, myRbootmean(population,R,k))
  print(k)

}



ggplot(bs_mean, aes(x = mu_estim, y = mu)) + geom_errorbar(aes(xmin = TL,xmax = TU)) + geom_abline(slope = 1, intercept = 0) + coord_fixed(xlim = c(-130,130),ylim = c(-130,130))



save(bs_mean, file = "Results_mean.RData")


