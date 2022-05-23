#rm(list=ls())

source("bs_mean_fct.R")
source("myRbootmean.R")

R <- 10000

population<-rnorm(n = 1000, mean = -10, sd = 0.5)
bs_mean <- myRbootmean(population,R,-10)

for (k in c(0,8)){
  
  population<-rnorm(n = 1000, mean = k, sd = 0.5)
  bs_mean <- rbind(bs_mean, myRbootmean(population,R,k))
  print(k)

}

ggplot(bs_mean, aes(x = mu, y = mu_estim)) + geom_errorbar(aes(ymin = TL,ymax = TU)) + geom_abline(slope = 1, intercept = 0)



save(bs_mean, file = "Results_mean.RData")


