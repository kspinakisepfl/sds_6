source("ma_acvs.R")
source("DaviesHarte.R")
source("MODIFIED_MA1_estimation_fct.R")

#here we'll plot theta against theta_hat

ITERS <- 10


temp <- 0
temp <- MODIFIED_MA1_estimation_fct(200, -0.90, 1000, 0.025, 0.975, 50)
print(paste0("j = ", 1))


for (j in 1:(ITERS-1))
{
  temp <- temp + MODIFIED_MA1_estimation_fct(200, -0.90, 1000, 0.025, 0.975, 50)
  print(paste0("j = ", (j+1)))
}


temp <- temp / ITERS
Result <- temp
print(-0.90)


########################################## RESUME WITH SAVED RESULTS FILE, FROM WHATEVER VALUE YOU STOPPED AT

for (k in seq(-0.89,0.90,0.01))
{
  
  temp <- 0
  temp <- MODIFIED_MA1_estimation_fct(200, k, 1000, 0.025, 0.975, 50)
  print(paste0("j = ", 1))
  
  
  for (j in 1:(ITERS-1))
  {
    temp <- temp + MODIFIED_MA1_estimation_fct(200, k, 1000, 0.025, 0.975, 50)
    print(paste0("j = ", (j+1)))
  }
  
  
  temp <- temp / ITERS
  Result <-rbind(Result, temp)
  print(k)
}

ggplot(Result, aes(x = theta, y = theta_hat_3)) + xlab(TeX(r"($\theta$)")) + ylab(TeX(r"($\hat{\theta_3}$)")) + geom_smooth(aes(ymin = T3L,ymax = T3U), fill = "purple", colour = "purple", stat = "identity") + geom_abline(slope = 1, intercept = 0) + coord_fixed(xlim = c(-1,1),ylim = c(-1,1))
ggplot(Result, aes(x = theta, y = theta_hat_4)) + xlab(TeX(r"($\theta$)")) + ylab(TeX(r"($\hat{\theta_4}$)")) + geom_smooth(aes(ymin = T4L,ymax = T4U), fill = "black", colour = "black", stat = "identity") + geom_abline(slope = 1, intercept = 0) + coord_fixed(xlim = c(-1,1),ylim = c(-1,1))

save(Result, file = "Results_MODIFIED.RData")
