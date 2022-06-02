source("ar_acvs.R")
source("DaviesHarte.R")
source("MODIFIED_AR1_estimation_fct.R")

#here we'll plot phi against phi_hat

ITERS <- 10


temp <- 0
temp <- MODIFIED_AR1_estimation_fct(200, -0.90, 1000, 0.025, 0.975, 10)
print(paste0("j = ", 1))


for (j in 1:(ITERS-1))
{
  temp <- temp + MODIFIED_AR1_estimation_fct(200, -0.90, 1000, 0.025, 0.975, 10)
  print(paste0("j = ", (j+1)))
}


temp <- temp / ITERS
Result <- temp
print(-0.90)


########################################## RESUME WITH SAVED RESULTS FILE, FROM WHATEVER VALUE YOU STOPPED AT

for (k in seq(-0.89,0.90,0.01))
{
  
  temp <- 0
  temp <- MODIFIED_AR1_estimation_fct(200, k, 1000, 0.025, 0.975, 10)
  print(paste0("j = ", 1))
  
  
  for (j in 1:(ITERS-1))
  {
    temp <- temp + MODIFIED_AR1_estimation_fct(200, k, 1000, 0.025, 0.975, 10)
    print(paste0("j = ", (j+1)))
  }
  
  
  temp <- temp / ITERS
  Result <-rbind(Result, temp)
  print(k)
}

ggplot(Result, aes(x = phi, y = phi_hat_3)) + xlab(TeX(r"($\phi$)")) + ylab(TeX(r"($\hat{\phi_3}$)")) + geom_smooth(aes(ymin = T3L,ymax = T3U), fill = "purple", colour = "purple", stat = "identity") + geom_abline(slope = 1, intercept = 0) + coord_fixed(xlim = c(-1,1),ylim = c(-1,1))
ggplot(Result, aes(x = phi, y = phi_hat_4)) + xlab(TeX(r"($\phi$)")) + ylab(TeX(r"($\hat{\phi_4}$)")) + geom_smooth(aes(ymin = T4L,ymax = T4U), fill = "black", colour = "black", stat = "identity") + geom_abline(slope = 1, intercept = 0) + coord_fixed(xlim = c(-1,1),ylim = c(-1,1))

save(Result, file = "Results_MODIFIED.RData")
