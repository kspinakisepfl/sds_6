source("ar_acvs.R")
source("DaviesHarte.R")
source("AR1_estimation_fct.R")

#here we'll plot phi against phi_hat

ITERS <- 10

temp <- 0
temp <- AR1_estimation_fct(200, -0.90, 1000, 0.025, 0.975, 20)
print(paste0("j = ", 1))


for (j in 1:(ITERS-1))
{
  temp <- temp + AR1_estimation_fct(200, -0.90, 1000, 0.025, 0.975, 20)
  print(paste0("j = ", (j+1)))
}


temp <- temp / ITERS
Result <- temp
print(-0.90)


########################################## RESUME WITH SAVED RESULTS FILE, FROM WHATEVER VALUE YOU STOPPED AT

for (k in seq(-0.89,0.90,0.01))
{
  
 temp <- 0
 temp <- AR1_estimation_fct(200, k, 1000, 0.025, 0.975, 20)
 print(paste0("j = ", 1))
 
 
 for (j in 1:(ITERS-1))
  {
    temp <- temp + AR1_estimation_fct(200, k, 1000, 0.025, 0.975, 20)
    print(paste0("j = ", (j+1)))
 }
 
 
 temp <- temp / ITERS
 Result <-rbind(Result, temp)
 print(k)
}


ggplot(Result, aes(x = phi, y = phi_hat_1)) + xlab(TeX(r"($\phi$)")) + ylab(TeX(r"($\hat{\phi_1}$)")) + geom_smooth(aes(ymin = T1L,ymax = T1U), fill = "blue", colour = "blue", stat = "identity") + geom_abline(slope = 1, intercept = 0) + coord_fixed(xlim = c(-1,1),ylim = c(-1,1))
ggplot(Result, aes(x = phi, y = phi_hat_2)) + xlab(TeX(r"($\phi$)")) + ylab(TeX(r"($\hat{\phi_2}$)")) + geom_smooth(aes(ymin = T2L,ymax = T2U), fill = "red", colour = "red", stat = "identity") + geom_abline(slope = 1, intercept = 0) + coord_fixed(xlim = c(-1,1),ylim = c(-1,1))
ggplot(Result, aes(x = phi, y = phi_hat_3)) + xlab(TeX(r"($\phi$)")) + ylab(TeX(r"($\hat{\phi_3}$)")) + geom_smooth(aes(ymin = T3L,ymax = T3U), fill = "purple", colour = "purple", stat = "identity") + geom_abline(slope = 1, intercept = 0) + coord_fixed(xlim = c(-1,1),ylim = c(-1,1))
ggplot(Result, aes(x = phi, y = phi_hat_4)) + xlab(TeX(r"($\phi$)")) + ylab(TeX(r"($\hat{\phi_4}$)")) + geom_smooth(aes(ymin = T4L,ymax = T4U), fill = "black", colour = "black", stat = "identity") + geom_abline(slope = 1, intercept = 0) + coord_fixed(xlim = c(-1,1),ylim = c(-1,1))

save(Result, file = "Results.RData")
