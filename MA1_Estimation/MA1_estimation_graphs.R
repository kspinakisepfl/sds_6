source("ma_acvs.R")
source("DaviesHarte.R")
source("MA1_estimation_fct.R")
source("MA1_Printer_fct.R")


#here we'll plot theta against theta_hat

ITERS <- 10

temp <- 0
temp <- MA1_estimation_fct(2000, -0.90, 1000, 0.025, 0.975, 20)
print(paste0("j = ", 1))


for (j in 1:(ITERS-1))
{
  temp <- temp + MA1_estimation_fct(2000, -0.90, 1000, 0.025, 0.975, 20)
  print(paste0("j = ", (j+1)))
}


temp <- temp / ITERS
Result <- temp
print(-0.90)


########################################## RESUME WITH SAVED RESULTS FILE, FROM WHATEVER VALUE YOU STOPPED AT

for (k in seq(-0.89,0.90,0.01))
{
  
  temp <- 0
  temp <- MA1_estimation_fct(2000, k, 1000, 0.025, 0.975, 20)
  print(paste0("j = ", 1))
  
  
  for (j in 1:(ITERS-1))
  {
    temp <- temp + MA1_estimation_fct(2000, k, 1000, 0.025, 0.975, 20)
    print(paste0("j = ", (j+1)))
  }
  
  
  temp <- temp / ITERS
  Result <-rbind(Result, temp)
  print(k)
}


save(Result, file = "Results.RData")
