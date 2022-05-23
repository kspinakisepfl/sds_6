  source("ma_acvs.R")
  source("DaviesHarte.R")
  source("MA1_estimation_fct.R")
  source("MODIFIED_MA1_estimation_fct.R")
  
  #here we'll plot theta against theta_hat
  # AR1_estimation_fct(N, theta_actual, B, CIleft, CIright)
  
  
  Result <-MA1_estimation_fct(2000, -0.9, 1000, 0.025, 0.975, 200)
  for (k in seq(-0.89,0.89,0.01))
  {

    Result <-rbind(Result, MA1_estimation_fct(2000, k, 1000, 0.025, 0.975, 200))
    print(k)
  }
  Result <-rbind(Result, MA1_estimation_fct(2000, 0.9, 1000, 0.025, 0.975, 200))
  
  ggplot(Result, aes(x = theta, y = theta_hat_1)) + xlab(TeX(r"($\theta$)")) + ylab(TeX(r"($\hat{\theta_1}$)")) + geom_smooth(aes(ymin = T1L,ymax = T1U), fill = "blue", colour = "blue", stat = "identity") + geom_abline(slope = 1, intercept = 0) + coord_fixed(xlim = c(-1,1),ylim = c(-1,1))
  ggplot(Result, aes(x = theta, y = theta_hat_2)) + xlab(TeX(r"($\theta$)")) + ylab(TeX(r"($\hat{\theta_2}$)")) + geom_smooth(aes(ymin = T2L,ymax = T2U), fill = "red", colour = "red", stat = "identity") + geom_abline(slope = 1, intercept = 0) + coord_fixed(xlim = c(-1,1),ylim = c(-1,1))
  ggplot(Result, aes(x = theta, y = theta_hat_3)) + xlab(TeX(r"($\theta$)")) + ylab(TeX(r"($\hat{\theta_3}$)")) + geom_smooth(aes(ymin = T3L,ymax = T3U), fill = "purple", colour = "purple", stat = "identity") + geom_abline(slope = 1, intercept = 0) + coord_fixed(xlim = c(-1,1),ylim = c(-1,1))
  ggplot(Result, aes(x = theta, y = theta_hat_4)) + xlab(TeX(r"($\theta$)")) + ylab(TeX(r"($\hat{\theta_4}$)")) + geom_smooth(aes(ymin = T4L,ymax = T4U), fill = "black", colour = "black", stat = "identity") + geom_abline(slope = 1, intercept = 0) + coord_fixed(xlim = c(-1,1),ylim = c(-1,1)) 
  
  save(Result, file = "Results.RData")
