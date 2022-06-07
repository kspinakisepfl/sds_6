myRbootmean <- function(dataset, R, k){
  
  L <- length(dataset)
  M <- vector(length = R)
  
  for (i in 1:R)
  {
    x <- sample(dataset, L, replace = TRUE)
    M[i] <- mean(x)
  }
  
  # Mean = mean(M)
  St_Dev = sd(M)
  # OUT <- cbind(Mean, St_Dev)
  
  Q = quantile(M,prob=c(0.025,0.975),names = FALSE)
  
  OUT <- data.frame(mu = k, mu_estim = mean(M), TL = Q[1], TU = Q[2], SE = St_Dev)
  
  
  return(OUT)
}