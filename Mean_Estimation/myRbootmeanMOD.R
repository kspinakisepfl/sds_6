myRbootmeanMOD <- function(dataset, R){
  
  L <- length(dataset)
  M <- vector(length = R)
  
  for (i in 1:R)
  {
    x <- sample(dataset, L, replace = TRUE)
    M[i] <- mean(x) 
    }
  

  Q <- quantile(M,prob=c(0.025,0.975),names = FALSE)
  
  OUT <- data.frame(mu_estim = mean(M), TL = Q[1], TU = Q[2])
  
  
  return(OUT)
}