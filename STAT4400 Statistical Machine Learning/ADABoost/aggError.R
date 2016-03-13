aggError1 <- function(X,y,alpha,allPars){
  err <- vector(mode = "numeric")
  B <- length(alpha)
  f <- matrix(data = NA, nrow = B, ncol = nrow(X))
  
  f[1,] <- classify(X,allPars[1,])*alpha[1]
  err[1] <- sum(sign(f[1,]) != y)/nrow(X)
  
  for (i in 2:B){
    f[i,] <- classify(X,allPars[i,])*alpha[i] + f[i-1,]
    err[i] <- sum(sign(f[i,]) != y)/nrow(X)    
  }
  return(err)
}