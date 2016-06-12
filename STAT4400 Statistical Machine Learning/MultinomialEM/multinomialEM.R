MultinomialEM <- function(H,K,tau){
  Kmat <- H[(sample(nrow(H),K, replace = FALSE)),] + 0.01
  t <- Kmat
#  t <- scale(Kmat) + 0.01
#  t <- t(scale(t(Kmat)))
  phi <- exp(H %*% log(t(t)))
  c <- rep((1/K),K)
  A <- (c*phi)/(rowSums(c*phi))
  delta = tau+1000
  while(delta > tau){
    #M-Step
    c <- colSums(A)/nrow(H)
    b <- t(A) %*% H
    t <- b/(rowSums(b))
    #E-Step
    A.old <- A
    phi <- exp(H %*% log(t(t)))
    A <- (c*phi)/(rowSums(c*phi))
    delta <- norm((A-A.old),'O')
  }
  m <- apply(A,1,which.max)
  return(m)
}

plotem <- function(m){
  z <- matrix(unlist(m), nrow = sqrt(length(m)), byrow = F)
  image(x = 1:sqrt(length(m)), y = 1:sqrt(length(m)), z = z,col = gray(z/5))
}

H <- matrix(readBin("histograms.bin", "double", 640000), 40000, 16)
K <- 5
tau <- 1
m <- MultinomialEM(H,K,tau)
plotem(m)