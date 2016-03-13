classify <- function(X,pars){
  j <- pars$j
  t <- pars$t
  f <- sign(X[,j]-t)
  return(f)
}