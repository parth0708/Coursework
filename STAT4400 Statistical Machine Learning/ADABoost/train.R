train <- function(X,w,y){
  n <- ncol(X)
  f <- matrix(data = NA, nrow = nrow(X), ncol = ncol(X))
  err.temp <- vector(mode = "numeric",length = ncol(X))
  t.temp <- vector(mode = "numeric",length = ncol(X))
  for (i in 1:ncol(X)){
    f <- function(t,Fx,y,n) sum(w*(sign(Fx-t) != y))/sum(w) 
    err.temp[i]  <- optimize(f, interval = range(X[,i]),maximum = FALSE,
                             n = nrow(X), Fx = X[,i], y = y)$objective
    t.temp[i]  <- optimize(f, interval = range(X[,i]),maximum = FALSE,
                           n = nrow(X), Fx = X[,i], y = y)$minimum
  }    
  err <- min(err.temp)
  j <- min(which(err.temp==err))
  t <- t.temp[j]
  m <- 1
  return(list(j=j,t=t,m=m))
}