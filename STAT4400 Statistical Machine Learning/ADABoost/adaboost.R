adaboost <- function(trainset,ytrain,B){
  n <- nrow(trainset)
  w <- vector(mode = "numeric",length = nrow(trainset)) #initializing weights
  w[] <- 1/n
  
  trainalpha <- vector(mode="numeric", length = B)
  err1.train <- vector(mode = "numeric")
  allPars <- list()

  for (b in 1:B){
    pars <- train(trainset,w,ytrain)
    allPars <- rbind(pars,allPars)
    f.train <- classify(trainset,pars)
    err1.train[b] <- sum(w*(f.train != ytrain))/sum(w) #error from current classifier
    trainalpha[b] <- findalpha(err1.train[b])
    w <- w*exp(trainalpha[b]*((f.train != ytrain)))
  }
  return(list(allPars = allPars,alpha = trainalpha))
}