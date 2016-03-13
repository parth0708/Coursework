#Parth Pareek
#UNI: PP2547
#Class: STAT4400
#Assignment: HW3

#Question 1

library(e1071)

############## FUNCTIONS ##############
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

classify <- function(X,pars){
  j <- pars$j
  t <- pars$t
  f <- sign(X[,j]-t)
  return(f)
}

findalpha <- function(err) return(log((1-err)/err)) #function to find alpha

aggError <- function(X,y,alpha,allPars){
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

############## CODE ##############

#Setting up data sets
uspsdata <- read.table("uspsdata.txt")
uspscl <- read.table("uspscl.txt")

dat <- matrix(unlist(uspsdata), nrow = 200, ncol = 256, byrow = FALSE)
y=as.factor(uspscl$V1)

#Randomly shuffle the data
index <- sample(nrow(dat))
dat <- dat[index,]
y <- y[index]

#Create 5 equally size folds
folds <- cut(seq(1,nrow(dat)),breaks=5,labels=FALSE)

B = 10
trainErr <- matrix(data = NA, nrow = 5, ncol = B)
testErr <- matrix(data = NA, nrow = 5, ncol = B)

#Perform 5 fold cross validation
for(i in 1:5){
  #Segement your data by fold using the which() function 
  testIndex <- which(folds==i,arr.ind=TRUE)
  testset <- dat[testIndex, ]
  ytest <- y[testIndex]
  trainset <- dat[-testIndex, ]
  ytrain <- y[-testIndex]
  
  ada <- adaboost(trainset,ytrain,B)
  allPars <- ada$allPars
  alpha <- ada$alpha
  
  trainErr[i,] <- aggError(trainset,ytrain,alpha,allPars)
  testErr[i,] <- aggError(testset,ytest,alpha,allPars)
}

mean.train.err <- vector(mode = "numeric", length = B)
mean.test.err <- vector(mode = "numeric", length = B)

for (j in 1:B){
  mean.train.err[j] <- mean(trainErr[,j])
  mean.test.err[j] <- mean(testErr[,j])
}

plot(seq(1,B,1),mean.train.err)
points(seq(1,B,1),mean.test.err)


