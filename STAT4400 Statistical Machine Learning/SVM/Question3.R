library(e1071)

#Setting up data sets
uspsdata <- read.table("uspsdata.txt")
uspscl <- read.table("uspscl.txt")

dat <- matrix(unlist(uspsdata), nrow = 200, ncol = 256, byrow = FALSE)
dat = data.frame(x=dat, y=as.factor(uspscl$V1))

#Dividing training and test data sets
train <- sample(1:nrow(dat), 0.8*nrow(dat))
trainset <- dat[train,]
testset <- dat[-train,]

#Finding best model
tune.out <- tune(svm, y~.,data = trainset, kernel = "linear", 
     ranges = list(cost = c(0.0001,0.001, 0.01, 0.1, 1)))
bestmod=tune.out$best.model
summary(bestmod)

ypred=predict(bestmod, testset)
tab <- table(predict=ypred, truth=testset$y)
tab
mismatch_rate = (tab[2]+tab[3])/sum(tab)
mismatch_rate

#RBF Kernel
tune.out <- tune(svm, y~.,data = trainset, kernel = "radial", 
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 10),
                               gamma = c(0.001, 0.01, 0.1, 1, 10)))
summary(tune.out)

bestmod=tune.out$best.model
summary(bestmod)

ypred=predict(bestmod, testset)
tab <- table(predict=ypred, truth=testset$y)
tab
mismatch_rate = (tab[2]+tab[3])/sum(tab)
mismatch_rate

#Function to calculate linear mismatch rate
calc_lin_mismatch <- function(traindata,cost,testset){
  tunefit <- svm(y~.,data = traindata, kernel = "linear", cost = cost)
  ypred=predict(tunefit ,testset)
  tab <- table(predict=ypred, truth=testset$y)
  mismatch_rate = (tab[2]+tab[3])/sum(tab)
  return(mismatch_rate)
}

#Function to calculate non-linear mismatch rate
calc_rbf_mismatch <- function(traindata,cost,gamma,testset){
  tunefit <- svm(y~.,data = traindata, kernel = "radial", 
                 cost = cost, gamma = gamma)
  ypred=predict(tunefit ,testset)
  tab <- table(predict=ypred, truth=testset$y)
  mismatch_rate = (tab[2]+tab[3])/sum(tab)
  return(mismatch_rate)
}

#Plotting mismatch rate (Linear)
linmismatch <- vector (mode = "numeric")
linc = c(0.001, 0.01, 0.1, 1, 10)
for (i in 1:5){
  linmismatch[i] = calc_lin_mismatch(trainset,linc[i],trainset)
}
plot((1/linc),linmismatch,xlab = "1/Cost Parameter", ylab = "Mismatch Rate",
     main = "Linear Kerner Mismatch Rate")


#Plotting mismatch rate (RBF)
#Repeat for individual cost combinations
rbfmismatch <- matrix(data = NA, nrow = 5, ncol = 5)
rbfc = c(0.001, 0.01, 0.1, 1, 10)
rbfg = c(0.001, 0.01, 0.1, 1, 10)
for (j in 1:5){
  for (i in 1:5){
    rbfmismatch[j,i] = calc_rbf_mismatch(trainset,rbfc[i],rbfg[j],trainset)
  }
}

plot((1/rbfc),rbfmismatch[5,],xlab = "1/Cost Parameter",
        ylab = "Mismatch Rate",main = "RBF Kernel Mismatch Rate")


