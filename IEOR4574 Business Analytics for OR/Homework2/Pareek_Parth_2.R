library(leaps)
library(gmp)

#Question 1
CollegeData <- read.csv("CollegeData.csv")
CollegeData <- na.omit(CollegeData)

#No. of rows = 1136

#Question 2
#Dollar terms - COSTT4_A, TUITIONFEE_OUT, TUITFTE, AVGFACSAL
CollegeData$COSTT4_A_Log <- log(CollegeData$COSTT4_A)
CollegeData$TUITIONFEE_OUT_Log <- log(CollegeData$TUITIONFEE_OUT)
CollegeData$TUITFTE_Log <- log(CollegeData$TUITFTE)
CollegeData$AVGFACSAL_Log <- log(CollegeData$AVGFACSAL)

#Adding interation terms
CollegeData$IT1 <- as.numeric(mul.bigz(CollegeData$COSTT4_A,CollegeData$TUITIONFEE_OUT))
CollegeData$IT2 <- as.numeric(mul.bigz(CollegeData$COSTT4_A,CollegeData$TUITFTE))
CollegeData$IT3 <- as.numeric(mul.bigz(CollegeData$COSTT4_A,CollegeData$AVGFACSAL))
CollegeData$IT4 <- as.numeric(mul.bigz(CollegeData$TUITIONFEE_OUT,CollegeData$TUITFTE))
CollegeData$IT5 <- as.numeric(mul.bigz(CollegeData$TUITIONFEE_OUT,CollegeData$AVGFACSAL))
CollegeData$IT6 <- as.numeric(mul.bigz(CollegeData$TUITFTE,CollegeData$AVGFACSAL))

meancol <- vector(mode = "numeric", length = 0)
for (i in 2:20){
  meancol[i] <- mean(CollegeData[,i])  
}


#Question3
set.seed(4574)
train <- sample(1:nrow(CollegeData),0.75*nrow(CollegeData))
test = -train
mean(CollegeData[train,]$SAT_AVG)
mean(CollegeData[test,]$SAT_AVG)

#Question4

Data <- CollegeData[,2:20]

#Best Subset Selection using 5-fold Cross Validation
k=5
p=8
set.seed(4574)
Data.train = Data[train,]
folds=sample(1:k,nrow(Data.train),replace=TRUE)
cv.errors=array(NA,dim=c(k,p)) 
for(j in 1:k){
  best.fit=regsubsets(SAT_AVG~.,data=Data.train[folds!=j,],nvmax=p)
  for(t in 1:p){
    pred=predict.regsubsets(best.fit,Data.train[folds==j,],t)
    actual=Data.train$SAT_AVG[folds==j]
    cv.errors[j,t]=mean((actual-pred)^2)
  }
}

mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors

#compute the "best" number of parameters, t*, through minimizing CV MSEs over t=1,...,p (step 3)
best.model = which.min(mean.cv.errors)

#find the best model with t* predictors using entire training dataset (step 4)
regfit.full=regsubsets(SAT_AVG~.,data=Data.train, nvmax=8)

#evaluate MSE of final chosen model on test dataset (step 5)
pred=predict.regsubsets(regfit.full,Data[test,],best.model)
actual = Data$SAT_AVG[test];
mean((actual - pred)^2) #test set MSE

#Best Model: TUITIONFEE_OUT, AVGFACSAL, PFTFAC, 
# C150_4, COSTT4_A_Log, COSTT4_A*TUITIONFEE_OUT
# TUITIONFEE_OUT*AVGFACSAL, TUITFTE*AVGFACSAL

lm(SAT_AVG~TUITIONFEE_OUT + AVGFACSAL + PFTFAC + C150_4 + COSTT4_A_Log 
   + COSTT4_A*TUITIONFEE_OUT + TUITIONFEE_OUT*AVGFACSAL
   + TUITFTE*AVGFACSAL,data=Data.train)


#Question 5
#best lambda = 0.1

library(glmnet)
x=model.matrix(Data$SAT_AVG~.,Data)[,-1]
y=Data$SAT_AVG
grid=c(0,10^(-3:3)) #set sequence of lambdas we want to test

set.seed(4574)
cv.out=cv.glmnet(x[train,],y[train],alpha=1,lambda=grid,nfolds=5) 
bestlam=cv.out$lambda.min

#Train model with best value of lambda on the training set
lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=bestlam)

#Evaluate this model on the test set
pred=predict(lasso.mod,x[test,])
actual = y[test]
mean((actual-pred)^2) 

#Question 6
#MSE (5 fold CV) : 5369.876
#Adj R^2 : 
#MSE (Lasso) : 5301.185
# Best Model: Lasso

round(coef(glmnet(x[train,],y[train],alpha=1,lambda=bestlam)),digits = 8)

