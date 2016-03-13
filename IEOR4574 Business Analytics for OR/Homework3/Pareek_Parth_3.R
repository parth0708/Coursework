library(MASS)
library(tree)
library(e1071)
#Question 1
#Part a
dat <- read.csv("OrangeJuice.csv")
set.seed(4754)
train <- sample(1:nrow(dat),0.5*nrow(dat))
dat.train <- dat[train,] #Training set
temp = -train
dat.temp <- dat[-train,]
valid <- sample(1:nrow(dat.temp),0.5*nrow(dat.temp))
dat.valid <- dat.temp[valid,] #Validation set
dat.test <- dat.temp[-valid,] #Test set
summary(dat.train)

#Part b
logit.fit = glm(Purchase~.-X, data = dat.train, family = "binomial")
summary(logit.fit)

#Part c
logit.fit1 = glm(Purchase~PriceMM+LoyalCH, data = dat.train, 
                 family = "binomial")
summary(logit.fit1)

#Part d
dat.tree <- tree(Purchase~.-X,data = dat.train)
summary(dat.tree)
plot(dat.tree)
text(dat.tree,pretty=1)

#Part e
c <- c(0.01,0.1,1,10,100) #Best model has cost = 0.1
for(i in 1:5){
  cat("Cost: ",c[i], "\n")
  svm.mod <- svm(Purchase~.-X,data = dat.train,cost = c[i])
  pred <- predict(svm.mod,dat.valid)
  tab <- table(pred=pred,truth=dat.valid$Purchase)
  print(tab)
  misclass = (tab[2]+tab[3])/sum(tab)
  cat("Misclassification Rate: ", misclass, "\n\n")
}

#Part f
svm.bestmod <- svm(Purchase~.-X,data = dat.train,cost = 0.1)
pred.svm.valid <- predict(svm.bestmod,dat.valid)
tab <- table(pred=pred.svm.valid,truth=dat.valid$Purchase)
misclass_svm.valid = (tab[2]+tab[3])/sum(tab)

pred.tree.valid <- predict(dat.tree,newdata = dat.valid, type = "class")
tab <- table(pred=pred.tree.valid,truth=dat.valid$Purchase)
misclass_tree.valid = (tab[2]+tab[3])/sum(tab)

prob.logit.valid <- predict(logit.fit1,dat.valid, type = "response")
p = 0.50;
pred.logit.valid = ifelse(prob.logit.valid > p,1,0)
tab <- table(pred=pred.logit.valid,truth=dat.valid$Purchase)
misclass_logit.valid = (tab[2]+tab[3])/sum(tab)

misclass_logit.valid #0.157
misclass_tree.valid #0.184
misclass_svm.valid #0.169

#Part g
pred.svm.test <- predict(svm.bestmod,dat.test)
tab <- table(pred=pred.svm.test,truth=dat.test$Purchase)
misclass_svm.test = (tab[2]+tab[3])/sum(tab)

pred.tree.test <- predict(dat.tree,newdata = dat.test, type = "class")
tab <- table(pred=pred.tree.test,truth=dat.test$Purchase)
misclass_tree.test = (tab[2]+tab[3])/sum(tab)

prob.logit.test <- predict(logit.fit1,dat.test)
p = 0.50;
pred.logit.test = ifelse(prob.logit.test > p,1,0)
tab <- table(pred=pred.logit.test,truth=dat.test$Purchase)
misclass_logit.test = (tab[2]+tab[3])/sum(tab)

misclass_logit.test #0.23
misclass_tree.test #0.22
misclass_svm.test #0.19

#Part h
#Pred: CH Truth: CH -- Profit: $3
#Pred: CH Truth: MM -- Profit: -$1
#Pred: MM Truth: CH -- Profit: $0
#Pred: MM Truth: MM -- Profit: $0
cost.matrix=matrix(c(-3,1,0,0),nrow=2,ncol=2,byrow=TRUE)
possible.ps = seq(from=0.10,to=0.90,by=0.01)
costs = rep(0, times=length(possible.ps))
for (i in 1:length(possible.ps)) {
  p = possible.ps[i]
  logit.decision = ifelse(prob.logit.test > p,1,0)
  logit.table = table(truth=dat.test$Purchase, predict=logit.decision)
  costs[i] = sum(logit.table * cost.matrix)
}
plot(possible.ps,costs)
#best decision threshold
logit.best.p = possible.ps[which.min(costs)]
#minimum cost corresponding to best.p
logit.min.cost = min(costs)

#-------------------------------------------------------------------------

#Question 2
#Part a
dat1 <- read.csv("classifyMe.csv") #all data
dat2 <- dat1[-241,] #removing outlier
summary(dat1)
summary(dat2)

#Part b
lda1 <- lda(y~X1+X2, data = dat1)
lda2 <- lda(y~X1+X2, data = dat2)
lda1
lda2

#Part c
tune1.out <- tune(svm, y~X1+X2,data = dat1, kernel = "linear",
         range = list(cost = c(0.1,1,10,100,1000)),
         tunecontrol=tune.control(sampling=c("cross"),cross=10))
svm1 = tune1.out$best.model
tune1.out <- tune(svm, y~X1+X2,data = dat2, kernel = "linear",
                  range = list(cost = c(0.1,1,10,100,1000)),
                  tunecontrol=tune.control(sampling=c("cross"),cross=10))
svm2 = tune1.out$best.model
svm1
svm2

#Part d
dat.valid <- read.csv("classifyMeValidation.csv")
corr_rate <- matrix(data = c("LDA (All)", "LDA (No Outlier)",
                    "SVM (All)", "SVM (No Outlier)"), nrow = 4, ncol = 2,
                    byrow = FALSE)
pred <- predict(lda1,dat.valid)
tab <- table(pred = pred$class, truth =dat.valid$y)
corr_rate[1,2] <- (tab[1]+tab[4])/sum(tab)

pred <- predict(lda2,dat.valid)
tab <- table(pred = pred$class, truth =dat.valid$y)
corr_rate[2,2] <- (tab[1]+tab[4])/sum(tab)

pred <- predict(svm1,dat.valid)
tab <- table(pred = pred, truth =dat.valid$y)
corr_rate[3,2] <- (tab[1]+tab[4])/sum(tab)

pred <- predict(svm2,dat.valid)
tab <- table(pred = pred, truth =dat.valid$y)
corr_rate[4,2] <- (tab[1]+tab[4])/sum(tab)

#LDA Correct Rate (outlier) = 0.6667
#LDA Correct Rate (no outlier) = 0.7833
#SVM Correct Rate (outlier) = 0.75
#SVM Correct Rate (no outlier) = 0.7677

#-------------------------------------------------------------------------

#Question 3
#Part a
library(class)
dat <- read.csv("cuisinePreferences.csv")
trainset <- dat[which(dat$Section == 1),1:19]
testset <- dat[which(dat$Section == 2),1:19]
names1 <- trainset[,1]
names2 <- testset[,1]

trainset <- as.matrix(trainset[,-1])
testset <- as.matrix(testset[,-1])

#Part b
findsim <- function(X){
  n = nrow(X)
  c = ncol(X)
  sim = matrix(NA,nrow = n, ncol = n) 
  for (i in 1:n)
  {
    for (j in 1:n)
    {
      d = trainset[i,] - trainset[j,]
      sim[i,j] = sqrt(sum(d*d, na.rm = TRUE))
    }
  }
  return(sim)
}

sim.train <- findsim(trainset)
rownames(sim.train) <- names1
colnames(sim.train) <- names1
clranking <- sort(sim.train[,"Parth Pareek"])
names(clranking[2:6])

#Part c
PredictRatings <- function(nn,X,sim){
  n = nrow(X)
  c = ncol(X)
  predictionMatrix = matrix(NA, nrow = n, ncol = c)
  
  for (u in 1:n)
  { 
    orderedCuisineRanks = X[order(sim[u,]),]
    for (cui in 1:c)
    {
      ratings = na.omit(orderedCuisineRanks[2:n,cui])
      ratings = as.vector(ratings)
      if (length(ratings) > nn )
        predictionMatrix[u,cui] = mean(ratings[1:nn])
      else
        predictionMatrix[u,cui] = mean(ratings)
    }
  }
  return(predictionMatrix)
}

predictionMat <- PredictRatings(3,trainset,sim.train)
colnames(predictionMat) = colnames(trainset)
row.names(predictionMat) = names1
  for (u in 1:nrow(trainset))
    for (cui in 1:ncol(trainset))
      if (!is.na(trainset[u,cui]))
        predictionMat[u,cui] = NA

#Part d
RMSEperformance = vector(length = 20)
for (nn in 1:20){
  predictionMatrix = PredictRatings(nn,trainset,sim.train)
  RMSEperformance[nn] = sqrt(mean((predictionMatrix - trainset)^2,
                                  na.rm = TRUE))
}

plot(1:20, RMSEperformance, xlab = "Nearest Neighbours") # plot the result

#Part e
sim.test <- findsim(testset)
rownames(sim.test) <- names2
colnames(sim.test) <- names2
predictTest <- PredictRatings(4,testset,sim.test)
colnames(predictTest) = colnames(testset)
row.names(predictTest) = names2

RMSEtest = sqrt(mean((predictTest[1:3,] - testset[1:3,])^2, na.rm = TRUE))
