revenue <- read.csv("Dataset_Revenue_v3.0.csv")
library(DMwR)
knndat <- knnImputation(revenue,5)
write.csv(knndat, "datcompleteknn_v4.0.csv")

####################################################################
revenue <- read.csv("datcompleteknn_v4.0.csv")
revenue <- na.omit(revenue)
revenue1 <- revenue[,-(1:2)]
revenue1[,3:4] <- log(revenue1[,3:4])
boxplot(revenue1[,-1])

dat <- revenue1

#splitting test and train
set.seed(1)
index <- sample(nrow(dat),0.75*nrow(dat),replace = FALSE)
train <- dat[index,]
test <- dat[-index,]
actual <- test$Revenue

#LM
model.lm <- lm(Revenue~.,data = train)
pred.lm <- predict(model.lm,test)
mse.lm <- mean((actual-pred.lm)^2) #2.0055

#Subset Selection
library(leaps)
subset <- regsubsets(Revenue~.,data = dat,nvmax = 25)
which.max(summary(subset)$adjr2)
#Best model: 20 var
rem <- c(15,18,19,20,21,22,26,28)
model.lm.sub <- lm(Revenue~.,data = train[,-rem])
pred.lm.sub <- predict(model.lm.sub,test[,-rem])
mse.lm.sub <- mean((actual-pred.lm.sub)^2) #1.9780

#Lasso
library(glmnet)
cv.out=cv.glmnet(as.matrix(train[,-3]),train$Revenue,alpha=1,lambda=c(0.01,0.1,1,10),nfolds=5)
bestlam=cv.out$lambda.min
model.lasso <- glmnet(as.matrix(train[,-3]),train$Revenue,alpha=1,lambda=bestlam)
pred.lasso <- predict(model.lasso,as.matrix(test[,-3]))
mse.lasso <- mean((actual-pred.lasso)^2) #1.9853

#Ridge
library(glmnet)
ridge.cv.out <- cv.glmnet(as.matrix(train[,-3]),train$Revenue,alpha=0,lambda=c(0.01,0.1,1,10),nfolds=5)
ridge.bestlam <- ridge.cv.out$lambda.min
model.ridge <- glmnet(as.matrix(train[,-3]),train$Revenue,alpha=0,lambda=ridge.bestlam)
pred.ridge <- predict(model.ridge,as.matrix(test[,-3]))
mse.ridge <- mean((actual-pred.ridge)^2) #1.98364

#Elastic Net
elastic.cv.out <- cv.glmnet(as.matrix(train[,-3]),train$Revenue,alpha=0.5,lambda=c(0.01,0.1,1,10),nfolds=5)
elastic.bestlam <- elastic.cv.out$lambda.min
model.elastic <- glmnet(as.matrix(train[,-3]),train$Revenue,alpha=0.5,lambda=elastic.bestlam)
pred.elastic <- predict(model.elastic,as.matrix(test[,-3]))
mse.elastic <- mean((actual-pred.elastic)^2) #1.99344

#Trees
library(tree)
model.trees <- tree(Revenue~., data = train)
pred.tree <- predict(model.trees,test)
mse.tree <- mean((actual-pred.tree)^2) #2.0729
plot(model.trees)
text(model.trees,pretty=0)

#RF
library(randomForest)
model.rf <- randomForest(train[,-3],train[,3])
pred.rf <- predict(model.rf,test)
mse.rf <- mean((actual-pred.rf)^2) #1.5626
varImpPlot(model.rf, n.var = 10, labels = c("Male|Age: Under 18","Male|Age: 35-44",
                                            "Major Production House",
                                            "Female|Age: 35-44",
                                            "Female|Age: 25-34","Male|Age: 45-49",
                                            "Male|Age: 25-34","Female|Age: 18-24",
                                            "Male|Age: 18-24","Budget"))
