moviehit <- read.csv("datcompleteknn_v4.0.csv")
moviehit <- na.omit(moviehit)
moviehit1 <- moviehit[,-(1:2)]
moviehit1$profit<-(moviehit1$Revenue-moviehit1$Budget)/moviehit1$Budget
dat1 <- moviehit1

set.seed(123)
dat1$hit <- ifelse(dat1$profit>=1,1,0)
rem <- c(1,3,29)
dat2 <- dat1[,-rem]
index <- sample(nrow(dat2),0.75*nrow(dat2),replace = FALSE)
train1 <- dat2[index,]
test1 <- dat2[-index,]

#p <- 0.50

library(pROC)

logit.fit = glm(hit ~., data = train1, family = "binomial")
summary(logit.fit)
logit.pred.prob = predict(logit.fit,newdata = test1, type = "response")
p=seq(from=0.1, to= 0.9, by= 0.01)
Acc=c()
for (i in p){
  logit.decision = ifelse(logit.pred.prob > i,1,0)
  logit.table = table(truth=test1$hit, predict=logit.decision)
  #print(i)
  #print(logit.table)
  Accuracy=(logit.table[1]+logit.table[4])/sum(logit.table)
  FPR=logit.table[3]/(logit.table[3]+logit.table[1])
  TPR=logit.table[4]/(logit.table[4]+logit.table[2])
  Acc=c(Acc, Accuracy)
  #print(Accuracy)
  #print(FPR)
  #print(TPR)
  test1$prob=logit.pred.prob
  g <- roc(hit ~ prob, data = test1)
  par(mfrow=c(1, 1))
  plot(g) 
}
#max(Acc)
#which.max(Acc)
p <- p[which.max(Acc)]

#Logistic
logit.fit <- glm(hit~., data = train1, family = "binomial")
summary(logit.fit)
logit.pred.prob <- predict(logit.fit,newdata = test1, type = "response")
logit.decision <- ifelse(logit.pred.prob > p,1,0)
logit.table <- table(truth=test1$hit, predict=logit.decision)
logit.table
FPR.logit <- logit.table[3]/(logit.table[1]+logit.table[3])
DR.logit <- logit.table[4]/(logit.table[3]+logit.table[4])
FPR.logit #0.2193
DR.logit

#Tree
tree.fit <- tree(hit~., data=train1)
tree.pred.prob <- predict(tree.fit,test1)
tree.decision <- ifelse(tree.pred.prob > p,1,0)
tree.table <- table(truth=test1$hit, predict=tree.decision)
tree.table
FPR.tree <- tree.table[3]/(tree.table[1]+tree.table[3])
FPR.tree #0.2456

#RF
rf.fit <- randomForest(as.factor(hit)~., data=train1)
rf.pred <- predict(rf.fit,test1[,-27])
rf.table <- table(truth=test1$hit, predict=rf.pred)
rf.table
FPR.rf <- rf.table[3]/(rf.table[1]+rf.table[3])
FPR.rf #0.1710

varImpPlot(rf.fit)

