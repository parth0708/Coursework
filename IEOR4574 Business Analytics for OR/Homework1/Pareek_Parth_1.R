#Question 1
setwd("~/Documents/Acads/02 Spring '16/02 BA for OR/Homework/Assignment 1/EggProduction") #changing current directory
EggProd = read.csv("EggProduction.csv")
EggProd #print data set
summary(EggProd) #display summary of data set

#Question 2
fit1 = lm(eggs~feed, data = EggProd) #regression of eggs with feed
summary(fit1)

#Question 3
fit2 = lm(eggs~feed+temperature, data = EggProd)
summary(fit2)

#Question 4
plot(EggProd$temperature, EggProd$feed, xlab="Temperature", ylab="Feed") #plotting the data

v1 = 1
v2 = 0
EggProd$binary <- ifelse((EggProd$temperature > 0) & (EggProd$temperature < 35) , v2, v1) #create binary variable from temperature
summary(EggProd$binary)

#Question 5
fit3 = lm(eggs~feed + temperature + binary, data = EggProd)
summary(fit3)

#Question 6
set.seed(1)
train = sample(1:nrow(EggProd),0.5*nrow(EggProd))
train
attach(EggProd)
fit_train1 = lm(eggs~feed, data = EggProd[train,])
fit_train2 = lm(eggs~feed + temperature, data = EggProd[train,])
fit_train3 = lm(eggs~feed + temperature + binary, data = EggProd[train,])
cat("R-squared for Model 1: ", summary(fit_train1)$r.squared)
cat("R-squared for Model 2: ", summary(fit_train2)$r.squared)
cat("R-squared for Model 3: ", summary(fit_train3)$r.squared)
#test = -train
#test1.pred = predict(fit_train1,newdata = EggProd[test,])
#test2.pred = predict(fit_train2,newdata = EggProd[test,])
#test3.pred = predict(fit_train3,newdata = EggProd[test,])
#tss = sum((EggProd$eggs[test]-mean(EggProd$eggs[test]))^2)
#rss1 = sum((EggProd$eggs[test]-test1.pred)^2)
#rss2 = sum((EggProd$eggs[test]-test2.pred)^2)
#rss3 = sum((EggProd$eggs[test]-test3.pred)^2)
#rsq1 = 1 - rss1/tss
#rsq2 = 1 - rss2/tss
#rsq3 = 1 - rss3/tss
#rsq1
#rsq2
#rsq3

#Question 7
CI_99 = confint(fit_train3, level = 0.99) #99% confidence interval for regression coefficients
CI_99

#Question 8
predict(fit_train3, newdata=list(feed = 25, temperature = -1, binary = 1), interval = "predict", level = 0.90)

