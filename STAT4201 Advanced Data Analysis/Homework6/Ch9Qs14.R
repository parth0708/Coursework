#Question 1
library(lattice)
library(ggplot2)
library(Sleuth3)
attach(ex0914)
dat <- ex0914

#Part a
pairs(dat)

#Part b
model <- lm(Heart~.,data = dat)
summary(model)

#Part c
plot(dat$Bank,model$residuals,xlab = "Bank", ylab = "Residuals")
abline(0,0)

plot(dat$Walk,model$residuals,xlab = "Walk", ylab = "Residuals")
abline(0,0)

plot(dat$Talk,model$residuals,xlab = "Talk", ylab = "Residuals")
abline(0,0)