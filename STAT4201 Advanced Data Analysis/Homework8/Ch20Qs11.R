library(MASS)
library(Sleuth3)
attach(ex2011)
dat <- ex2011

model1 <- glm(Failure~., data = dat, family = "binomial")
summary(model1) #z-value is -2.054; 2 sided p-value is 0.4 
                #therefore, one-sided would approximately be 0.2

model2 <- glm(Failure~.-1, data = dat, family = "binomial")
anova(model2,model1)

predict(model1, list(Temperature = 31))
(exp(5.564414)-1)/exp(5.564414)