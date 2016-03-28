library(MASS)
library(Sleuth3)
attach(ex1120)
dat <- ex1120
dat1 <- dat[-1,]
dat2 <- dat1[-1,]

#Part a
model <- lm(Calcite ~., data = dat)
model1 <- lm(Calcite ~., data = dat1)
model2 <- lm(Calcite ~., data = dat2)

#Part c
max(hatvalues(model)) #Leverage
which(hatvalues(model) == max(hatvalues(model))) #Index = 1
max(studres(model)) #Studentized Residual
which(studres(model) == max(studres(model))) #Index = 10
max(cooks.distance(model)) #Cook's distance
which(cooks.distance(model) == max(cooks.distance(model))) #Index = 1

#Part d
max(hatvalues(model1)) #Leverage
which(hatvalues(model1) == max(hatvalues(model1))) #Index = 2
min(studres(model1)) #Studentized Residual
which(studres(model1) == min(studres(model1))) #Index = 2
max(cooks.distance(model1)) #Cook's distance
which(cooks.distance(model1) == max(cooks.distance(model1))) #Index = 2

