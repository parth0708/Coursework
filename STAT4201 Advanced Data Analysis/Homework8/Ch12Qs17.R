library(Sleuth3)
library(leaps)
attach(ex1217)
dat <- ex1217
dat1 <- dat[,-(15:17)]

subset1 <- regsubsets(Mortality~.,dat1[,-1])
plot(subset1,scale = "Cp")
plot(subset1,scale = "bic")
min(leaps(x=dat1[,-(1:2)],y=dat1[,2])$Cp)

model11 <- lm(Mortality~Precip + JanTemp + JulyTemp + Educ + Density +
                NonWhite, data = dat)
model12 <- lm(Mortality~Precip + JanTemp + JulyTemp + Educ + Density +
                NonWhite + HC + NOX + SO2, data = dat)

anova(model11,model12)

subset2 <- regsubsets(Mortality~.,dat1[,-1])
bestmodel <- which.max(summary(subset2)$adjr2)
plot(subset2,scale = "Cp")
plot(subset2,scale = "bic")
summary(subset2)


model21 <- lm(Mortality~JanTemp + House + Educ + NonWhite, data = dat)
model22 <- lm(Mortality~JanTemp + House + Educ + NonWhite
              + HC + NOX + SO2, data = dat)

anova(model21,model22)


