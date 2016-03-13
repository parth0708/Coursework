#Question 2
library(lattice)
library(ggplot2)
library(car)
library(Sleuth3)
attach(ex0327)

#Part a
dat <- ex0327
datQ <- dat[which(dat$BeeType == "Queen"),]
datW <- dat[which(dat$BeeType == "Worker"),]
plot(datQ$DurationOfVisit, datQ$PollenRemoved, pch = 2,
     xlab = "Duration of Visit", ylab = "Pollens Removed",
     xlim = range(0:80), ylim = range(0:1))
points(datW$DurationOfVisit,datW$PollenRemoved)

#Part b
dat.trans <- ex0327
dat.trans$logit <- log(dat$PollenRemoved/(1-dat$PollenRemoved))
#dat.trans$logit <- logit(dat$PollenRemoved)
datQ.trans <- dat.trans[which(dat$BeeType == "Queen"),]
datW.trans <- dat.trans[which(dat$BeeType == "Worker"),]
plot(datQ.trans$DurationOfVisit, datQ.trans$logit, pch = 2,
     xlab = "Duration of Visit", ylab = "Logit",
     xlim = range(0:80), ylim = range(-3:3))
points(datW.trans$DurationOfVisit,datW.trans$logit)

#Part c
plot(log(datQ.trans$DurationOfVisit), datQ.trans$logit, pch = 2,
     xlab = "Log (Duration)", ylab = "Logit",
     xlim = range(0:5), ylim = range(-3:3))
points(log(datW.trans$DurationOfVisit),datW.trans$logit)

#Part d
dat.trans$BeeType <- ifelse(BeeType == "Worker", 1, 0)
model1 <- lm(logit ~ log(DurationOfVisit)*BeeType, data = dat.trans)
summary(model1)

#Part e
model2 <- lm(logit ~ log(DurationOfVisit) + BeeType, data = dat.trans)
summary(model2)
