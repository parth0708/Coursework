#Question 4
library(lattice)
library(ggplot2)
library(Sleuth3)
attach(ex0920)
dat <- ex0920
#Part a
model1 <- lm (Speed ~ Year + I(Year^2), data = ex0920)
summary(model1)

model2 <- lm (Speed ~ Year + I(Year^2) + Conditions, data = ex0920)
summary(model2)

dat$Conditions <- ifelse(dat$Conditions == "Fast", 1, 0)
model3 <- lm (Speed ~ Year + I(Year^2) + Conditions + I(Starters*Conditions),
              data = dat)
summary(model3)

model4 <- lm (Speed ~ Year + I(Year^2) + Starters*Conditions,
              data = dat)
summary(model4)
