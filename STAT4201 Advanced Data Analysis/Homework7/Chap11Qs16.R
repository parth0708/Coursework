library(MASS)
library(Sleuth3)
attach(case1101)
dat <- case1101
model <- lm(Metabol ~ Gastric*Sex)
summary(model)

hatvalues(model)[32] #Leverage
studres(model)[32] #Studentized Residual
cooks.distance(model)[32] #Cook's distance