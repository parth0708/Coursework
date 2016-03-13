#Question1
library(Sleuth3)
attach(ex0523)
dat <- ex0523
anova(lm(data = dat))