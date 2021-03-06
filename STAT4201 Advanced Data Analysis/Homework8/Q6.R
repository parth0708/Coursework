library(MASS)
library(leaps)
X <- c(-2,-1,1,2)
Y <- c(0,0,1,1)
dat <- data.frame(X,Y)
model <- glm(Y~X-1,data = dat, family = binomial)
summary(model)