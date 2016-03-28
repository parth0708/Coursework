library(Sleuth3)
attach(ex1026)
dat <- ex1026
dat$deep <- ifelse(Surface == "Deep", 1,0)
dat <- dat[-3]
model1 <- lm(Inhibit ~ UVB*deep, data = dat)
summary(model1)

model2 <- lm(Inhibit~., data = dat)
summary(model2)

model3 <- lm(Inhibit ~ 0 + UVB + I(deep*UVB), data = dat)
summary(model3)

anova(model3,model1)