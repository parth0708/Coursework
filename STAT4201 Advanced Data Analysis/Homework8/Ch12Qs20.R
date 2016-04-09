library(Sleuth3)
attach(ex1220)
dat <- ex1220
dat$DistSc <- dat$DistSc + 2
dat.native <- dat[,-(1:2)]
dat.native <- log(dat.native)
dat.nonnat <- cbind("NonNative" = (dat[,2] - dat[,3]),dat[,-(1:3)])
dat.nonnat <- log(dat.nonnat)

subset.native <- regsubsets(Native~., data = dat.native, method = "forward")
best.sub.native <- which.max(summary(subset.native)$adjr2)

model1 <- lm(Native~.,data = dat.native)
summary(model1)

model2 <- lm(NonNative~.,data = dat.nonnat)
summary(model2)
