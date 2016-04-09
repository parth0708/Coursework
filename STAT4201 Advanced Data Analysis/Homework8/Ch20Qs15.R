library(MASS)
library(leaps)
library(Sleuth3)
attach(ex2015)
dat <- ex2015

rr1 <- dat$PctRing1[which(dat$Site == "Random")]
rr2 <- dat$PctRing2[which(dat$Site == "Random")]
rr3 <- dat$PctRing3[which(dat$Site == "Random")]
rr4 <- dat$PctRing4[which(dat$Site == "Random")]
rr5 <- dat$PctRing5[which(dat$Site == "Random")]
rr6 <- dat$PctRing6[which(dat$Site == "Random")]
rr7 <- dat$PctRing7[which(dat$Site == "Random")]
rn1 <- dat$PctRing1[which(dat$Site == "Nest")]
rn2 <- dat$PctRing2[which(dat$Site == "Nest")]
rn3 <- dat$PctRing3[which(dat$Site == "Nest")]
rn4 <- dat$PctRing4[which(dat$Site == "Nest")]
rn5 <- dat$PctRing5[which(dat$Site == "Nest")]
rn6 <- dat$PctRing6[which(dat$Site == "Nest")]
rn7 <- dat$PctRing7[which(dat$Site == "Nest")]

t.test(rr1,rn1, alternative = "less")$p.value
t.test(rr2,rn2, alternative = "less")$p.value
t.test(rr3,rn3, alternative = "less")$p.value
t.test(rr4,rn4, alternative = "less")$p.value
t.test(rr5,rn5, alternative = "less")$p.value
t.test(rr6,rn6, alternative = "less")$p.value
t.test(rr7,rn7, alternative = "less")$p.value

model <- glm(Site~., data = dat, family = "binomial")
model1 <- glm(Site~PctRing1, data = dat, family = "binomial")

R1 <- PctRing1
R2 <- R1+PctRing2
R3 <- R2+PctRing3
R4 <- R3+PctRing4
R5 <- R4+PctRing5
R6 <- R5+PctRing6
R7 <- R6+PctRing7

sub <- regsubsets(dat$Site~R1+R2+R3+R4+R5+R6+R7, data = dat)
summary(sub)
