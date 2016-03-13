library(Sleuth3)
attach(ex0728)
dat <- ex0728
control <- dat[which(Years == "0"),]
test <- dat[which(Years != "0"),]
t.test(test$Activity,control$Activity,var.equal = TRUE, alternative = "greater") #for p-value
t.test(test$Activity,control$Activity,var.equal = TRUE) #for CI

lm(Activity~Years,data = dat)
plot(dat)
abline(a = lm(Activity~Years,data = dat)$coeff[1],
        b = lm(Activity~Years,data = dat)$coeff[2])