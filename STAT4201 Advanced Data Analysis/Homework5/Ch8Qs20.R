library(Sleuth3)
attach(ex0820)
dat <- ex0820

#Part a
plot(DemPctOfMachineVotes[-22],DemPctOfAbsenteeVotes[-22], ylab = "% Absentee Votes", xlab = "% Machine Votes")
points(DemPctOfMachineVotes[22],DemPctOfAbsenteeVotes[22], col= "red")

#Part b
model <- lm(DemPctOfAbsenteeVotes[-22]~DemPctOfMachineVotes[-22])
abline(a = model$coeff[1],b = model$coeff[2])
CIline <- predict(model, interval = "predict")

s1 <- smooth.spline(DemPctOfMachineVotes[-22],CIline[,2])
lines(s1)
s2 <- smooth.spline(DemPctOfMachineVotes[-22],CIline[,3])
lines(s2)

#Part c
preddata <- predict(model, newdata = list(DemPctOfMachineVotes = 49.3),
        interval = "predict")[1]

n = nrow(dat)-1
SSR <- sum(model$residuals^2)
sig_est <- sqrt(SSR/(n-2))
SE_est <- sig_est*sqrt(1 + 1/n + (((49.3-mean(DemPctOfMachineVotes[-22]))^2) / 
                                    ((n-1)*(var(DemPctOfMachineVotes[-22])))))

dev <- (79-preddata)/SE_est
pval <- 2*(1-pt(dev, df = 19))

#Part d
bon_pval <- (n+1)*pval
