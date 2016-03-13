library(lattice)
library(ggplot2)
library(Sleuth3)
attach(ex0817)
dat <- ex0817
dat_trans <- cbind(dat[1],log(dat[1]),(1/dat[1]),sqrt(dat[1]),
                   dat[2],log(dat[2]),(1/dat[2]),sqrt(dat[2]))
colnames(dat_trans) <- c("Load","Log(Load)","1/Load","sqrt(Load)",
                         "Mass","Log(Mass)","1/Mass","sqrt(Mass)")
pairs(dat_trans,horInd = 1:4,verInd = 5:8)

lm(dat_trans[,6]~dat_trans[,4])
plot(dat_trans[,6],lm(dat_trans[,6]~dat_trans[,4])$residuals, xlab = "Log (Mass)", ylab = "Residuals")
abline(a = 0, b = 0)
