library(lattice)
library(ggplot2)
library(Sleuth3)
attach(case0902)
dat <- case0902
dat_trans <- log(dat[,2:5])
pairs(dat_trans, horInd = 2:4, verInd = c(3,4,1),diag.panel = NULL)

model <- lm(Brain~.,data = dat_trans)
summary(model)

newdat <- data.frame(dat_trans[1:3],dat[5])
pairs(newdat, horInd = 2:4, verInd = c(3,4,1),diag.panel = NULL)