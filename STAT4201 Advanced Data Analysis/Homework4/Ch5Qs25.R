#Question2
library(Sleuth3)
attach(ex0525)
dat <- ex0525
boxplot(split(dat,Educ)[[1]]$Income2005,
        split(dat,Educ)[[2]]$Income2005,
        split(dat,Educ)[[3]]$Income2005)
dat$Income_log <- log(dat$Income2005)
boxplot(split(dat,Educ)[[1]]$Income_log,
        split(dat,Educ)[[2]]$Income_log,
        split(dat,Educ)[[3]]$Income_log)
model <- lm(Income_log~Educ, data = dat)
anova(model)
prenewdat <- split(dat,Educ)
newdat <- list()
newdat[[1]] <- prenewdat$"<12"
newdat[[2]] <- prenewdat$"12"
newdat[[3]] <- prenewdat$"13-15"
newdat[[4]] <- prenewdat$"16"
newdat[[5]] <- prenewdat$">16"
meandiff <- data.frame()
CI <- data.frame()
for (i in 1:4){
  meandiff <- rbind(meandiff,
                t.test(newdat[[i]]$Income_log, newdat[[i+1]]$Income_log,
                       var.equal = TRUE)$estimate)
  CI <- rbind(CI,
                t.test(newdat[[i+1]]$Income_log, newdat[[i]]$Income_log,
                           var.equal = TRUE)$conf.int)
}
meandiff$difference <- meandiff[,2] - meandiff[,1]
meandiff$backtrans <- exp(meandiff$difference)
meandiff

CI$Lo <- exp(CI[,1])
CI$Hi <- exp(CI[,2])
CI

