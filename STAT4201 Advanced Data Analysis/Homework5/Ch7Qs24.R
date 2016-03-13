library(Sleuth3)
attach(ex0724)
dat <- ex0724
summary(lm(dat$Denmark~dat$Year))
summary(lm(dat$Netherlands~dat$Year))
summary(lm(dat$Canada~dat$Year))
summary(lm(dat$USA~dat$Year))