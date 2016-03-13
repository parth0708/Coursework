#Question3
library(Sleuth3)
attach(case0601)
dat <- split(case0601,Handicap)

#Planned Comparison
sampleMean <- c(mean(dat[[1]]$Score),mean(dat[[2]]$Score),
                mean(dat[[3]]$Score),mean(dat[[4]]$Score),
                mean(dat[[5]]$Score))
sampleSD <- c(sd(dat[[1]]$Score),sd(dat[[2]]$Score),
              sd(dat[[3]]$Score),sd(dat[[4]]$Score),
              sd(dat[[5]]$Score))
n <- c(nrow(dat[[1]]),nrow(dat[[2]]),nrow(dat[[3]]),
       nrow(dat[[4]]),nrow(dat[[5]]))
coeff <- c(1/3,1/3,-1,0,1/3)

pooledSD <- sqrt(sum((n-1)*sampleSD^2)/((sum(n) - length(n))))
g <- sum(coeff*sampleMean)
SE <- pooledSD*sqrt(sum((coeff^2)/n))
t <- g/SE
pval <- 2*(1-pt(t,65))

mobility <- rbind(dat[[1]],dat[[2]],dat[[5]])
comm <- dat[[3]]
t.test(mobility$Score,comm$Score,var.equal = TRUE)

#Tukey HSD
dat_test <- aov(case0601$Score~case0601$Handicap)
dat_anova <- anova(dat_test)
dat_tukey <- TukeyHSD(dat_test)
