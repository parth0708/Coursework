#Question6
library(Sleuth3)
attach(ex0623)
anova(lm(WtLoss24~Group,data = ex0623))

dat <- split(ex0623,Group)
boxplot(dat[[1]]$WtLoss24,dat[[2]]$WtLoss24,dat[[3]]$WtLoss24) #data fairly stable, no tranformation needed

dat_test <- aov(ex0623$WtLoss24~ex0623$Group)
dat_anova <- anova(dat_test)
dat_tukey <- TukeyHSD(dat_test)
plot(dat_tukey, las = 0)