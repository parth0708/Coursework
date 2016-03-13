#Question 5
library(Sleuth3)
attach(case0901)
dat <- case0901
dat_test1 <- aov(dat$Flowers~dat$Intensity+dat$Time)
dat_anova1 <- anova(dat_test)

model2 <- lm(Flowers ~ as.factor(Intensity)*Time, data = dat)
dat_test2 <- aov(model2)
dat_anova2 <- anova(dat_test2)