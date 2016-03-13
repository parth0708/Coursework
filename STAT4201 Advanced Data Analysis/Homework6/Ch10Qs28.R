#Question 6
library(Sleuth3)
attach(ex1028)
dat <- ex1028
model1 <- lm(Storms ~ ElNino)
summary(model1)

model2 <- lm(Hurricanes ~ ElNino)
summary(model2)

model3 <- lm(StormIndex ~ WestAfrica*ElNino)
summary(model3)