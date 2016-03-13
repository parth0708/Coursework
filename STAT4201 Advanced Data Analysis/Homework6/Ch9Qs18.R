#Question 3
library(lattice)
library(ggplot2)
library(Sleuth3)
attach(ex0918)
dat <- ex0918
datNA <- ex0918[which(ex0918$Continent == "NA"),]
datEU <- ex0918[which(ex0918$Continent == "EU"),]
plot(datNA$Latitude,datNA$Females, xlab = "Latitude", ylab = "Females")
plot(datEU$Latitude,datEU$Females, xlab = "Latitude", ylab = "Females")
plot(datNA$Latitude,datNA$Males, xlab = "Latitude", ylab = "Males")
plot(datEU$Latitude,datEU$Males, xlab = "Latitude", ylab = "Males")

newdat <- matrix(data = NA, nrow = 42, ncol = 4)
newdat <- rbind(dat[,1:2],dat[,1:2])
newdat$Sex[1:21] <- 1
newdat$Sex[22:42] <- 0
newdat$Sex <- as.factor(newdat$Sex)
newdat$WingSize[1:21] <- dat$Females
newdat$WingSize[22:42] <- dat$Males 
model2 <- lm(WingSize ~ Sex*Latitude, data = newdat)
summary(model2)


