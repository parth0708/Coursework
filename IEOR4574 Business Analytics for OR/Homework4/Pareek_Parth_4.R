#Question 1
attach(USArrests)
dat <- USArrests

#Part a
hc.complete=hclust(dist(dat),method="complete")
plot(hc.complete)

#Part b
clusters = cutree(hc.complete,3)
plot(dat,col=clusters+1,pch=10,lwd=3)
clusters

#Part c
dat.scaled <- scale(dat)
hc.complete.scaled=hclust(dist(dat.scaled),method="complete")
clusters.scaled = cutree(hc.complete.scaled,3)
plot(dat,col=clusters.scaled+1,pch=10,lwd=3)
clusters.scaled

#Question 2
dat <- read.csv("HW4-Q2-data.csv")
true.clusters <- dat$GroupLabel

#Part a
km.out=kmeans(dat[-1],centers=3,nstart=20)
km.out$cluster
true.clusters #clusters match correctly

#Part b
km.out=kmeans(dat[-1],centers=2,nstart=20)
km.out$cluster
true.clusters #clusters 2 and 3 are now part of same cluster

#Part c
km.out=kmeans(dat[-1],centers=4,nstart=20)
km.out$cluster
true.clusters #cluster 3 is now split almost eqully into 2 different clusters

#Part d
km.out=kmeans(scale(dat[-1]),centers=3,nstart=20)
km.out$cluster
true.clusters #same as part a


#Question 3
dat <- read.csv("Hillside-data.csv")

model1 <- lm(X2012.ST ~ Treated, data=dat)
summary(model1)

#Part a
dat_untreated <- subset(dat,Treated == 0)
dat_treated <- subset(dat,Treated == 1)
untreatdiff <- mean(dat_untreated$X2012.ST)-mean(dat_untreated$X2011.ST)
untreatdiffpct <- untreatdiff/mean(dat_untreated$X2011.ST)
treatdiff <- mean(dat_treated$X2012.ST)-mean(dat_treated$X2011.ST)
treatdiffpct <- treatdiff/mean(dat_treated$X2011.ST)
treatdiff - untreatdiff
treatdiffpct - untreatdiffpct

#Part b
model <- lm(X2011.ST~X2010.ST, data = dat)
summary(model)

#Part c
predval <- model$coefficients[1] + (model$coefficients[2]*dat$X2011.ST)
sqrt(sum((predval - dat$X2012.ST)^2)/nrow(dat))

#Part d
treatdiffnew <- mean(predval[which(dat$Treated == 1)] - dat$X2011.ST[which(dat$Treated == 1)])
untreatdiffnew <- mean(predval[which(dat$Treated == 0)] - dat$X2011.ST[which(dat$Treated == 0)])
treatdiffnewpct <- treatdiffnew/mean(dat_treated$X2011.ST)
untreatdiffnewpct <- untreatdiffnew/mean(dat_untreated$X2011.ST)
treatdiffnew - untreatdiffnew
treatdiffnewpct - untreatdiffnewpct









