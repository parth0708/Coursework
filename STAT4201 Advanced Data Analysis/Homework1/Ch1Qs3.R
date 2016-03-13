# Question 3
group1 <- c(1.31, 1.45, 1.12, 1.16, 1.30, 1.50, 1.20, 1.22, 1.42, 1.14, 1.23, 1.59, 1.11, 1.10, 1.53, 1.52, 1.17, 1.49, 1.62, 1.29)
group2 <- c(1.13, 1.71, 1.39, 1.15, 1.33, 1.00, 1.03, 1.68, 1.76, 1.55, 1.34, 1.47, 1.74, 1.74, 1.19, 1.15, 1.20, 1.59, 1.47)
combined <- c(group1,group2)
boxplot(group1,group2)

#Question4
#Null Hypothesis H0: mu(group1) = mu(group2)
#Alternative Hypothesis H1: mu(group1) != mu(group2)

m <- rep(NA,1000)
for (i in 1:1000){
#  set.seed(i)
  index = sample(length(combined), size = length(group1))
  x = combined[index]
  y = combined[-index]
  m[i] <- abs(mean(y)-mean(x))
}

obs_mean = abs(mean(group1)-mean(group2))

mean((obs_mean<=m))

#p-value is very high
#Cannot reject Null Hypothesis
