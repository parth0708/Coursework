#Question4
sampleMean <- c(30.2,28.8,26.2,31.1,30.2)
sampleSD <- c(3.82,5.26,4.66,4.91,3.53)
n <- rep(9,5)

#Part A
pooledSD <- sqrt(sum((n-1)*sampleSD^2)/((sum(n) - length(n))))

#Part B
coeff <- c(1/3,-1/2,-1/2,1/3,1/3)

#Part C
g <- sum(coeff*sampleMean)
SE <- pooledSD*sqrt(sum((coeff^2)/n))
t <- qt(0.975,40)
CI <- c(g-(t*SE),g+(t*SE))
HW <- SE*t