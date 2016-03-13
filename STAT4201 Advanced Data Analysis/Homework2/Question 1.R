a <- rnorm(10, mean = 0, sd = 1)
variance <- vector(mode="numeric", length = 0)

mean = 0
s = 1
n = 10

#mu = 0
l1 <- 0
u1 <- ((sqrt(10)*mean(a))/1.96)^2
c(l1,u1)

#mu unknown
l2 <- ((n-1)*1/qchisq(0.975, n-1))
u2 <- ((n-1)*1/qchisq(0.025, n-1))
CI2 <- c(l2,u2)

for (i in 1:10000){
  set.seed(i)
  b <- rnorm(10, , sd = 1)
  variance[i] = var(b)
}

mean(variance <= l1 | variance >= u1)
  
