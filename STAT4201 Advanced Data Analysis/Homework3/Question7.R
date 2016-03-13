pval <- vector(mode = "numeric", length = 0)

for(i in 1:10000){
  a <- rnorm(100, mean = 0, sd = 1)
  pval[i] <- t.test(a)$p.value  
}
hist(pval, xlab = "p-values", main = "Histogram of p-values")