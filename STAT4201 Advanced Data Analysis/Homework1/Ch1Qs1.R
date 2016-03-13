#Question1
group <- c(68, 77, 82, 85, 53, 64, 71)

x = t(combn(group,4,FUN= NULL, simplify = TRUE))

y <- matrix(, nrow = 35, ncol = 3)
for (i in 1:35){
  y[i,] = setdiff(group, x[i,])
}

m <- rep(NA,35)
for (i in 1:35){
#  set.seed(i)
#  index = sample(length(group), size = 4)
#  x = group[index]
#  y = group[-index]
#  print(x)
#  print(y)
  m[i] <- abs(mean(x[i,])-mean(y[i,]))
}
ob1 <- c(68,77,82,85)
ob2 <- c(53,64,71)
mean(((mean(ob1)-mean(ob2)<=m)))
