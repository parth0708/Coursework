library(Sleuth3)
attach(case0302)
group1 <- vector(mode="numeric", length = 0)
group2 <- vector(mode="numeric", length = 0)
for (i in 1:nrow(case0302)){
  ifelse(case0302$Veteran[i] == "Vietnam", group1 <- c(case0302$Dioxin[i],group1),group2 <- c(case0302$Dioxin[i],group2))
}
boxplot(group1,group2)
group1 <- sort(group1)
group1_1 <- vector(mode="numeric", length = 0)
group1_2 <- vector(mode="numeric", length = 0)
for (j in 1:(length(group1)-1)){
  group1_1[j]=group1[j]
}
for (k in 1:(length(group1)-2)){
  group1_2[k]=group1[k]
}

t.test(group1_1,group2,alternative = "greater", var.equal=TRUE) #removing one data point
t.test(group1_2,group2,alternative = "greater", var.equal=TRUE) #removing both data points
t.test(group1,group2,alternative = "greater", var.equal=TRUE) #for p-value
t.test(group1,group2, var.equal=TRUE) #for CI


