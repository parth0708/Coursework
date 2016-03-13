library(Sleuth3)
attach(ex0221)
group1 <- vector(mode="numeric", length = 0)
group2 <- vector(mode="numeric", length = 0)
for (i in 1:nrow(ex0221)){
  ifelse(ex0221$Status[i] == "Perished", group1 <- c(ex0221$Humerus[i],group1),group2 <- c(ex0221$Humerus[i],group2))
}
boxplot(group1,group2)
group1 <- sort(group1)
group1_1 <- vector(mode="numeric", length = 0)
for (j in 2:length(group1)){
  group1_1[j-1]=group1[j]
}

t.test(group1,group2,alternative="less", var.equal=TRUE) #with all observations
t.test(group1_1,group2,alternative="less", var.equal=TRUE) #without lowest observation