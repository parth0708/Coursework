library(Sleuth3)
attach(ex0223)

group1 <- vector(mode="numeric", length = 0)
group2 <- vector(mode="numeric", length = 0)
for(i in 1:51){
  ifelse(ex0223$SpeedLimit[i] == "Inc", group1 <- c(ex0223$PctChange[i],group1), group2 <- c(ex0223$PctChange[i],group2))
}

g1mean = mean(group1)
g2mean = mean(group2)
n = length(group1) + length(group2)
Diff = g2mean - g1mean
Diff + c(-qt(0.975,df = n-1)*SE,qt(0.975,df = n-1)*SE) # 95% CI

t.test(group1,group2, equal.var="TRUE", alternative="greater")
