library(exactRankTests)
library(Sleuth3)
attach(ex0221)
group1 <- vector(mode="numeric", length = 0)
group2 <- vector(mode="numeric", length = 0)
group <- vector(mode="numeric", length = 0)

group = ex0221
group$rank <- rank(group$Humerus, ties.method = c("average"))

for (i in 1:nrow(ex0221)){
  ifelse(group$Status[i] == "Perished", group1 <- c(group$Humerus[i],group1),group2 <- c(group$Humerus[i],group2))
}
wilcox.exact(group1,group2,alternative="two.sided")

#b - uses Normal approximation

rbar <- mean(group$rank)
sr <- sqrt(var(group$rank))
mean_t <- rbar * length(group1)
sd_t <- sr * sqrt((length(group1)*length(group2))/(length(group1)+length(group2)))
qnorm(0.1719/2, mean = mean_t, sd = sd_t)
