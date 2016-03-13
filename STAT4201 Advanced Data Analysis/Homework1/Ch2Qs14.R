group1 <- c(8,12,10,14,2,0,0)
group2 <- c(-6,0,1,2,-3,-4,2)

g1mean = mean(group1)
g2mean = mean(group2)

g1var = var(group1)
g2var = var(group2)

SD_pooled = sqrt((((length(group1)-1)*g1var) + ((length(group2)-1)*g2var))/(length(group1)+length(group2)-2))
SE = SD_pooled*(sqrt((1/length(group1)) + (1/length(group2))))
n = length(group1) + length(group2)
Diff = g1mean - g2mean

Diff + c(-qt(0.975,df = n-2)*SE,qt(0.975,df = n-2)*SE) # 95% CI
pt(Diff/SE, df = n-2)

t.test(group1-group2, alternative="greater", mu=0)
