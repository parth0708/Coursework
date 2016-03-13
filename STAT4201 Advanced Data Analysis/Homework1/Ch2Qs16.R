group1 = c(12.0,12.0,12.9,13.6,16.6,17.2,17.5,18.2,19.1,19.3,19.8,20.3,20.5,20.6,21.3,21.6,22.1,22.2,22.6,23.1,24.0,24.3,26.7,29.7)
group2 = c(5.0,5.4,6.1,10.9,11.8,12.0,12.3,14.8,15.0,16.8,17.2,17.2,17.4,17.5,18.5,18.7,18.7,19.2,19.5,20.7,21.2,22.1,24.0)

g1mean = mean(group1)
g2mean = mean(group2)

g1var = var(group1)
g2var = var(group2)

SD_pooled = sqrt((((length(group1)-1)*g1var) + ((length(group2)-1)*g2var))/(length(group1)+length(group2)-2))
SE = SD_pooled*(sqrt((1/length(group1)) + (1/length(group2))))
n = length(group1) + length(group2)
Diff = g1mean - g2mean

Diff + c(-qt(0.975,df = n-2)*SE,qt(0.975,df = n-2)*SE) # 95% CI

t.test(group1,group2)
