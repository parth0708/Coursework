#Chap 2 Qs 12
Diff = 280
SE = 46.66
n = 1095
Diff + c(-qt(0.975,df = n-1)*SE,qt(0.975,df = n-1)*SE) # 95% CI
Diff + c(-qt(0.95,df = n-1)*SE,qt(0.95,df = n-1)*SE) # 90% CI
1-pt(Diff/SE, df = n-1)
pts = seq(-5,5,length = 500)
plot(pts,dt(pts, df = n-1),xlab = "x", ylab = "t(x)")
