#Question5
#ANOVA F-test with p-value = 0:0850
#n=36; I=6
#For 95% CI, alpha = 0.025

#LSD
qt(0.975,30)

#F Protected - no change, since p-value > 0.05
qt(0.975,30)

#Tukey Kramer
qtukey(0.95,6,30)/sqrt(2)

#Bonferroni
k <- 6*5/2
qt(1-(0.05/(k*2)),30)

#Scheffe
sqrt(5*qf(0.95,5,30))

