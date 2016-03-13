library(Sleuth3)
attach(ex0430)
data <- ex0430
multiplier <- data$Sunscreen/data$PreTreatment
t.test(multiplier)

exp(t.test(log(data$Sunscreen)-log(data$PreTreatment))$conf.int)
exp(t.test(log(data$Sunscreen)-log(data$PreTreatment))$estimate)

wilcox.test(log(data$Sunscreen),log(data$PreTreatment),paired = TRUE, conf.int = TRUE)
wilcox.test(log(data$Sunscreen)-log(data$PreTreatment))$p.value
CI <- vector(mode = "numeric", length = 0)
for (i in seq(3,14,length.out=100)){
  if (wilcox.test(log(data$Sunscreen/i),log(data$PreTreatment),paired = TRUE)$p.value > 0.05){
    CI <- c(i,CI)
  }
}

min(CI)
max(CI)


