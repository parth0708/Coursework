library(Sleuth3)
attach(ex0432)
data <- ex0432

wilcox.test(data$Placebo - data$Marijuana, conf.int = TRUE)

CI <- vector(mode = "numeric", length = 0)
for (i in seq(1,100,length.out = 10000)){
  if((wilcox.test(data$Placebo - data$Marijuana - i)$p.value) > 0.05){
    CI <- c(i,CI)
  }
}

min(CI)
max(CI)