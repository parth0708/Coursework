library(Sleuth3)
attach(ex0332)

group_Public <- subset(ex0332,Type == "Public")
group_Private <- subset(ex0332,Type == "Private")

boxplot(log(group_Public$InState),log(group_Private$InState))


t.test((log(group_Public$OutOfState)-log(group_Public$InState)), var.equal=TRUE)
t.test(log(group_Private$InState),log(group_Public$InState), var.equal=TRUE)
t.test(log(group_Private$OutOfState),log(group_Public$OutOfState), var.equal=TRUE)