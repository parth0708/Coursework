mu = c(0.1, 0.5, 1, 2)
power10 <- power.t.test(n=10, delta = mu, sd = 1, sig.level = 0.05, type = "two.sample", alternative = "two.sided")
power10
power20 <- power.t.test(n=20, delta = mu, sd = 1, sig.level = 0.05, type = "two.sample", alternative = "two.sided")
power20
plot(mu,power10$power,col="green", ylab = "Power")
points(mu,power20$power, col="blue")