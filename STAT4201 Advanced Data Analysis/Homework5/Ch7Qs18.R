library(Sleuth3)
attach(case0702)
dat <- case0702

pH <- dat$pH
time <- dat$Time
model <- lm(pH ~ log(time))
predict(model, newdata = list(time = 5), interval = "predict",
        level = 0.95, se.fit = TRUE)

n = nrow(dat)
timet <- log(time)
SSR <- sum(model$residuals^2)
sig_est <- sqrt(SSR/(n-2))
SE_est <- sig_est*sqrt(1 + 1/n + (((log(5)-mean(timet))^2) / 
                                    ((n-1)*(var(timet)))))