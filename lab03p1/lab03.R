library(datasets)
library(moments)

set.seed(338979)

moje_dane <- sample(datasets::trees$Volume, 30, replace = TRUE)
x <- moje_dane

hist(x, breaks = 10)

kurtosis(x)

mean(x)
mean(x, trim = 0.1)

kurtosis(rnorm(100, 0, 30))
kurtosis(runif(100, 10, 60))

# 2.6
x <- rnorm(200)
hist(x, probability = TRUE)
lines(density(x), col = "red")
curve(dnorm(x), add = TRUE, col = "blue")

# 2.7
x <- c(rnorm(80, 50, 10), rnorm(80, 200, 15))
mean(x)
median(x)
boxplot(x)

hist(x, probability = TRUE)
for (i in seq(0.1, 1, 0.1)) {
  lines(density(x, adjust = i), col = "gray")
}
lines(density(x, adjust = 0.3), col = "red")
curve((80 * dnorm(x, 50, 10) + 80 * dnorm(x, 200, 15)) / 160, add = TRUE, col = "blue")

# 2.9
library(ggplot2)

x <- diamonds$carat
hist(x, probability = TRUE, breaks = 50)
adjust = 1
lines(density(x, adjust = adjust, kernel = "gaussian"), col = "red")
lines(density(x, adjust = adjust, kernel = "epanechnikov"), col = "darkgreen")
lines(density(x, adjust = adjust, kernel = "rectangular"), col = "blue")
lines(density(x, adjust = adjust, kernel = "triangular"), col = "purple")

# 3.1
x <- rnorm(20)
y <- rnorm(100)
plot(ecdf(x))
curve(pnorm(x), add = TRUE, col = "blue")
plot(ecdf(y))
curve(pnorm(x), add = TRUE, col = "blue")

# 3.2
y <- rcauchy(500)

cmean <- sapply(seq_along(y), function(i) mean(y[1:i]))
cmedian <- sapply(seq_along(y), function(i) median(y[1:i]))
plot(cmean, type = "l", col = 2)
lines(cmedian, add = TRUE, col = 3)
abline(h = 0, col = 4)
legend("topleft", c("mean", "median", "a = 0"), col = 2:4, lty = 1)

csd <- sapply(seq_along(y), function(i) sd(y[1:i]))
cSQR <- sapply(seq_along(y), function(i) IQR(y[1:i]) / 2)
plot(csd, type = "l", col = 2)
lines(cSQR, add = TRUE, col = 3)
abline(h = 1, col = 4)
legend("topleft", c("sd", "SQR", "b = 1"), col = 2:4, lty = 1)

# 3.3
y <- rnorm(500)

cmean <- sapply(seq_along(y), function(i) mean(y[1:i]))
cmedian <- sapply(seq_along(y), function(i) median(y[1:i]))
plot(cmean, type = "l", col = 2)
lines(cmedian, add = TRUE, col = 3)
abline(h = 0, col = 4)
legend("topright", c("mean", "median", "mu = 0"), col = 2:4, lty = 1)

csd <- sapply(seq_along(y), function(i) sd(y[1:i]))
cIQR <- sapply(seq_along(y), function(i) IQR(y[1:i]) / 1.35)
plot(csd, type = "l", col = 2)
lines(cIQR, add = TRUE, col = 3)
abline(h = 1, col = 4)
legend("bottomright", c("sd", "IQR/1.35", "sigma = 1"), col = 2:4, lty = 1)
