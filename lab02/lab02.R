#=======================================================#
#                  STATYSTYKA MATEMATYCZNA              # 
#                     LABORATORIUM 2                    #
#=======================================================#

# Cel: statystyka opisowa -------------------------------------------------

library(dplyr)
library(moments)
library(ggplot2)


# Zadanie 1 ---------------------------------------------------------------

nauka <- c(
  20, 35, 45, 50, 55, 60, 60, 65,
  70, 75, 80, 80, 85, 90, 95, 
  seq(from = 100, to = 140, by = 10),
  150, 150, 
  seq(from = 160, to = 330, by = 10)
)

# a) 
(series <- cut(nauka, 6))

# b) 
(count <- table(series))
(cumcount <- cumsum(count))
(freq <- count / sum(count))

# c) histogram
barplot(count)
hist(nauka)


# Zadanie 2 ---------------------------------------------------------------

# a) 
diamonds %>%
  ggplot(aes(x = carat)) +
  geom_histogram()

# b)
(h <- hist(diamonds$carat, breaks = "FD"))
lines(h$mids, h$counts, col = 3)

# c)
stem(sample(diamonds$carat, 50, replace = FALSE))

# d)
price <- diamonds$price
mean(price)
sd(price)
var(price)
median(price)
skewness(price)
range(price)
summary(price)
kurtosis(price)

winsor_mean <- function(x, replace = 0.1) {
  n <- length(x)
  k <- as.integer(n * replace)
  x <- sort(x)
  x[1:k] <- x[k+1]
  x[(n-k+1):n] <- x[n-k]
  mean(x)
}

winsor_mean(price)

# e
boxplot(price)

# f)
quantile(price, c(5, 10, 25, 50, 75, 90, 95) / 100)

# g)
mean(price, trim = 0.1)
mean(price)
median(price)

n = 100
means <- rep(0, n)
trims <- seq(0, 0.5, length.out = 100)
for (i in seq_along(trims)) {
  means[i] <- mean(price, trim = trims[i])
}
plot(trims, means, type = 'l')
abline(h = c(mean(price), median(price)))


# Zadanie 3 ---------------------------------------------------------------
msleep %>% 
  ggplot(aes(x = sleep_total, color = vore)) +
  geom_density()
# a)


# b)


# c)



# Zadanie 4 ---------------------------------------------------------------

msleep %>%
  ggplot(aes(y = sleep_total, colour = vore)) +
  geom_boxplot()

# Zadanie 5 ---------------------------------------------------------------

set.seed(2204)
dane_wykladnicze <- rexp(500, rate = 0.2)

# a)
hist(dane_wykladnicze)

# b) i c)
skewness(dane_wykladnicze)
kurtosis(dane_wykladnicze)


# Zadanie 6 ---------------------------------------------------------------



# Zadanie 7 ---------------------------------------------------------------

transakcje <- c(rnorm(80, 50, 10), rnorm(80, 200, 15))

# a)


# b)


# c) 



# Zadanie 8 ---------------------------------------------------------------



# Zadanie 9 ---------------------------------------------------------------


