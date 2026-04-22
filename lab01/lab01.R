#=======================================================#
#                  STATYSTYKA MATEMATYCZNA              # 
#                     LABORATORIUM 1                    #
#=======================================================#


# Cel: wybrane rozkłady prawdopodobieństwa --------------------------------

# norm - rozkład normalny
# chisq - rozkład chi-kwadrat
# gamma - rozkład gamma
# exp - rozkład wykładniczy
# f - rozkład F-Snedecora

# d - gęstość f(x) lub rozkład prawdopodobieństwa P(X = x)
# p - dystrybuanta F(x) = P(X <= x)
# q - funkcja kwantylowa 
# r - generowanie liczb pseudolosowych


# Zadanie 1 ---------------------------------------------------------------

# a) N(0, 1), N(1, 1), N(2, 1)


# gęstośćhttps://pages.mini.pw.edu.pl/~grzegorzewskip/www/?Dydaktyka:Statystyka_matematyczna:Laboratoria

curve(dnorm(x, 0, 1), -5, 5, col = 2)
curve(dnorm(x, 1, 1), -5, 5, col = 3, add = TRUE)
curve(dnorm(x, 2, 1), -5, 5, col = 4, add = TRUE)
legend("topleft", c("N(0, 1)", "N(1, 1)", "N(2, 1)"),
       lty = 1,
       col = c(2, 3, 4),
       text.col = 1)
title("Gęstość")

# dystrybuanta

params <- list(c(0, 1), c(1, 1), c(2, 1))
colors <- seq_along(params) + 1
for (i in seq_along(params)) {
  mean <- params[[i]][1]
  sd <- params[[i]][2]
  curve(pnorm(x, mean, sd),
        xlim = c(-4, 6),
        ylim = c(0, 1),
        col = colors[i],
        add = i > 1)
}
legend("topleft", sapply(params, function(x) paste0("N(", x[1], ", ", x[2], ")")),
       lty = 1,
       col = c(2, 3, 4),
       text.col = 1)
title("Dystrybuanta")

# funkcja przeżycia 



# b) N(0, 1), N(0, 0.5), N(0, 2)


# gęstość



# dystrybuanta



# funkcja przeżycia



# Zadanie 2 ---------------------------------------------------------------

# a) 
pnorm(179, 173, 6)

# b)
pnorm(180, 173, 6) - pnorm(167, 173, 6)

# c)
1 - pnorm(181, 173, 6)

# d)
qnorm(0.6, 173, 6)


# Zadanie 3 ---------------------------------------------------------------

# a)
qnorm(0.95, 0, 1)

# b)
qt(0.99, 20)

# c)
qchisq(0.95, 10)

# d)
qf(0.99, 3, 18)


# Zadanie 4 ---------------------------------------------------------------


# a)
params <- list(c(1, 1) , c(0.5, 1) , c(2, 1) , c(3, 1))
colors <- seq_along(params) + 1
for (i in seq_along(params)) {
  a <- params[[i]][1]
  b <- params[[i]][2]
  curve(dgamma(x, a, b),
        xlim = c(-1, 8),
        ylim = c(0, 1.7),
        col = colors[i],
        add = i > 1)
}
legend("topright", sapply(params, function(x) paste0("Gamma(", x[1], ", ", x[2], ")")),
       lty = 1,
       col = c(2, 3, 4),
       text.col = 1)
title("Gęstość")

# b)
params <- list(c(2, 1) , c(2, 2) , c(2, 3))
colors <- seq_along(params) + 1
for (i in seq_along(params)) {
  a <- params[[i]][1]
  b <- params[[i]][2]
  curve(dgamma(x, a, b),
        xlim = c(-1, 8),
        ylim = c(0, 1.1),
        col = colors[i],
        add = i > 1)
}
legend("topright", sapply(params, function(x) paste0("Gamma(", x[1], ", ", x[2], ")")),
       lty = 1,
       col = c(2, 3, 4),
       text.col = 1)
title("Gęstość")


# Zadanie 5 ---------------------------------------------------------------

# dist_func: nazwa funkcji (np. dgamma, dchisq, dt)
# params_list: lista list z parametrami dla każdej krzywej
# xlim: zakres osi X
# ylim: zakres osi Y (opcjonalny)
# main: tytuł wykresu
# labels: wektor nazw do legendy (opcjonalny)

plot_distributions <- function(dist_func, 
                               params_list, 
                               xlim, 
                               ylim = NULL, 
                               main,
                               labels = NULL) {
  colors <- seq_along(params_list) + 1
  for (i in seq_along(params_list)) {
    a <- params[[i]][1]
    b <- params[[i]][2]
    curve(do.call(dist_func, c(list(x = x), params_list[[i]])),
          xlim = xlim,
          ylim = ylim,
          col = colors[i],
          add = i > 1)
  }
  legend("topright", labels,
         lty = 1,
         col = colors,
         text.col = 1)
  title(main)
}

# --- Przykład użycia ---

params <- list(
  list(mean = 0, sd = 1),
  list(mean = 0, sd = 2),
  list(mean = 0, sd = 3)
)

plot_distributions(dnorm, 
                   params, 
                   xlim = c(-5, 5), 
                   main = "Porównanie gęstości rozkładu normalnego",
                   labels = c("Norm(0,1)", "Norm(0,2)", "Norm(0,3)"))


# Zadanie 6 ---------------------------------------------------------------



# Zadanie 7 ---------------------------------------------------------------



# Zadanie 8 ---------------------------------------------------------------


# a)



# b) 



# c) 



# Zadanie 9 ---------------------------------------------------------------

dgeom(0, 0.1)
dgeom(1, 0.1)
dgeom(2, 0.1)
1 - pgeom(10, 0.1)

# Zadanie 10 --------------------------------------------------------------

dhyper(0, 5, 200 - 5, 10)

# Zadanie 11 --------------------------------------------------------------


# a)

1 - pexp(1000, 0.0001)
1 - pexp(10000, 0.0001)
1 - pexp(30000, 0.0001)

# b)

qexp(0.1, 0.0001)

# Zadanie 12 --------------------------------------------------------------


# a)

n <- 20000
x <- runif(n)
y <- runif(n)
z <- y < x^2
plot(x, y, pch = '.', col = z + 2)
mean(z)

# b)

n <- 100000
xlim <- c(-1, 1)
ylim <- c(0, 1)
x <- runif(n, xlim[1], xlim[2])
y <- runif(n, ylim[1], ylim[2])
z <- (x^2 < y) & (y < 1 - x^2)
plot(x, y, pch = '.', col = z + 2)
mean(z) * (xlim[2] - xlim[1]) * (ylim[2] - ylim[1])
