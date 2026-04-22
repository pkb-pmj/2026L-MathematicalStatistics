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

par(mfrow = c(1, 3))

# gęstość

#?curve
# funkcja curve może również rysować wykres wyrażenia zawierającego zmienną x

curve(dnorm(x, 0, 1), xlim = c(-3, 3), ylim = c(0, 0.5), col = 1)
curve(dnorm(x, 1, 1), xlim = c(-3, 3), col = 2, add = TRUE)
curve(dnorm(x, 2, 1), xlim = c(-3, 3), col = 3, add = TRUE)
legend("topleft", c("N(0,1)", "N(1,1)", "N(2,1)"), lty = 1, col = 1:3, text.col = 1)

title("Gęstość")


# dystrybuanta

curve(pnorm(x, 0, 1), xlim = c(-3, 3), ylim = c(0, 0.5), col = 1)
curve(pnorm(x, 1, 1), xlim = c(-3, 3), col = 2, add = TRUE)
curve(pnorm(x, 2, 1), xlim = c(-3, 3), col = 3, add = TRUE)
legend("topleft", c("N(0,1)", "N(1,1)", "N(2,1)"), lty = 1, col = 1:3, text.col = 1)

title("Dystrybuanta")


# funkcja przeżycia 

curve(1 - pnorm(x, 0, 1), xlim = c(-3, 3), ylim = c(0, 0.5), col = 1)
curve(1 - pnorm(x, 1, 1), xlim = c(-3, 3), col = 2, add = TRUE)
curve(1 - pnorm(x, 2, 1), xlim = c(-3, 3), col = 3, add = TRUE)
legend("topleft", c("N(0,1)", "N(1,1)", "N(2,1)"), lty = 1, col = 1:3, text.col = 1)

title("Funkcja przeżycia")


# b) N(0, 1), N(0, 0.5), N(0, 2)

par(mfrow = c(1, 3))

# gęstość

curve(dnorm(x, 0, 1), xlim = c(-3, 3), ylim = c(0, 0.5), col = 1)
curve(dnorm(x, 0, 0.5), xlim = c(-3, 3), col = 2, add = TRUE)
curve(dnorm(x, 0, 2), xlim = c(-3, 3), col = 3, add = TRUE)
legend("topleft", c("N(0,1)", "N(0,0.5)", "N(0,2)"), lty = 1, col = 1:3, text.col = 1)

title("Gęstość")


# dystrybuanta

curve(pnorm(x, 0, 1), xlim = c(-3, 3), ylim = c(0, 0.5), col = 1)
curve(pnorm(x, 0, 0.5), xlim = c(-3, 3), col = 2, add = TRUE)
curve(pnorm(x, 0, 2), xlim = c(-3, 3), col = 3, add = TRUE)
legend("topleft", c("N(0,1)", "N(0,0.5)", "N(0,2)"), lty = 1, col = 1:3, text.col = 1)

title("Dystrybuanta")


# funkcja przeżycia

curve(1 - pnorm(x, 0, 1), xlim = c(-3, 3), ylim = c(0, 0.5), col = 1)
curve(1 - pnorm(x, 0, 0.5), xlim = c(-3, 3), col = 2, add = TRUE)
curve(1 - pnorm(x, 0, 2), xlim = c(-3, 3), col = 3, add = TRUE)
legend("topleft", c("N(0,1)", "N(0,0.5)", "N(0,2)"), lty = 1, col = 1:3, text.col = 1)

title("Funkcja przeżycia")


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
?qt
qt(0.99, 20)

# c)
?qchisq
qchisq(0.95, 10)

# d)
?qf
qf(0.99, 3, 18)


# Zadanie 4 ---------------------------------------------------------------

?dgamma

# a)

par(mfrow = c(1, 1))
curve(dgamma(x, 1, 1), xlim = c(0, 6), ylim = c(0, 1), col = 1)
curve(dgamma(x, 0.5, 1), xlim = c(0, 6), col = 2, add = TRUE)
curve(dgamma(x, 2, 1), xlim = c(0, 6), col = 3, add = TRUE)
curve(dgamma(x, 3, 1), xlim = c(0, 6), col = 4, add = TRUE)
legend("topright", c("Gamma(1,1)", "Gamma(0.5,1)", "Gamma(2,1)", "Gamma(3,1)"), lty = 1, col = 1:4, text.col = 1)


# b)

par(mfrow = c(1, 1))
curve(dgamma(x, 2, 1), xlim = c(0, 6), ylim = c(0, 1), col = 1)
curve(dgamma(x, 2, 2), xlim = c(0, 6), col = 2, add = TRUE)
curve(dgamma(x, 2, 3), xlim = c(0, 6), col = 3, add = TRUE)
legend("topright", c("Gamma(2,1)", "Gamma(2,2)", "Gamma(2,3)"), lty = 1, col = 1:3, text.col = 1)



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
  
  for (i in 1:length(params_list)) {
    curve(do.call(dist_func, c(list(x = x), params_list[[i]])), 
          xlim = xlim, 
          ylim = ylim,
          col = i, 
          add = (i > 1), 
          ylab = "Gęstość", 
          main = main)
  }
  
  if (!is.null(labels)) {
    legend("topright", legend = labels, col = 1:length(labels), lty = 1)
  }
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


params <- list(
  list(df = 5),
  list(df = 10),
  list(df = 40)
)

plot_distributions(dchisq, 
                   params, 
                   xlim = c(0, 80), 
                   main = "Porównanie gęstości rozkładu chi-kwadrat",
                   labels = c("Chi^2(5)", "Chi^2(10)", "Chi^2(40)"))



# Zadanie 7 ---------------------------------------------------------------


liczba_stopni <- c(2, 5, 10, 30, 50)
params <- lapply(degrees, function(d) list(df = d))
labels <- paste("df =", degrees)


plot_distributions(dchisq, params, xlim = c(0, 100), 
                   main = "Rozkład chi-kwadrat", 
                   labels = labels)

# przykładowy rozkład normalny, który dość dobrze odwzorowuje chi-kwadrat (50)
curve(dnorm(x, 49, 10), col = 1, add = TRUE)


# Zadanie 8 ---------------------------------------------------------------

# a)
params <- list(
  list(df1 = 10, df2 = 5),
  list(df1 = 10, df2 = 10)
)

plot_distributions(df, 
                   params, 
                   xlim = c(0, 3),
                   ylim = c(0, 1),
                   main = "Porównanie gęstości rozkładu F Snedecor",
                   labels = c("F Snedecor(10,5)", "F Snedecor(10,10)"))

# b) 
params <- list(
  list(df1 = 5, df2 = 2),
  list(df1 = 3, df2 = 2)
)

plot_distributions(df, 
                   params, 
                   xlim = c(0, 3),
                   ylim = c(0, 1),
                   main = "Porównanie gęstości rozkładu F Snedecor",
                   labels = c("F Snedecor(5,2)", "F Snedecor(3,2)"))


# c) 
params <- list(
  list(df1 = 2, df2 = 1),
  list(df1 = 2, df2 = 10)
)

plot_distributions(df, 
                   params, 
                   xlim = c(0, 3),
                   ylim = c(0, 2),
                   main = "Porównanie gęstości rozkładu F Snedecor",
                   labels = c("F Snedecor(2,1)","F Snedecor(2,10)"))


# Zadanie 9 ---------------------------------------------------------------

dgeom(0, 0.1)
dgeom(1, 0.1)
dgeom(2, 0.1)
dgeom(3, 0.1)
1 - pgeom(10, 0.1)


# Zadanie 10 --------------------------------------------------------------

phyper(0, 5, 195, 10)


# Zadanie 11 --------------------------------------------------------------

# a)
1 - pexp(1000, 0.0001)
1 - pexp(10000, 0.0001)
1 - pexp(30000, 0.0001)


# b)
qexp(0.1, 0.0001)
