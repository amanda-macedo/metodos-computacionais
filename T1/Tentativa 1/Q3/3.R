setwd("~/GitHub/metodos-computacionais/T1/Tentativa 1/Q3")

dados <- read.csv("T1_q3.csv", header = TRUE)

x <- dados$x
y <- dados$y
x0 <- seq(from = 0.1, to = 0.9, by = 0.05)


k_epa <- function(x) {
  0.75 * (1 - x^2) * (x > -1) * (x < 1)
}

nad_wat <- function(x, y, x0, h, k = k_epa) {
  x1 <- (x - x0) / h
  sum(k(x1) * y) / sum(k(x1))
}

nad_wat_grid <- function(x, y, x0, h, k = k_epa) {
  sapply(
    seq(1, length(x0)),
    function(i) nad_wat(x, y, x0[i], h)
  )
}

h <- 0.05
x0 <- 0.25

q3a <- nad_wat(x, y, x0, h)
q3b <- nad_wat_grid(x, y, x0 = seq(from = 0.1, to = 0.9, by = 0.05), h)

round(abs(q3a), 3) * 1000
round(abs(mean(q3b)), 3) * 1000
