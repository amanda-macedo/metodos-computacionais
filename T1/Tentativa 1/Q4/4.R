setwd("~/GitHub/metodos-computacionais/T1/Tentativa 1/Q4")

f_nr <- function(
    x0, # chute inicial
    f1, # f1 := f(x), onde f(x) = 0
    f2, # f2 := f'(x)
    eps = 1 / 10000, # precisão
    ... # demais argumentos de f1 e f2
) {
  cc <- eps + 1
  conta <- 0
  trajetoria <- x0
  while (cc > eps) {
    x1 <- x0 - f1(x0, ...) / f2(x0, ...)
    cc <- (x1 - x0)^2
    x0 <- x1
    trajetoria <- c(trajetoria, x1)
    conta <- conta + 1
  }
  return(
    list(
      ponto_otimo = x1,
      num_iter = conta,
      trajetoria = trajetoria
    )
  )
}
dados <- read.csv("T1_q4.csv", header = T)

x <- dados$x


theta <- function(x, lambda) lambda * exp(-lambda * x) * (x > 0) * (lambda > 0)
f0 <- function(lambda, x) mean(log(theta(x, lambda)))
f1 <- function(lambda, x) (1 / lambda) - mean(x)
f2 <- function(lambda, x) -(1 / lambda^2)

x0 <- 1 / median(x)
max_log_like <- f_nr(x0 = x0, f1 = f1, f2 = f2, x = x)
q4a <- round(abs(max_log_like$ponto_otimo), 3) * 1000
q4b <- f1(lambda = 4, x)
q4c <- f2(lambda = 2, x)

q4a
round(abs(q4b), 3) * 1000
round(abs(q4c), 3) * 1000
