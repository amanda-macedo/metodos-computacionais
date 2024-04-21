## Q1
load(file = "T1_q1.rda")

l <- sapply(seq(1, length(lista.q1)), function(i) lista.q1[[i]][3, 3])
q1a <- sum(l)

q1b <- lista.q1[[87]][1, 2]

q1c <- sum(data.q1$X70)

q1d <- data.q1$X148[2]

nrows <- nrow(matriz.q1)
ncols <- ncol(matriz.q1)
q1e <- sum(
  sapply(
    seq(1, nrows * ncols),
    function(i) if (matriz.q1[i] %% 2 == 0) matriz.q1[i] else 0
  )
)

q1a
q1b
q1c
q1d
q1e

## Q2 - a
load(file = "T1_q2_a.rda")

q2_a_a <- mean((data.q2[, 1] == "azul") * (data.q2[, 2] == "azul"))
round(q2_a_a, 3) * 1000

q2_a_b <- mean((data.q2[, 1] == "não.azul") * (data.q2[, 2] == "não.azul"))
round(q2_a_b, 3) * 1000

q2_a_c <- mean(!((data.q2[, 1] == "não.azul") * (data.q2[, 2] == "não.azul")))
round(q2_a_c, 3) * 1000

## Q2 - b
load(file = "T1_q2_b.rda")

q2_b_a <- mean((data.q2[, 1] == "defeituosa") * (data.q2[, 2] == "defeituosa"))


q2_b_b <- mean((data.q2[, 1] == "perfeita") * (data.q2[, 2] == "perfeita"))

q2_b_c <- mean(
  (
   (data.q2[, 1] == "perfeita") * (data.q2[, 2] == "defeituosa") +
     (data.q2[, 1] == "defeituosa") * (data.q2[, 2] == "perfeita"))
)

q2_b_d <- mean((data.q2[, 1] == "perfeita") * (data.q2[, 2] == "defeituosa"))

round(q2_b_a, 3) * 1000
round(q2_b_b, 3) * 1000
round(sum(q2_b_c), 3) * 1000
round(sum(q2_b_d), 3) * 1000

## Q3
dados3 <- read.csv("T1_q3.csv", header = TRUE)
x <- dados3$x
y <- dados3$y

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
x0 <- 0.85

q3a <- nad_wat(x, y, x0, h)
q3b <- nad_wat_grid(x, y, x0 = seq(from = 0.1, to = 0.9, by = 0.05), h)

round(abs(q3a), 3) * 1000
round(abs(mean(q3b)), 3) * 1000

# Q4
dados4 <- read.csv("T1_q4.csv", header = TRUE)
x <- dados4$x

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

# Q5
load(file = "T1_q5.rda")
plot(data_q5$x, data_q5$y)