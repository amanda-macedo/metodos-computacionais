setwd("~/GitHub/metodos-computacionais/T1/Tentativa 1/Q1")
load(file = "T1_q1.rda")

# a
lista.q1[[1]]
lista.q1[[1]][3, "z"]

soma_valores <- 0
for (i in 1:length(lista.q1)) {
  valor <- lista.q1[[i]][3, "z"]
  soma_valores <- soma_valores + valor}
soma_valores

# b
lista.q1[[87]][1, "y"]

# c
sum(data.q1$X70)

# d
data.q1$X148[2]

# e
indices = which(matriz.q1 %% 2 == 0, arr.ind = TRUE) #pega indices dos elemnetos pares
pares = matriz.q1[indices]
sum(pares)
