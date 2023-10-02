rm(list = ls())
dev.off()

library("mlbench") # machine learning bench
library("corpcor") # usado para função da pseudoinversa

set.seed(203)

N <- 100 # numero de observacoes
n <- 2 # dimensao da entrada
p <- 75 # numero de neuronios na camada intermediaria
data <- mlbench.spirals(N, n)

# matriz com todos os dados de entrada: tamanho N x 2
# N observações de 2 dimensoes cada
X <- data[[1]]

# matriz Y com as saidas esperadas de cada ponto em X
# pode ser 1 ou 2
Y <- c()
Ycolors <- c()
classes <- data[[2]]
i <- 1
for (class in classes) {
  Y[i] <- if(class == "1") 1 else -1 # usamos tanh na funcao de ativação: deve ser -1 ou 1
  # se quiser usar 1 e 2, a funcao de ativação deve ser tanh(.) + 2, que varia entre 1 e 2
  # mas usar 1 e -1 é melhor, mais legivel e mais simples
  Ycolors[i] <- if (Y[i] == 1) "red" else "blue"
  i <- i + 1
}

# adiciona termo de bias na ultima coluna da matriz de entrada X
Xaug <- cbind(X, 1)

# define a matriz aleatoria dos pesos Z
Z <- replicate(p, runif(n + 1, -0.5, 0.5)) # n + 1 por causa do bias

# agora vem a matriz H = tanh(X Z), a projecao em dimensao maior na camada
# intermediaria
H <- tanh(Xaug %*% Z) # N x p

# obtem os pesos do Adaline de saida atraves de pseudoinversa normalmente
W <- pseudoinverse(H) %*% Y

# plota as curvas de nivel. gera o grid
gridPoints <- 100
xgrid <- seq(from = -1.5, to = 1.5, length.out = gridPoints)
ygrid <- seq(from = -1.5, to = 1.5, length.out = gridPoints)
M <- matrix(0, nrow = gridPoints, ncol = gridPoints)

# calcula o valor de Y em cada ponto e joga na matriz M
for (i in 1:(gridPoints)) {
  for (j in 1:(gridPoints)) {
    xin <- as.matrix(c(xgrid[i], ygrid[j], 1))
    hgrid <- tanh(t(xin) %*% Z)
    M[i, j] <- as.numeric(tanh(hgrid %*% W))
  }
}

# plot(
#   NULL,
#   main = "Espiral",
#   xlab = "x1",
#   ylab = "x2",
#   ylim = c(-1.5, 1.5),
#   xlim = c(-1.5, 1.5)
# )
# 
# points(X[,1], X[,2], lwd = 2, col = Ycolors)
# contour(xgrid, ygrid, M, col = "brown", nlevels = 1, lwd = 2, lty = 1, add = TRUE)

filled.contour(xgrid, ygrid, M, nlevels = 0, lwd = 1, lty = 1, xlab = "x1", ylab = "x2",
               main = if (p == 5) {
                 "Espiral, p = 5"
               } else if (p == 10) {
                 "Espiral, p = 10"
               } else {
                 "Espiral, p = 30"
               },
               col = c("#0000FF33", "#FF000033"), plot.axes = {
                 axis(1)
                 axis(2)
                 contour(xgrid, ygrid, M, nlevels = 0, add = TRUE)
                 points(X[,1], X[,2], lwd = 3, col = Ycolors)
               })
