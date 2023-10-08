rm(list = ls())
# dev.off()

library("corpcor") # usado para função da pseudoinversa
source("C:\\dev\\rna-2023-2\\utils\\rbf.R")
source("C:\\dev\\rna-2023-2\\utils\\funcoesUteisR.R")

# set.seed(203)

N <- 200 # numero de amostras de cada classe
n <- 2 # dimensao do espaço de entrada
p <- 10 # numero de neuronios da camada intermediaria

retlist <- gera_gaussianas_2classes_2D(N / 2, c(0, 0), 0.4)
xClass1_1 <- retlist[[1]]

retlist <- gera_gaussianas_2classes_2D(N / 2, c(4, 4), 0.4)
xClass1_2 <- retlist[[1]]

retlist <- gera_gaussianas_2classes_2D(N / 2, c(4, 0), 0.4)
xClass2_1 <- retlist[[1]]

retlist <- gera_gaussianas_2classes_2D(N / 2, c(0, 4), 0.4)
xClass2_2 <- retlist[[1]]

# cria a matriz X com todos os meus dados de entrada e a coluna de bias
joinedClasses <- rbind(xClass1_1, xClass1_2, xClass2_1, xClass2_2)
X <- matrix(cbind(rep(1, N), joinedClasses), nrow = 2 * N, ncol = 3)

# cria a matriz Y com todas as saidas esperadas
# os primeiros N elementos da classe 1 são Y = 1 (eu defini isso)
# e os proximos N elementos da classe 2 são Y = -1
Y <- matrix(cbind(rep(1, N), rep(-1, N)), nrow = 2 * N, ncol = 1)
Ycolors <- c()
i <- 1
for (y in Y) {
  Ycolors[i] <- if (Y[i] == 1) "red" else "blue"
  i <- i + 1
}

# 90% do conjunto de dados de entrada vira treinamento, o resto teste
dataset <- cbind(X, Y)
sample <- sample(c(TRUE, FALSE), nrow(dataset), replace = TRUE, prob = c(0.9, 0.1))
Xtrain <- dataset[sample, 1:(n + 1)]
Xtest <- dataset[!sample, 1:(n + 1)]
Ytrain <- dataset[sample, ncol(dataset)]
Ytest <- dataset[!sample, ncol(dataset)]

Ytraincolors <- c()
i <- 1
for (y in Ytrain) {
  Ytraincolors[i] <- if (Ytrain[i] == 1) "red" else "blue"
  i <- i + 1
}

plot(
  NULL,
  main = "Classificação via RBF",
  xlab = "x1",
  ylab = "x2",
  ylim = c(-2, 6),
  xlim = c(-2, 6)
)

points(Xtrain[, 2], Xtrain[, 3], lwd = 1, col = Ytraincolors)

# agora treina a rede RBF
modRBF <- trainRBF(Xtrain, Ytrain, p)
yhat <- YRBF(Xtrain, modRBF)

# calcula a saida da rede para cada ponto do grid
gridPoints <- 100
xgrid <- seq(from = -2, to = 6, length.out = gridPoints)
ygrid <- seq(from = -2, to = 6, length.out = gridPoints)
Xvalid <- cbind(1, xgrid, ygrid)


