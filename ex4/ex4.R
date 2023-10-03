rm(list = ls())
# dev.off()

library("corpcor") # usado para função da pseudoinversa
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
X <- matrix(cbind(joinedClasses, rep(1, N)), nrow = 2 * N, ncol = 3)

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

points(Xtrain[, 1], Xtrain[, 2], lwd = 1, col = Ytraincolors)

# agora treina a rede RBF
n <- dim(Xtrain)[2] # correcao pelo bias
N <- dim(Xtrain)[1] # correcao: total de dados é a qtd de dados de teste
xclust <- kmeans(Xtrain, p)

# salva centros
m <- as.matrix(xclust$centers)

# define a matriz de covariancia
covlist <- list()

for (i in 1:p) {
  ici <- which(xclust$cluster == i)
  xci <- Xtrain[ici, ]
  if (n == 1) {
    covi <- var(xci)
  } else {
    covi <- cov(xci)
  }

  covlist[[i]] <- covi
}

# Calcula matriz H
H <- matrix(nrow = N, ncol = p)
for (j in 1:N)
{
  for (i in 1:p)
  {
    mi <- m[i,]
    covi <- covlist[i]
    covi <- matrix(unlist(covlist[i]), ncol = n, byrow = T) + 0.001 *  diag(n)
    H[j, i] <- pdfnvar(Xtrain[j, ], mi, covi, n)
  }
}

Haug <- cbind(H, 1)
W <- pseudoinverse(Haug) %*% Ytrain

# calcula a saida da rede para cada ponto do grid
gridPoints <- 100
xgrid <- seq(from = -2, to = 6, length.out = gridPoints)
ygrid <- seq(from = -2, to = 6, length.out = gridPoints)
M <- matrix(0, nrow = gridPoints, ncol = gridPoints)

# for (j in 1:N)
# {
#   for (i in 1:p)
#   {
#     mi <- m[i,]
#     covi <- covlist[i]
#     covi <- matrix(unlist(covlist[i]), ncol = n, byrow = T) + 0.001 *  diag(n)
#     H[j, i] <- pdfnvar(Xtrain[j, ], mi, covi, n)
#   }
# }


