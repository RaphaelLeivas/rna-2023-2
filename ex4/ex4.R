rm(list = ls())
# dev.off()

library("corpcor") # usado para função da pseudoinversa
source("C:\\dev\\rna-2023-2\\utils\\funcoesUteisR.R")

set.seed(203)

N <- 200 # numero de amostras de cada classe
n <- 2 # dimensao do espaço de entrada
p <- 10 # numero de neuronios da camada intermediaria

retlist <- gera_gaussianas_2classes_2D(N/2, c(0,0), 0.4)
xClass1_1 <- retlist[[1]]

retlist <- gera_gaussianas_2classes_2D(N/2, c(4,4), 0.4)
xClass1_2 <- retlist[[1]]

retlist <- gera_gaussianas_2classes_2D(N/2, c(4, 0), 0.4)
xClass2_1 <- retlist[[1]]

retlist <- gera_gaussianas_2classes_2D(N/2, c(0,4), 0.4)
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
# USAR PACOTE PRONTO DO R AQUI - QUANDO TIVER INTERNET
# Ntrain <- 0.9 * 2 * N
# Ntest <- 0.1 * 2 * N
# Xtrain <- matrix(0, ncol = 3, nrow = Ntrain)
# Xtest <- matrix(0, ncol = 3, nrow = Ntest)
# 
# randomLines <- sample(2 * N)
# i <- 1
# for (line in randomLines) {
#   if (i <= Ntrain) {
#     Xtrain[i,] <- X[line,]
#   } else {
#     Xtest[(i - Ntrain),] <- X[line,]
#   }
# 
#   i <- i + 1
# }

plot(
  NULL,
  main = "Classificação via RBF",
  xlab = "x1",
  ylab = "x2",
  ylim = c(-2, 6),
  xlim = c(-2, 6)
)

points(X[,1], X[,2], lwd = 1, col = Ycolors)

# treina a rede RBF
