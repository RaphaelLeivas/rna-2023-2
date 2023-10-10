rm(list = ls())
# dev.off()

source("C:\\dev\\rna-2023-2\\utils\\rbf.R")
source("C:\\dev\\rna-2023-2\\utils\\funcoesUteisR.R")

set.seed(203)

numberOfSimulations <- 10
accArray <- c()
modRBFarray <- list()

N <- 240 # numero de amostras de cada classe
n <- 2 # dimensao do espaço de entrada
p <- 10 # numero de neuronios da camada intermediaria

retlist <- gera_gaussianas_2classes_2D(N / 2, c(0, 0), 0.5)
xClass1_1 <- retlist[[1]]

retlist <- gera_gaussianas_2classes_2D(N / 2, c(4, 4), 0.5)
xClass1_2 <- retlist[[1]]

retlist <- gera_gaussianas_2classes_2D(N / 2, c(4, 0), 0.5)
xClass2_1 <- retlist[[1]]

retlist <- gera_gaussianas_2classes_2D(N / 2, c(0, 4), 0.5)
xClass2_2 <- retlist[[1]]

# cria a matriz X com todos os meus dados de entrada e a coluna de bias
joinedClasses <- rbind(xClass1_1, xClass1_2, xClass2_1, xClass2_2)
X <- matrix(joinedClasses, nrow = 2 * N, ncol = 2)

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
Xtrain <- dataset[sample, 1:n]
Xtest <- dataset[!sample, 1:n]
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

points(Xtrain[, 1], Xtrain[, 2], lwd = 2, col = Ytraincolors)

for (sim in 1:numberOfSimulations) {
  # agora treina a rede RBF
  modRBF <- trainRBF(Xtrain, Ytrain, p)
  
  # joga esse modelo do conjunto de testes
  Yhat_test <- YRBF(Xtest, modRBF)
  
  totalOfTests <- length(Ytest)
  correctTests <- 0
  
  for (i in 1:totalOfTests) {
    if (sign(Ytest[i]) == sign(Yhat_test[i])) {
      correctTests <- correctTests + 1;
    }
  }
  
  accArray[sim] <- correctTests / totalOfTests
  modRBFarray[[sim]] <- modRBF
}

averageAccuracy <- mean(accArray)
standardDeviation <- sd(accArray)

msg <- paste(averageAccuracy, " +- ", standardDeviation)

print(msg)

# agora só falta gerar  o gráfico com a melhor superfície de separação
# determina a simulação com melhor acuracia
bestSim <- 0
bestAcc <- 0
for (i in 1:sim) {
  if (accArray[i] >= bestAcc) {
    bestAcc <- accArray[i]
    bestSim <- i
  }
}

bestModRBF <- modRBFarray[[bestSim]]

gridPoints <- 100
xgrid <- seq(from = -2, to = 6, length.out = gridPoints)
ygrid <- seq(from = -2, to = 6, length.out = gridPoints)
M <- matrix(0, nrow = gridPoints * gridPoints, ncol = 2)

for (i in 1:(gridPoints)) {
  currentLine <- (i - 1) * gridPoints
  for (j in 1:(gridPoints)) {
    M[currentLine + j, 1] <- xgrid[i]
    M[currentLine + j, 2] <- ygrid[j]
  }
}

# temos o Xgrid de entrada: a matriz M. calcula a saida para o melhor modelo
# RBF obitdo
Mhat <- YRBF(M, bestModRBF)

# converte para uma matriz grid x grid
Mhatgrid <- matrix(0, nrow = gridPoints, ncol = gridPoints)
for (i in 1:(gridPoints)) {
  currentLine <- (i - 1) * gridPoints
  for (j in 1:(gridPoints)) {
    Mhatgrid[i, j] <- Mhat[currentLine + j, 1]
  }
}

Ygridcolors <- c()
i <- 1
for (y in Ytest) {
  Ygridcolors[i] <- if (Ytest[i] == 1) "red" else "blue"
  i <- i + 1
}


filled.contour(xgrid, ygrid, Mhatgrid, nlevels = 0, lwd = 1, lty = 1, xlab = "x1", ylab = "x2",
               main = "Classificação via RBF",
               col = c("#0000FF33", "#FF000033"), plot.axes = {
                 axis(1)
                 axis(2)
                 contour(xgrid, ygrid, Mhatgrid, nlevels = 0, add = TRUE)
                 points(Xtest[,1], Xtest[,2], lwd = 2, col = Ygridcolors)
               })







