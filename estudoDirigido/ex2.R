rm(list = ls())
dev.off()

source("C:\\dev\\rna-2023-2\\utils\\rbf.R")
source("C:\\dev\\rna-2023-2\\utils\\funcoesUteisR.R")

set.seed(203)

numberOfSimulations <- 1
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
sample <- sample(c(TRUE, FALSE), nrow(dataset), replace = TRUE, prob = c(0.7, 0.3))
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

# hiperparametros (argumentos) do treinamento da MLP
maxepocas <- 1000
tol <- 0.01
eepoca <- tol + 1
nepocas <- 1
eta <- 0.01

for (sim in 1:numberOfSimulations) {
  # treina a MLP
  mse <- 0
  
  # inicializa todos os pesos
  # primeiro neuronio
  z10 <- getRandomNumber()
  z11 <- getRandomNumber()
  z12 <- getRandomNumber()
  # segundo
  z20 <- getRandomNumber()
  z21 <- getRandomNumber()
  z22 <- getRandomNumber()
  # terceiro
  z30 <- getRandomNumber()
  z31 <- getRandomNumber()
  z32 <- getRandomNumber()
  # neuronio de saida
  w40 <- getRandomNumber()
  w41 <- getRandomNumber()
  w42 <- getRandomNumber()
  w43 <- getRandomNumber()
  
  evec <- matrix(nrow = 1, ncol = maxepocas)
  while ((nepocas < maxepocas) && (eepoca > tol)) {
    ei2 <- 0
    N <- dim(Xtrain)[1]
    xseq <- sample(length(Ytrain))
    for (i in 1:length(Ytrain))
    {
      irand <- xseq[i]
      x1 <- Xtrain[irand, 1]
      x2 <- Xtrain[irand, 2]
      
      h1 <- tanh(x1 * z11 + x2 * z12 + z10)
      h2 <- tanh(x1 * z21 + x2 * z22 + z20)
      h3 <- tanh(x1 * z31 + x2 * z32 + z30)
      
      yhat <- h1 * w41 + h2 * w42 + h3 * w43 + w40
      
      e <- Ytrain[irand] - yhat
      
      de4 <- e * (sech2(h1 * w41 + h2 * w42 + h3 * w43 + w40))
      
      dw40 <- eta * de4 * 1
      dw41 <- eta * de4 * h1
      dw42 <- eta * de4 * h2
      dw43 <- eta * de4 * h3
      
      de1 <- de4 * w41 * (sech2(x1 * z11 + x2 * z12 + z10))
      dz10 <- eta * de1 * 1
      dz11 <- eta * de1 * x1
      dz12 <- eta * de1 * x2
      
      de2 <- de4 * w42 * (sech2(x1 * z21 + x2 * z22 + z20))
      dz20 <- eta * de2 * 1
      dz21 <- eta * de2 * x1
      dz22 <- eta * de2 * x2
      
      de3 <- de4 * w43 * (sech2(x1 * z31 + x2 * z32 + z30))
      dz30 <- eta * de3 * 1
      dz31 <- eta * de3 * x1
      dz32 <- eta * de3 * x2
      
      w40 <- w40 + dw40
      w41 <- w41 + dw41
      w42 <- w42 + dw42
      w43 <- w43 + dw43
      
      z10 <- z10 + dz10
      z11 <- z11 + dz11
      z12 <- z12 + dz12
      
      z20 <- z20 + dz20
      z21 <- z21 + dz21
      z22 <- z22 + dz22
      
      z30 <- z30 + dz30
      z31 <- z31 + dz31
      z32 <- z32 + dz32
      
      ei2 <- ei2 + (e * e) / 2
    }
    
    nepocas <- nepocas + 1
    evec[nepocas] <- ei2 / N
    
    eepoca <- evec[nepocas]
  }
  
  # concluido o treinamento, agora é testar com os dados de teste
  # calcula a saida da rede
  
  # yhat_list <- c()
  # x_test_length <- length(x_test)
  # for (i in 1:x_test_length) {
  #   x1 <- x_test[i, 1]
  #   
  #   h1 <- tanh(x1 * z11 + z10)
  #   h2 <- tanh(x1 * z21 + z20)
  #   h3 <- tanh(x1 * z31 + z30)
  #   
  #   yhat <- h1 * w41 + h2 * w42 + h3 * w43 + w40
  #   mse <- mse + (y_test[i] - yhat)^2
  # }
}

# averageAccuracy <- mean(accArray)
# standardDeviation <- sd(accArray)
# 
# msg <- paste(averageAccuracy, " +- ", standardDeviation)
# print(msg)

# calcula a saida da rede para uma entrada
Ymlp <- function(x1, x2) {
  h1 <- tanh(x1 * z11 + x2 * z12 + z10)
  h2 <- tanh(x1 * z21 + x2 * z22 + z20)
  h3 <- tanh(x1 * z31 + x2 * z32 + z30)
  
  yhat <- h1 * w41 + h2 * w42 + h3 * w43 + w40
  
  return (sign(yhat)) # usa se é positivo ou negativo (limiar de ativação)
}

gridPoints <- 100
xgrid <- seq(from = -2, to = 6, length.out = gridPoints)
ygrid <- seq(from = -2, to = 6, length.out = gridPoints)
M <- matrix(0, nrow = gridPoints, ncol = gridPoints)

for (i in 1:(gridPoints)) {
  for (j in 1:(gridPoints)) {
    M[i, j] <- Ymlp(xgrid[i], ygrid[j])
  }
}

Ygridcolors <- c()
i <- 1
for (y in Ytest) {
  Ygridcolors[i] <- if (Ytest[i] == 1) "red" else "blue"
  i <- i + 1
}

filled.contour(xgrid, ygrid, M, nlevels = 0, lwd = 1, lty = 1, xlab = "x1", ylab = "x2",
               main = "Classificação via MLP",
               col = c("#0000FF33", "#FF000033"), plot.axes = {
                 axis(1)
                 axis(2)
                 contour(xgrid, ygrid, M, nlevels = 0, add = TRUE)
                 points(Xtest[,1], Xtest[,2], lwd = 2, col = Ygridcolors)
               })







