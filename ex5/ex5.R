rm(list = ls())
# dev.off()

source("C:\\dev\\rna-2023-2\\utils\\funcoesUteisR.R")

set.seed(203)

mse_list <- c()
numberOfIterations = 1

# geração dos dados de treinamento
x_train <- seq(from = 0, to = 3 * pi, by = 0.15)
x_train <- x_train + (runif(length(x_train)) - 0.5) / 5
i <- sample(length(x_train))
x_train <- x_train[i]
y_train <- sin(x_train)
y_train <- y_train + (runif(length(y_train)) - 0.5) / 5
plot(x_train, y_train, col = "blue", xlim = c(0, 2 * pi), ylim = c(-1.25, 1.25), xlab = "x", ylab = "y")

# geração dos dados de teste
x_test <- seq(from = 0, to = 3 * pi, by = 0.01)
y_test <- sin(x_test)
par(new = T)
lines(x_test, y_test, col = "red", lwd = 2)
legend(x = 4, y = 1.25, legend = c("train", "test", "model"), col = c("blue", "red", "black"), pch = c("o", "_", "_"))

# dados de treinamento são 42 observações de apenas uma variável: só tem uma dimensão
# a saída também só tem uma dimensão
# converte eles para matriz, para ter certeza
x_train <- as.matrix(x_train, ncol = 1)
y_train <- as.matrix(y_train, ncol = 1)
x_test <- as.matrix(x_test, ncol = 1)
y_test <- as.matrix(y_test, ncol = 1)

# hiperparametros (argumentos) do treinamento da MLP
maxepocas <- 50000
tol <- 0.01
eepoca <- tol + 1
nepocas <- 1
eta <- 0.005

for (iteration in 1:numberOfIterations) {
  # treina a MLP
  mse <- 0
  
  # inicializa todos os pesos
  z10 <- getRandomNumber()
  z11 <- getRandomNumber()
  z20 <- getRandomNumber()
  z21 <- getRandomNumber()
  z30 <- getRandomNumber()
  z31 <- getRandomNumber()
  w40 <- getRandomNumber()
  w41 <- getRandomNumber()
  w42 <- getRandomNumber()
  w43 <- getRandomNumber()
  
  evec <- matrix(nrow = 1, ncol = maxepocas)
  while ((nepocas < maxepocas) && (eepoca > tol)) {
    ei2 <- 0
    N <- dim(x_train)[1]
    xseq <- sample(length(y_train))
    for (i in 1:length(y_train))
    {
      irand <- xseq[i]
      x1 <- x_train[irand, 1]
      
      h1 <- tanh(x1 * z11 + z10)
      h2 <- tanh(x1 * z21 + z20)
      h3 <- tanh(x1 * z31 + z30)
      
      yhat <- h1 * w41 + h2 * w42 + h3 * w43 + w40
      
      e <- y_train[irand] - yhat
      
      de4 <- e * (sech2(h1 * w41 + h2 * w42 + h3 * w43 + w40))
      
      dw40 <- eta * de4 * 1
      dw41 <- eta * de4 * h1
      dw42 <- eta * de4 * h2
      dw43 <- eta * de4 * h3
      
      de1 <- de4 * w41 * (sech2(x1 * z11 + z10))
      dz10 <- eta * de1 * 1
      dz11 <- eta * de1 * x1
      
      de2 <- de4 * w42 * (sech2(x1 * z21 + z20))
      dz20 <- eta * de2 * 1
      dz21 <- eta * de2 * x1
      
      de3 <- de4 * w43 * (sech2(x1 * z31 + z30))
      dz30 <- eta * de3 * 1
      dz31 <- eta * de3 * x1
      
      w40 <- w40 + dw40
      w41 <- w41 + dw41
      w42 <- w42 + dw42
      w43 <- w43 + dw43
      z10 <- z10 + dz10
      z11 <- z11 + dz11
      z20 <- z20 + dz20
      z21 <- z21 + dz21
      z30 <- z30 + dz30
      z31 <- z31 + dz31
      
      ei2 <- ei2 + (e * e) / 2
    }
    
    nepocas <- nepocas + 1
    evec[nepocas] <- ei2 / N
    
    eepoca <- evec[nepocas]
  }
  
  # concluido o treinamento, agora é testar com os dados de teste
  # calcula a saida da rede
  
  yhat_list <- c()
  x_test_length <- length(x_test)
  for (i in 1:x_test_length) {
    x1 <- x_test[i, 1]
    
    h1 <- tanh(x1 * z11 + z10)
    h2 <- tanh(x1 * z21 + z20)
    h3 <- tanh(x1 * z31 + z30)
    
    yhat <- h1 * w41 + h2 * w42 + h3 * w43 + w40
    yhat_list <- append(yhat_list, yhat)
    mse <- mse + (y_test[i] - yhat)^2
  }
  
  mse_list <- append(mse_list, mse / x_test_length)
}

lines(x_test, yhat_list, col = "black", lwd = 2)

average_mse <- mean(mse_list)
sd_mse <- sd(mse_list)

print(average_mse)
print(sd_mse)
