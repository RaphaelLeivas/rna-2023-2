rm(list = ls())
# dev.off()

library("mlbench")
library("RSNNS")

#pega os dados da package mlbench
data("BreastCancer")
dataset <- BreastCancer

#Realiza o tratamento dos dados
dataset <- dataset[complete.cases(dataset),]
data2 <- unname(as.matrix(dataset))

# rotula as observações como 0 (maligno) e 1 (benigno)
N <- dim(data2)[1] # numero de observações
n <- dim(data2)[2] # numero de variaveis

# seleciona 70% para treinamento e 30% para teste, aleatoriamente
n <- n - 2 # tira as colunas de id e class ao final
sample <- sample(c(TRUE, FALSE), nrow(data2), replace = TRUE, prob = c(0.7, 0.3))
Xtrain <- data2[sample, 2:n]
Xtest <- data2[!sample, 2:n]
Ytrain <- data2[sample, ncol(data2)]
Ytest <- data2[!sample, ncol(data2)]

# converte para -1 (maligno) e 1 (benigno)
for (i in 1:length(Ytrain)) {
  if (Ytrain[i] == "benign") {
    Ytrain[i] <- 1
  } else {
    Ytrain[i] <- -1
  }
}

for (i in 1:length(Ytest)) {
  if (Ytest[i] == "benign") {
    Ytest[i] <- 1
  } else {
    Ytest[i] <- -1
  }
}

# garante que todos sao numericos
class(Xtrain) <- "numeric"
class(Xtest) <- "numeric"
class(Ytrain) <- "numeric"
class(Ytest) <- "numeric"

# aqui finalizou o tratamento dos dados. treina a rede
rede<-mlp(Xtrain, Ytrain, size=5, maxit=2000, initFunc="Randomize_Weights",
          initFuncParams=c(-0.3, 0.3), learnFunc="Rprop",
          learnFuncParams=c(0.1, 0.1), updateFunc="Topological_Order",
          updateFuncParams=c(0), hiddenActFunc="Act_TanH",
          shufflePatterns=TRUE, linOut=TRUE)

# calcula a saida da rede com os dados de teste
yhat_test <- predict(rede, Xtest)

# calcula a acuracia (taxa de acertos)
correct <- 0
N_test <- length(Ytest)
for (i in 1:N_test) {
  if (sign(yhat_test[i]) == sign(Ytest[i])) {
    correct <- correct + 1
  }
}

acc <- correct / N_test

print(acc)



