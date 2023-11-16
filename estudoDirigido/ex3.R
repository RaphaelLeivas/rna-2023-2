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

N <- dim(data2)[1] # numero de observações
n <- dim(data2)[2] # numero de variaveis
n <- n - 2 # tira as colunas de id e class ao final

numberOfIterations <- 10
acc_list <- c()

for (sim in 1:numberOfIterations) {
  # seleciona 70% para treinamento e 30% para teste, aleatoriamente
  sample <- sample(c(TRUE, FALSE), nrow(data2), replace = TRUE, prob = c(0.7, 0.3))
  Xtrain <- data2[sample, 2:n]
  Xtest <- data2[!sample, 2:n]
  Ytrain <- data2[sample, ncol(data2)]
  Ytest <- data2[!sample, ncol(data2)]

  # converte para 0 (maligno) e 1 (benigno)
  for (i in 1:length(Ytrain)) {
    if (Ytrain[i] == "benign") {
      Ytrain[i] <- 1
    } else {
      Ytrain[i] <- 0
    }
  }

  for (i in 1:length(Ytest)) {
    if (Ytest[i] == "benign") {
      Ytest[i] <- 1
    } else {
      Ytest[i] <- 0
    }
  }

  # garante que todos sao numericos
  class(Xtrain) <- "numeric"
  class(Xtest) <- "numeric"
  class(Ytrain) <- "numeric"
  class(Ytest) <- "numeric"

  # aqui finalizou o tratamento dos dados. treina a rede
  rede<-mlp(Xtrain, Ytrain, size=10, maxit=1000, initFunc="Randomize_Weights",
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
    # a rede calcula um valor entre 0 e 1. 
    # se esta mais proximo de 0, ela diz que é malgino
    # se esta mais proximo de 1, ela diz que é benigno
    # usa round para arrendondar para 0 ou 1
    if (round(yhat_test[i]) == Ytest[i]) {
      correct <- correct + 1
    }
  }

  acc <- correct / N_test

  acc_list <- append(acc_list, acc)
}

averageAccuracy <- mean(acc_list)
standardDeviation <- sd(acc_list)

msg <- paste(averageAccuracy, " +- ", standardDeviation)
print(msg)

barplot(acc_list * 100,
        main = "Acurácia por iteração",
        xlab = "Iteração",
        ylab = "Acurácia (%)",
        names.arg = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
        ylim=c(0,100),
        col = "darkred")






