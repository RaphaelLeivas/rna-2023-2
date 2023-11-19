rm(list = ls())
# dev.off()

library("RSNNS")

set.seed(203)

trainReduzidoPath <- "C:\\dev\\rna-2023-2\\trabalhoFinal\\datasets\\trainReduzido.csv"
validacaoPath <- "C:\\dev\\rna-2023-2\\trabalhoFinal\\datasets\\validacao.csv"

trainReduzido <- read.csv(file = trainReduzidoPath, header = T)
validacao <- read.csv(file = validacaoPath, header = T)

# separa as saidas esperadas de cada observação
Ytrain <- trainReduzido[, "label"]
Xtrain <- trainReduzido[, 3:786] / 255

# seleciona só uma parte para treinamento, o dataset de entrada é grande demais
percentage <- 0.80
N <- dim(Xtrain)[1] # numero de observacoes de treinamento

sample <- sample(c(TRUE, FALSE), nrow(Xtrain), replace = TRUE, prob = c(percentage, 1 - percentage))
Xtest <- Xtrain[!sample,]
Xtrain <- Xtrain[sample,]
Ytest <- Ytrain[!sample]
Ytrain <- Ytrain[sample]

# selectedRows <- sample(1:N, size = percentage * N)
# Ytrain <- Ytrain[selectedRows]
# Xtrain <- Xtrain[selectedRows,]

# dados de validacao
Xvalid <- validacao[, 2:785] / 255

# chama a rede 
rede<-mlp(Xtrain, Ytrain, size=c(10, 10), maxit=500, initFunc="Randomize_Weights",
          initFuncParams=c(-0.3, 0.3), learnFunc="Rprop",
          learnFuncParams=c(0.1, 0.1), updateFunc="Topological_Order",
          updateFuncParams=c(0), hiddenActFunc="Act_Logistic", linOut = TRUE,
          shufflePatterns=TRUE)

yhat <- predict(rede, Xtest)

# calcula a acuracia de classificação da rede
correct <- 0
for (i in 1:length(Ytest)) {
  if (round(yhat[i]) == Ytest[i]) {
    correct <- correct + 1
  }
}

print(correct / length(Ytest))

