rm(list = ls())
# dev.off()

set.seed(203)

trainReduzidoPath <- "C:\\dev\\rna-2023-2\\trabalhoFinal\\datasets\\trainReduzido.csv"
validacaoPath <- "C:\\dev\\rna-2023-2\\trabalhoFinal\\datasets\\validacao.csv"

trainReduzido <- read.csv(file = trainReduzidoPath, header = T)
validacao <- read.csv(file = validacaoPath, header = T)

# separa as saidas esperadas de cada observação
Ytrain <- trainReduzido[, "label"]
Xtrain <- trainReduzido[, 3:786]

# seleciona só uma parte para treinamento, o dataset de entrada é grande demais
percentage <- 0.05
N <- dim(Xtrain)[1] # numero de observacoes de treinamento
selectedRows <- sample(1:N, size = percentage * N)
Ytrain <- Ytrain[selectedRows]
Xtrain <- Xtrain[selectedRows,]

# dados de validacao
Xvalid <- validacao[, 2:785]

# chama a rede 
rede<-mlp(Xtrain, Ytrain, size=c(3, 3), maxit=1000, initFunc="Randomize_Weights",
          initFuncParams=c(-0.3, 0.3), learnFunc="Rprop",
          learnFuncParams=c(0.1, 0.1), updateFunc="Topological_Order",
          updateFuncParams=c(0), hiddenActFunc="Act_TanH",
          shufflePatterns=TRUE, linOut=TRUE)

yhat <- predict(rede, Xvalid)
