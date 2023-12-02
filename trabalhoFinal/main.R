rm(list = ls())
# dev.off()

library("RSNNS")

set.seed(203)

trainReduzidoPath <- "C:\\dev\\rna-2023-2\\trabalhoFinal\\datasets\\trainReduzido.csv"
validacaoPath <- "C:\\dev\\rna-2023-2\\trabalhoFinal\\datasets\\validacao.csv"
mlpSubmissionPath <- "C:\\dev\\rna-2023-2\\trabalhoFinal\\datasets\\submissionMlp.csv"

trainReduzido <- read.csv(file = trainReduzidoPath, header = T)
validacao <- read.csv(file = validacaoPath, header = T)

# schuffle inicial nos dados, por linha
trainReduzido <- trainReduzido[sample(1:nrow(trainReduzido)), ]

# separa as saidas esperadas de cada observação
rotulos <- trainReduzido[, "label"]
dados <- trainReduzido[, 3:786] / 255

# numero de folds
numFolds <- 5

# separa os dados em folds
rowIndices <- 1:nrow(trainReduzido)
folds <- split(rowIndices, sample(1:length(rowIndices), size = numFolds))

accArray <- c()
for (f in 1:numFolds) {
  Xtrain = dados[-folds[[f]],]
  Ytrain = rotulos[-folds[[f]]]
  Xtest = dados[folds[[f]],]
  Ytest = rotulos[folds[[f]]]

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

  accArray <- append(accArray, correct / length(Ytest))
}

barplot(accArray * 100,
        main = "Acurácia por iteração",
        xlab = "Iteração",
        ylab = "Acurácia (%)",
        names.arg = c("1", "2", "3", "4", "5"),
        ylim=c(0,100),
        col = "green")

msg1 <- paste(mean(accArray), " +- ", sd(accArray))
msg2 <- paste("Max: ", max(accArray))
print(msg1)
print(msg2)

# dados de validacao
Xvalid <- validacao[, 2:785] / 255

Yvalid <- predict(rede, Xvalid)
Yvalid <- round(Yvalid)

df <- data.frame(ImageId = 1:4000,
                 Label = Yvalid
)

write.csv(df, mlpSubmissionPath, row.names=FALSE)
