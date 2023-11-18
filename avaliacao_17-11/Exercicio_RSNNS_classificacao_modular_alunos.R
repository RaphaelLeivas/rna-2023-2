rm(list=ls())
dev.off()

library("RSNNS")
library("mlbench")

geraDados <- function(){
  data("Sonar") 
  dados = Sonar
  colrotulo = 61  #qual a coluna que tem o rótulo
  
  # Set the number of folds
  numFolds <- 10
  
  # Split the data into folds
  rowIndices <- 1:nrow(dados)
  folds <- split(rowIndices, sample(1:length(rowIndices), size = numFolds))
  
  rotulos = sign(as.numeric(dados[,colrotulo])-1.5)
  dados = dados[,-colrotulo]
  return(list(dados,rotulos,folds))
}


treinaRede_MLP <- function(x,y){
  
  
 # você deve implementar sua rede dentro desta função 
  #ela receberá x e y de treinamento e retornará o modelo 
  rede<-mlp(x, y, size=10, maxit=1000, initFunc="Randomize_Weights",
            initFuncParams=c(-0.3, 0.3), learnFunc="Rprop",
            learnFuncParams=c(0.1, 0.1), updateFunc="Topological_Order",
            updateFuncParams=c(0), hiddenActFunc="Act_TanH",
            shufflePatterns=TRUE, linOut = TRUE)
  
  #rede = ......
  
  
  return(rede)
}

#dados
DADOS = geraDados()
dados = DADOS[[1]]
rotulos = DADOS[[2]]
folds = DADOS[[3]]

nfolds = length(folds)
acc = matrix(0,nrow = 1,ncol = nfolds) #matriz de Risco total

for (f in 1:nfolds) {
  f
  xtrain = dados[-folds[[f]],]
  ytrain = rotulos[-folds[[f]]]
  xtest = dados[folds[[f]],]
  ytest = rotulos[folds[[f]]]
  
  #treina a rede
  rede = treinaRede_MLP(xtrain,ytrain)
  
  #testa no conjunto de teste e armazena acurácia
  yhat<-sign(predict(rede,xtest))
  
  result = table(ytest,yhat)
  acc[f] = sum(diag(result))/sum(result)
} 

barplot(acc * 100,
        main = "Acurácia por iteração",
        xlab = "Iteração",
        ylab = "Acurácia (%)",
        names.arg = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
        ylim=c(0,100),
        col = "darkred")

msg1 <- paste(mean(acc), " +- ", sd(acc))
msg2 <- paste("Max: ", max(acc))
print(msg1)
print(msg2)
