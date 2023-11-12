rm(list = ls())
# dev.off()

library("RSNNS")

set.seed(102)

N<-200
x<-runif(N,0,2*pi)
y<-sin(x)+rnorm(N,0,0.1)

plot(
  NULL,
  main = "Treinamento com RSNNS",
  xlab = "x",
  ylab = "sen(x) + ruido",
  ylim = c(-1.2, 1.2),
  xlim = c(0, 2 * pi)
)
legend(x = 4, y = 1.25, legend = c("Treino", "Esperado", "Modelo"), col = c("red", "black", "blue"), pch = c("o", "_", "_"))

points(x, y, col = "red", lwd = 1)

# treinamento da rede
rede<-mlp(x, y, size=c(5,5, 5, 5, 5), maxit=2000, initFunc="Randomize_Weights",
            initFuncParams=c(-0.3, 0.3), learnFunc="Rprop",
            learnFuncParams=c(0.1, 0.1), updateFunc="Topological_Order",
            updateFuncParams=c(0), hiddenActFunc="Act_Logistic",
            shufflePatterns=TRUE, linOut=TRUE)

# teste da rede
xt<-seq(0,2*pi,0.01)
yseno<-sin(xt)
yhat<-predict(rede,as.matrix(xt))

lines(xt, yhat, col = "blue", lwd = 3) # modelo
lines(xt, yseno, col = "black", lwd = 3) # esperado

mse <- sum((yseno - yhat)^2)
print(mse)
