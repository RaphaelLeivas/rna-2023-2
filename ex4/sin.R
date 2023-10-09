rm(list = ls())
# dev.off()

source("C:\\dev\\rna-2023-2\\utils\\rbf.R")
source("C:\\dev\\rna-2023-2\\utils\\funcoesUteisR.R")

# note que xin é uma matriz com 1 coluna -> dimensão de entrada é somente 1
# cada linha é uma observação de 1 variavel, quq queremos interpolar
xin <- matrix(seq(0, 2 * pi, 0.01 * pi), ncol = 1)
yin <- matrix(sin(xin), ncol = 1)

modRBF <- trainRBF(xin, yin, 2)
xrange <- matrix(seq(0, 2 * pi, 0.001 * pi), ncol = 1)
yhat_teste <- YRBF(xrange, modRBF)

plot(
  NULL,
  main = "Aproximação sen(x) via RBF",
  xlab = "x",
  ylab = "y = f(x), yhat",
  ylim = c(-1, 1),
  xlim = c(0, 2 * pi)
)

points(xin, yin, lwd = 1, col = "black")
lines(xrange, yhat_teste, col = "red")

