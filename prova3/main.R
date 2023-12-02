rm(list = ls())
# dev.off()

source("C:\\dev\\rna-2023-2\\utils\\funcoesUteisR.R")

set.seed(203)

# ------ PRIMEIRA QUESTAO -------

# x <- matrix(c(2,0,2,0,0,2,8,1,0,1), nrow = 10, ncol = 1)
# w <- matrix(c(2.0, 0.5, 1.8, 7.0, 2.0, 5.5, 0.2, 7.0, 2.6, 1.3, 0.4), nrow = 1, ncol = 11)

x <- c(2,0,2,0,0,2,8,1,0,1)
w <- c(2.0, 0.5, 1.8, 7.0, 2.0, 5.5, 0.2, 7.0, 2.6, 1.3, 0.4)

yhat <- 0
for (i in 1:length(x)) {
  yhat <- yhat + x[i] * w[i]
}

yhat <- yhat + w[length(w)]

print(yhat)

# ------ SEGUNDA QUESTAO -------

h_i <- function(x, u, r) {
  return (exp(-(0.5) * ((x-u) / (r))^2))
}

lastMat <- 1
h1 <- h_i(lastMat, 2.3, 1.2)
h2 <- h_i(lastMat, 5.7, 0.6)

q2 <- h1 + h2

# ------ TERCEIRA QUESTAO -------





