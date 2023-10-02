rm(list = ls())
# dev.off()

library("corpcor") # usado para função da pseudoinversa
source("C:\\dev\\rna-2023-2\\utils\\funcoesUteisR.R")
source("C:\\dev\\rna-2023-2\\utils\\trainadaline.R")
source("C:\\dev\\rna-2023-2\\utils\\trainperceptron.R")

# set.seed(203)

N <- 200 # numero de amostras de cada classe

retlist <- gera_gaussianas_2classes_2D(N, c(2,2), 0.4)
xClass1 <- retlist[[1]]
yClass1 <- retlist[[2]]

retlist <- gera_gaussianas_2classes_2D(N, c(4,4), 0.4)
xClass2 <- retlist[[1]]
yClass2 <- retlist[[2]]

# cria a matriz X com todos os meus dados de entrada e a coluna de bias
joinedClasses <- rbind(xClass1, xClass2)
X <- matrix(cbind(joinedClasses, rep(1, N)), nrow = 2 * N, ncol = 3)

# cria a matriz Y com todas as saidas esperadas
# os primeiros N elementos da classe 1 são Y = 1 (eu defini isso)
# e os proximos N elementos da classe 2 são Y = -1
Y <- matrix(cbind(rep(1, N), rep(-1, N)), nrow = 2 * N, ncol = 1)

# tenho a matrix X, e tendo a matriz Y. os pesos sao obtidos via pseudoinversa
W <- pseudoinverse(X) %*% Y

# yhatgrid é y chapeu: é a saida aprendida da maquina
plot(
  NULL,
  main = "Adaline via pseudoinversa, gradiente e perceptron",
  xlab = "x1",
  ylab = "x2",
  ylim = c(0, 6),
  xlim = c(0, 6)
)

points(X[1:N, ], lwd = 1, col = "green")
points(X[(N+1):(2*N), ], lwd = 1, col = "blue")

# agora define a superficie (plano) de separacao
xgrid <- seq(from = 0, to = 6, by = 0.1)
# w2 x2 + w1 x1 + w0 = 0 -> essa e a equaçao do hiperplano de separação
# cuidado com a ordem dos w dentro do vetor: olha a equação que ele resolveu
# para identificar quem é
w2 <- W[2]
w1 <- W[1]
w0 <- W[3] # ultimo é o bias por causa da coluna 1 na ultima coluna de X
ygrid <- (1 / w2) * (- w1 * xgrid - w0)

# lines(xgrid, ygrid, lwd = 2, col = "red")

# aprende os parametros pelo gradiente descedente

retlist <- trainadaline(joinedClasses, Y, 0.01, 0.01, 1000, TRUE)
W_g <- retlist[[1]]
errorGradient <- retlist[[2]]

w2_g <- W_g[3]
w1_g <- W_g[2]
w0_g <- W_g[1]
ygrid_g <- (1 / w2_g) * (- w1_g * xgrid - w0_g)

lines(xgrid, ygrid_g, lwd = 2, col = "purple")

# agora via perceptron
retlist <- trainperceptron(joinedClasses, Y, 0.1, 0.01, 100, TRUE)
W_p <- retlist[[1]]
errorGradient <- retlist[[2]]

w2_p <- W_p[3]
w1_p <- W_p[2]
w0_p <- W_p[1]
ygrid_p <- (1 / w2_p) * (- w1_p * xgrid - w0_p)

# lines(xgrid, ygrid_p, lwd = 2, col = "orange")
