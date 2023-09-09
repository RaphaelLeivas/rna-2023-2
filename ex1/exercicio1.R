rm(list = ls())
# dev.off()

library("corpcor") # usado para função da pseudoinversa

fgx <- function(xin) {
  (0.5 * xin^2) + (3 * xin) + 10
}

NUMBER_OF_SAMPLES <- 10

X <- runif(n = NUMBER_OF_SAMPLES, min = -15, max = 10) # vetor aleatorio
Y <- fgx(X) + 10 * rnorm(length(X), mean = 0, sd = 4) # dados de aprendizado
par(mfrow = c(2, 4))

for (i in 1:8) {
  POLYNOMIAL_DEGREE <- i
  
  switch(POLYNOMIAL_DEGREE,
         H <- cbind(X^1, X^0),
         H <- cbind(X^2, X^1, X^0),
         H <- cbind(X^3, X^2, X^1, X^0),
         H <- cbind(X^4, X^3, X^2, X^1, X^0),
         H <- cbind(X^5, X^4, X^3, X^2, X^1, X^0),
         H <- cbind(X^6, X^5, X^4, X^3, X^2, X^1, X^0),
         H <- cbind(X^7, X^6, X^5, X^4, X^3, X^2, X^1, X^0),
         H <- cbind(X^8, X^7, X^6, X^5, X^4, X^3, X^2, X^1, X^0),
  )
  
  # %*% é multiplicação matricial
  w <- pseudoinverse(H) %*% Y
  
  # aqui, w sao os parametros aprendidos. (vetor de parametros w)
  
  ## --- aprendizado da RNA se encerra aqui. --- ##
  ##  agora, é somente teste com os parametros aprendidos w
  
  xgrid <- seq(-15, 10, 0.1)
  ygrid <- ((0.5 * xgrid^2) + (3 * xgrid) + 10)
  
  switch(POLYNOMIAL_DEGREE,
         Hgrid <- cbind(xgrid^1, xgrid^0),
         Hgrid <- cbind(xgrid^2, xgrid^1, xgrid^0),
         Hgrid <- cbind(xgrid^3, xgrid^2, xgrid^1, xgrid^0),
         Hgrid <- cbind(xgrid^4, xgrid^3, xgrid^2, xgrid^1, xgrid^0),
         Hgrid <- cbind(xgrid^5, xgrid^4, xgrid^3, xgrid^2, xgrid^1, xgrid^0),
         Hgrid <- cbind(xgrid^6, xgrid^5, xgrid^4, xgrid^3, xgrid^2, xgrid^1, xgrid^0),
         Hgrid <- cbind(xgrid^7, xgrid^6, xgrid^5, xgrid^4, xgrid^3, xgrid^2, xgrid^1, xgrid^0),
         Hgrid <- cbind(xgrid^8, xgrid^7, xgrid^6, xgrid^5, xgrid^4, xgrid^3, xgrid^2, xgrid^1, xgrid^0),
  )
  
  yhatgrid <- Hgrid %*% w
  
  # yhatgrid é y chapeu: é a saida aprendida da maquina
  plot(
    NULL,
    main = if (POLYNOMIAL_DEGREE == 1) {
      "Polinômio Grau 1"
    } else if (POLYNOMIAL_DEGREE == 2) {
      "Polinômio Grau 2"
    } else if (POLYNOMIAL_DEGREE == 3) {
      "Polinômio Grau 3"
    } else if (POLYNOMIAL_DEGREE == 4) {
      "Polinômio Grau 4"
    } else if (POLYNOMIAL_DEGREE == 5) {
      "Polinômio Grau 5"
    } else if (POLYNOMIAL_DEGREE == 6) {
      "Polinômio Grau 6"
    } else if (POLYNOMIAL_DEGREE == 7) {
      "Polinômio Grau 7"
    } else if (POLYNOMIAL_DEGREE == 8) {
      "Polinômio Grau 8"
    },
    xlab = "Eixo X",
    ylab = "Eixo Y",
    ylim = c(-100, 200),
    xlim = c(-15, 10)
  )
  lines(xgrid, yhatgrid, col = "red", lwd = 2)
  lines(xgrid, ygrid, col = "blue", lwd = 2)
  points(X, Y, col = "black", lwd = 2)
  
  # legend(
  #     "bottom",
  #     inset = c(-0.3, 0.1),
  #     legend = c(
  #         "Função geradora",
  #         "Polinômio aprendido pela RNA",
  #         "Dados de amostragem"
  #     ),
  #     fill = c("blue", "red", "black")
  # )
}
