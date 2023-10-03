gera_gaussianas_2classes_2D <- function(N,M,SD){
  # N = número de amostras
  # M = vetor com o ponto médio da distribuição. Ex: c(2,2) Para centrar no ponto 2,2
  # SD = dispersão das amostras (a mesma para as duas dimensões)
  
  xc1<-matrix(rnorm(N),ncol=2)*SD + (matrix(M,ncol=2,nrow=N/2, byrow = TRUE))
  xc2<-matrix(rnorm(N),ncol=2)*SD + (matrix(M,ncol=2,nrow=N/2, byrow = TRUE))
  
  y1 = array(1,c(N,1))
  y2 = y1*(-1)
  
  X = rbind(xc1,xc2)
  Y = rbind(y1,y2)
  
  retlist<-list(X,Y)
  
  return(retlist) 
}

###########################################
#plotando as superfícies de contorno
# plot(xc1[,1],xc1[,2],col="red",xlim=c(0,6),ylim=c(0,6))
# par(new=T)
# contour2D(M1,seqi,seqj,levels=0,xlim=c(0,6),ylim=c(0,6))


#############################################

#gerando um grid
# seqi<-seq(0.06,6,0.06)
# seqj<-seq(0.06,6,0.06)
# M1 <- matrix(0,nrow=length(seqi),ncol=length(seqj)) 
# ci<-0
# for (i in seqi){
#   ci<-ci+1
#   cj<-0
#   for(j in seqj)
#   {
#     cj<-cj+1
    
#     M1[ci,cj]<- ALGUM_CALCULO_COM(matrix(c(i,j),nrow=2))
    
#   }
# }

# função radial gaussiana
pdfnvar <- function(x, m, K, n) {
  # x: amostra atual
  # m: centro atual 
  # K: matriz de covariancias
  # n: dimensao do espaço de entrada (com bias, em geral)
  if (n == 1) {
    r <- sqrt(as.numeric(K))
    px <- (1-(sqrt(2 * pi * r *r))) * exp(-0.5 * ((x - m)/(r))^2)
  } else {
    px <- ((1-(sqrt(2 * pi)^n*(det(K)))) * exp(-0.5 * (t(x - m) %*% (solve(K)) %*% (x - m))))
  }

  return (px)
}