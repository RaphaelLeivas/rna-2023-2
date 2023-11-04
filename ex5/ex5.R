rm(list = ls())
# dev.off()

source("C:\\dev\\rna-2023-2\\utils\\funcoesUteisR.R")

set.seed(209)

# geração dos dados de treinamento
x_train<-seq(from=0, to=2*pi, by =0.15)
x_train<-x_train + (runif(length(x_train))-0.5)/5
i <- sample(length(x_train))
x_train <- x_train[i]
y_train <- sin(x_train)
y_train<-y_train + (runif(length(y_train))-0.5)/5
plot(x_train,y_train,col='blue',xlim = c(0,2*pi), ylim = c(-1,1),xlab = 'x',ylab = 'y')

#geração dos dados de teste
x_test <-seq(from=0, to=2*pi, by =0.01)
y_test <-sin(x_test)
par(new=T)
plot(x_test,y_test,col='red',type='l',xlim = c(0,2*pi), ylim = c(-1,1),xlab = 'x',ylab = 'y')
legend(x=4, y=1, legend = c('train','test'), col = c('blue','red'),pch=c('o','_'))

# dados de treinamento são 42 observações de apenas uma variável: só tem uma dimensão
# a saída também só tem uma dimensão
# converte eles para matriz, para ter certeza 
x_train <- as.matrix(x_train, ncol = 1)
y_train <- as.matrix(y_train, ncol = 1)
x_test <- as.matrix(x_test, ncol = 1)
y_test <- as.matrix(y_test, ncol = 1)

# hiperparametros (argumentos) do treinamento
maxepocas<-3000
tol<-0.01
eepoca<-tol+1
nepocas<-1
eta<-0.05

# inicializa todos os pesos
z10 = 1
z11 = 1
z20 = 1
z21 = 1
z30 = 1
z31 = 1
w30 = 1
w31 = 1
w32 = 1

evec<-matrix(nrow=1,ncol=maxepocas) 
while ((nepocas < maxepocas) && (eepoca>tol))
{
  ei2<-0
  xseq<-sample(length(y_train))       
  for (i in 1:length(y_train))
  {
    irand<-xseq[i]   
    x1<-x_train[irand,1]
    
    h1 = tanh(x1*z11 + z10)
    h2 = tanh(x1*z21 + z20)
    
    yhat = tanh(h1*w31 + w30)
    
    e = y_train[irand] - yhat
    
    de3 = e * (sech2(h1*w31 + h2*w32 + w30))
    
    dw30 = eta* de3 * 1
    dw31 = eta* de3 * h1
    dw32 = eta* de3 * h2
    
    de1 = de3 * w31 * (sech2(x1*z11 + z10))
    dz10 = eta* de1 * 1
    dz11 = eta* de1 * x1
    
    de2 = de3 * w32 * (sech2(x1*z21 + z20))
    dz20 = eta* de2 * 1
    dz21 = eta* de2 * x1
    
    w30 = w30 + dw30
    w31 = w31 + dw31
    w32 = w32 + dw32
    z10 = z10 + dz10
    z11 = z11 + dz11
    z20 = z20 + dz20
    z21 = z21 + dz21
    
    ei2<-ei2+(e*e)/2         
  }
  
  nepocas<-nepocas+1       
  evec[nepocas]<-ei2
  
  eepoca<-evec[nepocas]    
}  

# concluido o treinamento, agora é testar com os dados de teste
# calcula a saida da rede

yhat_list <- c()
x_test_length <- length(x_test)
for (i in 1:x_test_length) {
  x1<-x_test[i,1]
  
  h1 = tanh(x1*z11 + z10)
  h2 = tanh(x1*z21 + z20)
  
  yhat = tanh(h1*w31 + h2*w32 + w30)
  yhat_list <- append(yhat_list, yhat)
}

points(x_test, yhat_list, col = "black")




