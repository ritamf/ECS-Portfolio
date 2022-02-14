# Clear all variables
rm(list=ls())
#Get path
path<-getwd(); path
# Set path
setwd(path)

dados <- read.table(file="caudais.txt", sep=" ", header=TRUE, dec=".")

set.seed(1234)

g <- function(sigma,dados,x0,m){
  
  m<-m #comprimento da cadeia
  sigma<-sigma
  xt<-numeric(m)
  
  u<-runif(m) #gerar NPA U ~ U(0,1)
  xt[1]<-rchisq(1,df=x0) #valor inicial gerado a partir da fun��o proponente, guardado em xt[1]
  count<-0
  
  for (i in 2:m) {
    y<-rchisq(1,df=abs(xt[i-1]))
    
    num<-exp(sum(-(dados-y)/sigma)-sum(exp(-(dados-y)/sigma)))*dchisq(xt[i-1],df=abs(y))
    den<-exp(sum(-(dados-xt[i-1])/sigma)-sum(exp(-(dados-xt[i-1])/sigma)))*dchisq(y,df=abs(xt[i-1]))
    
    alpha<-num/den
    
    if (u[i]<=alpha) {
      xt[i]=y
    }
    else{
      xt[i]=xt[i-1]
      count=count+1
    }
  }
  print(count)
  print(count/m) 
  return(xt)
}

m=10000
sigma=16.65
xt<-g(sigma,dados,1,m)
mean(xt) 

y1<-xt[9500:m]    #valores da itera��o 9500-10000

par(mfrow=c(1,1))
#sobrepondo:
hist(y1, freq=FALSE,main="Histograma da cadeia de Markov de 9500:10000",col="lightskyblue3")
lines(density(y1),col="red",lwd=2)
text(22,0.14,"Estimativa da Densidade \ndo N�cleo",cex=1.1, col="red")

#Representa��o de uma por��o da cadeia de Markov gerada:
plot(9500:m,xt[9500:m],ty="l",lwd=2,xlab="Itera��es",ylab="X")
title(main=" MCMC - Metropolis-Hastings\nRepresenta��o de uma por��o da cadeia de Markov")

library(coda)

x0 <- c(2,8,15,24) # valores iniciais dispersos

# ----------cadeias iniciais-----------#
X <- matrix(0, nrow=4, ncol=m)


# ----------subcadeias-----------------#
lag=100
b=2000
indices=seq(b,m,by=lag)
XX <- matrix(0, nrow=4, ncol=length(indices))


for (i in 1:4){
  X[i, ] <- g(sigma, dados, x0[i],m)
  
  for(j in 1:length(indices)) {
    XX[i,j ]<-X[i,indices[j]]
  }
  
}

# ----------plot acf das iniciais---------#
par(mfrow=c(4,2))

for (i in 1:4){ 
  acf(X[i, ]) #acf � a fun��o de autocorrela��o
}

# ----------plot acf das subcadeias-------#
for (i in 1:4){
  acf(XX[i, ])
}

library(coda)

#---------- plot das traject�rias e densidades-----------#
plot(mcmc(XX[1, ],start=b,thin=lag)) #traj�torias e densidades

#---------- plot dos percentis-----------#
cumuplot(mcmc(XX[1, ],start=b,thin=lag)) #evolu��o dos percentis

#MONOTORIZA��O DA CONVERG�NCIA

geweke.plot(mcmc(XX[1, ]))
geweke.diag(mcmc(XX[1, ]))

gelman.plot(mcmc.list(mcmc(X[1, ]),mcmc(X[2, ]),mcmc(X[3, ]),mcmc(X[4, ])))
gelman.diag(mcmc.list(mcmc(X[1, ]),mcmc(X[2, ]), mcmc(X[3, ]),mcmc(X[4, ])))

