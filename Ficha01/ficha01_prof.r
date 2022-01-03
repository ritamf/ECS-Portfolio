#######################################################################################

####################### Folha 1 ############################################################################################################
##############Estat�stica Computacional e Simula��o#########################

###########################################################################



############# exerc�cio 1 ############################################


###### 1.1  Gerar valores de distribui��o normal

#### gerar valores de dist N(0,1)
rnorm (100)

#### gerar valores de dist N(2,4)
gerados_cem<-rnorm(100,2,2)  #mostrar dados


#### alinea a

summary(gerados_cem)
sd(gerados_cem)
var(gerados_cem)



##### alinea b

fdp<-dnorm(gerados_cem,2,2)  #valores da fdp dos valores gerados
fdp
plot(gerados_cem, fdp, main="valores da fdp dos valores gerados de N(2,4)")

fd<-pnorm(gerados_cem,2,2)  #valores da fd dos valores gerados
fd	
plot(gerados_cem, fd, main="valores da fd de valores gerados de N(2,4)")


##### alinea c

hist(gerados_cem)  # frequencias absolutas
hist(gerados_cem, prob = TRUE,main = NULL,ylim=c(0,0.25))  # frequencias relativas
text(1.5,0.25,"100 NPA's NORMAL(2,2)")

#alternativamente
hist(gerados_cem, prob = TRUE,main = "100 NPA's NORMAL(2,2)",ylim=c(0,0.25))  # frequencias relativas



####### d  considerar uma sequencia de valores 


####   alinea  i  
###calcular valores da fdp da normal usando uma sequencia e construindo a fdp 

sequencia<-seq(-3.5,9.0,0.5)  #sequencia de valores 
sequencia

densidade_sequencia<-dnorm(sequencia,2,2)
densidade_sequencia
plot(densidade_sequencia)


	#ou calculo da fdp usando a express�o e n�o o comando:

		dens.norm<-function(u,mu,sigma){1/(sigma*(2*pi)**0.5)*exp(-1/(2*sigma**2)*(u-mu)**2)}
		w<- dens.norm(sequencia,2,2)
		w

	# mostrar o grafico da fun��o, com titulo e legendas:
		plot(sequencia,w,main="dens.norm",xlab="z",ylab="f(z)")


####   alinea ii , graficos sobrepostos

## amostra de dimens�o 100, designada gerados_cem

hist(gerados_cem)  # frequencias absolutas
hist(gerados_cem, prob = TRUE,main = "100 NPA's NORMAL(2,2)",ylim=c(0,0.25))  # frequencias relativas

z<-sequencia<-seq(-3.5,9.0,by=0.5)
dnorm(sequencia,2,2)
lines(z,dnorm(z,2,2),col="red")




## amostra de dimens�o 1000 , designada gerados

rnorm(10000, mean=2, sd=2)
gerados_mil<-rnorm(10000,2,2)
summary(gerados_mil)
hist(gerados_mil, prob = TRUE,main = "1000 NPA's NORMAL(2,2)",ylim=c(0,0.25))

t<-seq(-6.0,10.0,by=0.5)
t
dnorm(t,2,2)
lines(t,dnorm(t,2,2),col="red")



####################################################################

############# exerc�cio 2 ############################################

######################################################################

########### gerar valores da Bi(100,0.25)	

rbinom(200,100,0.25)

binomial<-rbinom(200,100,0.25)
binomial

####  alinea a

summary(binomial)  ### caracteristicas sum�rias

#calcular os valores da fun��o distribui��o nos valores gerados 
pbinom(binomial,100,0.25)
pbinom(200, 100,0.25)
pbinom(0, 100,0.25)

#calcular os valores da fun��o probabilidade nos valores gerados: 
dbinom(binomial,100,0.25)
dbinom(200, 100,0.25)
dbinom(0, 100,0.25)


### Representa��o grafica da fun��o de probabilidade



hist(binomial) #trata os dados como se fossem continuos-s� em grandes amostras- a evitar
barplot(binomial) #n�o apropriado

par(mfrow=c(1,2)) 

barplot(table(binomial))  #este � apropriado, faz a contagem das frequencias absolutas

barplot(table(binomial)/length(binomial))  #usando as frequencias relativas


###############################################################################

#################### alinea b

# Exerc�cio semelhante � aula:Se pretendermos gerar 5000 valores da binomial(100,0.25)
#OU b(10,0.25)- Representar a fmp e valores de probabilidade de uma sequencia gerada


par(mfrow = c(2,2))


# 5000 NPA's BINOMIAL(10,0.25)
n=5000
k=10
rbinom(n,size=k,prob=0.25)
plot(table(rbinom(n,size=k,prob=0.25))/n,xlim=c(0,k),ylim=c(0,0.35), 
lwd=8,col="grey")
title(main = list("5000 NPA's BINOMIAL(10,0.25)", cex=1.0, col="grey", font=1))

# f.m.p.BINOMIAL(10,0.25)
t<-seq(0,k,by=1)
t
lines(t,dbinom(t,size=k, prob=0.25),lwd=3,col="red")
text(k/2,0.33,"f.m.p.BINOMIAL(10,0.25)", cex=1, font=2,col="red")
dbinom(9,10,0.25)
dbinom(10,10,0.25)

# 5000 NPA's BINOMIAL(50,0.25)
n=5000
k=50
plot(table(rbinom(n,size=k,prob=0.25))/n, type = "h",xlim=c(0,k),ylim=c(0,0.18), lwd=8,col="grey")
title(main = list("5000 NPA's BINOMIAL(50,0.25)", cex=1.5, col="grey", font=1))
# f.m.p.BINOMIAL(50,0.25)
t<-seq(0,k,by=1)
t
lines(t,dbinom(t,size=k, prob=0.25),type="h", lwd=3,col="red")
text(k/2,0.17,"f.m.p.BINOMIAL(50,0.25)", cex=1, font=2,col="red")


# 5000 NPA's BINOMIAL(100,0.25)
n=5000
k=100
plot(table(rbinom(n,size=k,prob=0.25))/n, type = "h",xlim=c(0,k),ylim=c(0,0.12), lwd=10,col="grey")
title(main = list("5000 NPA's BINOMIAL(100,0.25)", cex=1.0, col="grey", font=1))
# f.m.p.BINOMIAL(100,0.25)
t<-seq(0,k,by=1)
lines(t,dbinom(t,size=k, prob=0.25),type="h", lwd=3,col="red")
text(k/2,0.11,"f.m.p.BINOMIAL(100,0.25)", cex=1, font=2,col="red")



#sem usar tantos valores com dp nula
# 5000 NPA's BINOMIAL(100,0.25)
n=5000
k=100
plot(table(rbinom(n,size=k,prob=0.25))/n, type = "h",xlim=c(0,50),ylim=c(0,0.12), lwd=10,col="grey")
title(main = list("5000 NPA's BINOMIAL(100,0.25)", cex=1.0, col="grey", font=1))
# f.m.p.BINOMIAL(100,0.25)
t<-seq(8,50,by=1)
lines(t,dbinom(t,size=k, prob=0.25),type="h", lwd=3,col="red")
text(k/4,0.11,"f.m.p.BINOMIAL(100,0.25)", cex=1, font=2,col="red")




#############  alinea c
###(ficou para trabalho de casa)


rbinom(5000, 100, 0.25)
bin<-rbinom(5000, 100, 0.25)

plot(table(rbinom(5000,size=100,prob=0.25))/5000, type = "h",xlim=c(0,50),ylim=c(0,0.12), lwd=10,col="grey")
title(main = list("5000 NPA's BINOMIAL(100,0.25)", cex=1.0, col="grey", font=1))

rnorm(5000,25, sqrt(18.75))
norm<-rnorm(5000, 25, sqrt(18.75))


hist(norm, prob = TRUE,ylim=c(0,0.12),  lwd=1,col="red")  # frequencias relativas
text(28,0.11,"5000 NPA's Normal")




####################################################################

############# exerc�cio 3 ############################################

######################################################################

################### comandos do R para Pareto Pa(a,b); 
################### a par�metro de forma e b par�metro de escala.

install.packages("extremefit")
library(extremefit)
#na package,a distribui��o  Pareto definida por: pareto(shape, loc, scale)
#neste caso loc=0 

############### sem usar os comandos do R
########### alinea a	

install.packages("extremefit")
library(extremefit)

#a1-escrever o valor da f.d.p da pareto numa sequencia de valores

dens.paret<-function(u,alpha,beta){
+ alpha*(beta**alpha)*u**(-(alpha+1))}

dens.paret(5,3,1.5) #fdp da Pareto(3,1.5) em x=5

#usando comando do R
  dpareto(5,3,0,1.5)


z<- seq(1.5,10,0.01)#sequencia iniciando em 1.5 at� 10, passo 0.01
z

w<-dens.paret(z,3,1.5) #obter valores da fdp  calculada na sequencia z
w



# mostra o grafico da fun��o, com titulo e legendas
 plot(z,w,main="dens.pareto",xlab="z",ylab="f(z)")

#usando comando do R, para calcular fdp calculada na sequencia z
dpareto(z,3,0,1.5)



#a2-escrever o valor da fd da pareto numa sequencia de valores

dist.paret<-function(u,alpha,beta){
+ 1-(beta**alpha)*(u**(-alpha))}

z<- seq(1.5,10,0.01)

y<-dist.paret(z,3,1.5)
y

legenda<-"dist.pareto"

# mostra o grafico da fun��o, com titulo e legendas
 plot(z,y,main=legenda,xlab="z",ylab="F(z)")


#a3-quantis
####escrevendo a express�o que permite calcular os quantis

quant.paret<-function(p,alpha,beta){
    + beta*(1-p)**(-1/alpha)}

   q1<- quant.paret(1/4,3,1.5) #1� quartil

 q1


   med<- quant.paret(1/2,3,1.5)  #mediana

med

   q3<- quant.paret(3/4,3,1.5)  #3� quartil

 q3



	#qpareto(0.25,3,0,1.5)


 d3<- quant.paret(0.3,3,1.5) # 3� decil 
d3

#usando comando do R, 
qpareto(0.3,3,0,1.5)#3�decil
qpareto(0.25,3,0,1.5)# 1�quartil



#a4-gera��o de n�s aleat�rios- metodo da transforma��o inversa

   #1� gerar uniformes (0,1)

   runif (100)

   u<-runif(100)


   # 2� gerar valores da Pareto
   
    val.paret<-function(u,alpha,beta){
    + beta*(1-u)**(-1/alpha)}

   x<- val.paret(u,3,1.5)

   x


#b) graficos da f.d.p. da Pareto com diferentes valores de alpha e beta
   
#fazer como TPC

#c) mostrar existencia ou n�o do valor medio
#gera��o de n�s aleat�rios

   #1� gerar uniformes (0,1)

   runif (1000)
  y<-runif(1000)
  
   # 2� gerar valores da Pareto

alpha<-3
beta<-1.5
   
    val.paret<-function(y,alpha,beta){
    + beta*(1-y)**(-1/alpha)}

   x<- val.paret(y,alpha,beta)

mean(x) #media amostral dos valores gerados
var(x)  #variancia amostral dos valores gerados

valormed<-(alpha*beta)/(alpha -1)#express�o de E(X)
valormed

variancia<-(alpha*beta**2)/((alpha-2)*(alpha -1)**2) #exp. de V(X)
variancia

#desv<-sqrt(variancia)
#desv
summary(x)

### repetindo v�rias vezes verifica-se que nem a nem a variancia  m�dia amostrais variam muito



###outra gera��o com outro parametro para alfa 
   #1� gerar uniformes (0,1)

   runif (1000)

   y<-runif(1000)

   y

   # 2� gerar valores da Pareto
   
    val.paret<-function(y,alpha,beta){
    + beta*(1-y)**(-1/alpha)}

   x<- val.paret(y,0.5,1.5)

   x
mean(x)
#summary(x)
var(x)
alpha<-0.5
beta<-1.5

## Conclus�o:grande instabilidade para media e variancia amostrais






####gera��o com alpha >1 e <2 (existe media mas n�o variancia)
#gera��o de n�s aleat�rios

   #1� gerar uniformes (0,1)

   runif (1000)

   y<-runif(1000)

   y

   # 2� gerar valores da Pareto
   
    val.paret<-function(y,alpha,beta){
    + beta*(1-y)**(-1/alpha)}

   x<- val.paret(y,1.5,10)

   x

summary(x)
sd(x)




##### gera��o com parametro (alpha>2)
#gera��o de n�s aleat�rios

   #1� gerar uniformes (0,1)

   runif (1000)

   y<-runif(1000)

   y

   # 2� gerar valores da Pareto
   
    val.paret<-function(y,alpha,beta){
    + beta*(1-y)**(-1/alpha)}

   x<- val.paret(y,3,1.5)

   x

summary(x)
sd(x)

#conclus�o: ambas media e desvio padr�o s�o est�veis



#OBS: comparar com o valor dado pela express�o te�rica




###########################################################################

#################  exercicio 4 ############################################

###########################################################################

###### a.  calcular valores da fun��o densidade de probabilidade da logistica

#####   escrevendo a  express�o da fun��o

dens.logist<-function(u,alpha,beta){
+ beta**(-1)* (exp(-(u-alpha)/beta))/((1+exp(-(u-alpha)/beta))**2)}

z<- seq(-10,10,0.1)
z

w<-dens.logist(z,1,2)
w





leg<-"dens.logist"

 #plot(z,w,main=leg,xlab="z",ylab="f(z)")

 plot(z,w,main="dens.logist",xlab="z",ylab="f(z)")


###### usando o comando do R

dlogis(3, 1,2)
w3<-dens.logist(3,1,2)
w3
#0.0983

dlogis(5,1,2)
#0.052

dlogis(0,1,2)
#0.117

dlogis(-5,1,2)
#0.022

dlogis(-1,1,2)
#0.098

dlogis(2,1,2)
#0.117

x<-seq(-10,10,0.1)  #usando o c�digo
dlogis(x,1,2)
w<-dlogis(x,1,2)
w

######## mostra o grafico da fun��o
 plot(x,w,xlab="x",ylab="f(x)")


########## ###### alinea b.  calculando valores da fun��o distribui��o da logistica

#####   usando a fun��o do R

   
  
    plogis(2,1,2)
#0.622459

    plogis(5,1,2)
#0.8807

    plogis(8,1,2)
#0.970

    plogis(10,1,2)
#0.989

    plogis(0,1,2)
#0.3775


x<-seq(-10,10,0.1)
#####dlogis(x,1,2)
w<-plogis(x,1,2)
w


leg<-"dist1.logist"

 plot(x,w,main=leg,xlab="x",ylab="f(x)")


###### fun��o do R, mas escrevendo a  express�o da f.d.log�stica




dist.logist<-function(u,alpha,beta){
+ (1+exp(-(u-alpha)/beta))**(-1)}

z<-seq(-10,10,0.5)
 
 w<- dist.logist(z,1,2)

#definir a legenda como titulo principal-main
 leg<-"dist2.logist"

# mostra o grafico da fun��o, com titulo e legendas
 plot(z,w,main=leg,xlab="z",ylab="f(z)")




########### alinea c. calcular quantis

   #####usar o comando do R

    qlogis(0.25,1,2)
#-1.1972

    qlogis(0.5,1,2)
#1

    qlogis(0.75,1,2)
#3.197


   ##### sem usar o comando do R

#1� quartil
quant.log<-1-2*log((1-0.25)/0.25)

quant.log
#-1.197225


#mediana
quant.log<-1-2*log((1-0.5)/0.5)

quant.log
#1


#3� quartil
quant.log<-1-2*log((1-0.75)/0.75)

quant.log
#3.197225




########## 4alinea d. gerar valores da log�stica

   ##### usando a fun��o do R

    rlogis(50,1,2) #gera 50 observ  de param (1,2)


	#representar graficamente todos os valores gerados

x.rlogist<-rlogis(5000,1,2)
hist(w.rlogist)

dens.logist<-function(u,alpha,beta){
+ beta**(-1)* (exp(-(u-alpha)/beta))/((1+exp(-(u-alpha)/beta))**2)}


w<-dens.logist(x.rlogist,1,2)

	# mostra o grafico da fun��o

 plot(x.rlogist,w,xlab="x",ylab="f(x)")




  ###### sem usar a fun��o do R, para gerar os numeros aleatorios


	 #1� gerar uniformes (0,1)

   runif (100)

   y<-runif(100)

   y

   	# 2� gerar valores da logistica
   
    val.logist<-function(y,alpha,beta){
    + alpha-beta*log((1-y)/y)}

   x.rlog<- val.logist(y,1,2)

   x.rlog


	#para ver que o grafico � semelhante ao anterior

x.rlog<-rlogis(100,1,2)
hist(x.rlog)

dens.logist<-function(u,alpha,beta){
+ beta**(-1)* (exp(-(u-alpha)/beta))/((1+exp(-(u-alpha)/beta))**2)}


w<-dens.logist(x.rlog,1,2)

	# mostra o grafico da fun��o, com titulo e legendas

 plot(x.rlog,w,xlab="x",ylab="f(x)")



	######### 2 graficos -histograma e gerados ##############

    val.logist<-function(y,alpha,beta){
    + alpha-beta*log((1-y)/y)}

   x.rlog<- val.logist(y,1,2)

   x


	#Histograma com freq relativas

x.rlog<-rlogis(100,1,2)
hist(x.rlog,prob=TRUE,main=NULL)
 t<-seq(-10,10,by=0.5)
lines(t,dlogis(t,1,2),col="red")





##########################################################################

############################# Exercicio 5  ###############################

##########################################################################


##########alinea a

X<-runif(1000)
plot(X[1:999],X[2:1000], xlab=expression(X[i]), ylab=expression(X[i+1]))



#mexendo no tamanho
X<-runif(1000)
plot(X[1:999],X[2:1000], cex=0.5,xlab=expression(X[i]), ylab=expression(X[i+1]))

acf(X)


######### alinea b  tirado

LCG <- function(n, m, a, c, X0) {
X <- c()
Xn <- X0
for (i in 1:n) {
Xn <- (a*Xn + c) %% m
X[i] <- Xn
}
return(X)
}
m<-8
a<-5
c<-1
seed<-0
X<-LCG(1000,m,a,c,seed)/m
plot(X[1:999],X[2:1000],  asp=1,cex=0.5,xlab=expression(X[i]), ylab=expression(X[i+1]))

acf(X) #carater periodico


########## alinea c  tirado 
####considerando outro conjunto de valores

LCG <- function(n, m, a, c, X0) {
X <- c()
Xn <- X0
for (i in 1:n) {
Xn <- (a*Xn + c) %% m
X[i] <- Xn
}
return(X)
}
m<-1024
a<-401
c<-101
seed<-0
X<-LCG(1000,m,a,c,seed)/m
#plot(X[1:999],X[2:1000],  asp=1,cex=0.5,xlab=expression(X[i]), ylab=expression(X[i+1]))

acf(X) #fun�o de autocorrela�ao: conclus�o, sem correla��o





####################################################################

############## Exercicio 6 ##########################################

####################################################################

### 1000 NPA's da beta(2,2) usando m�todo Rejei��o

 #par(mfrow=c(2,2))

######### alinea b

c<-3/2
n <- 1000
k <- 0 # contador para as observa��es aceites
j <- 0 # itera��es
x <- numeric(n)

while (k < n) {
 y<- runif(1) # distribui��o proponente fdp g
 u<- runif(1) #va uniforme
 j <- j + 1
 if (u<4*y*(1-y)  ) {
     # aceita y
     k <- k + 1
     x[k] <- y
     }
 }

x #observa��es com a distribui��o beta(2,2) obtidas pelo met. rejei��o

j # contador das itera��es

vesp<-1000*c
vesp



####### alinea c

qbeta(0.25,2,2)  #1� quartil da distribui��o
qbeta(0.5,2,2)
qbeta(0.75,2,2)

summary(x)


var(x)


###### alinea d

hist(x,prob=TRUE,main=NULL,,ylim=c(0,2))
text(.5,1.8,"1000 NPA�s Beta(2,2)")

hist(x,prob=TRUE,main="1000 NPA�s Beta(2,2)",,ylim=c(0,2))


t<-seq(0,1,by=0.01)
lines (t,dbeta(t,2,2),col="red")
text(0.8,1.5,"fdp Be(2,2)", col="red")



#######  alinea e 
###  10000 NPA's da beta(2,2) usando m�todo Rejei��o


c<-3/2
n <- 10000
k <- 0 # contador para as observa��es aceites
j <- 0 # itera��es
x <- numeric(n)

while (k < n) {
   y<- runif(1) # distribui��o proponente fdp g 
   u<- runif(1) #va uniforme
   j <- j + 1
   if (u<4*y*(1-y)  ) {
      # aceita y
      k <- k + 1
      x[k] <- y
      }
   }

j # contador das itera��es
vesp<-10000*c
vesp

hist(x,prob=TRUE,main=NULL,,ylim=c(0,2))
text(.5,1.8,"10000 NPA�s Beta(2,2)")

t<-seq(0,1,by=0.01)
lines (t,dbeta(t,2,2),col="red")
text(0.8,1.4,"fdp Be(2,2)", col="red")


ks.test(x,"pbeta",2,2,alternative="two.sided")  #testa, usando o tsete de Kolmogorov-Smirnov se aos dados prov�m de uma beta(2,2)


qbeta(0.25,2,2)
qbeta(0.5,2,2)
qbeta(0.75,2,2)
summary(x)
var(x)



#############################################################################

################  exercicio 7   ##############################################

############################################################################


# mistura de gamas


######## 1. caso mais simples:   gerar misturas de 2 gamas:

##### 0.3ga(1.5,3)+0.7ga(5,3)

n<-1000
p<-c(0.3,0.7)

lambda<-c(1.5,5)

k<-sample(1:2, size=n, replace=TRUE, prob=p)

k
shape<-lambda[k]  #vetor, com elementos de lambda indexados por k
shape
x<-rgamma(n,shape=shape,rate=3)
x


#plot da densidade de mistura
plot(density(x),xlim=c(-1,6),ylim=c(0,2),lwd=3,xlab="x", main="",col="grey20")
text(4,2,"1000 NPA�s mistura", col="grey20")


#cada gama
for(i in 1:2)
lines(density (rgamma(n,lambda[i],3)), lty=2)

# global mas com uma sequencia
t<-seq (0,7,by=0.1)
lines(t,0.3*dgamma(t,1.5,3)+0.7*dgamma(t,5,3),lwd=1,col="red")




##################2.  caso do enunciado

#1 gerar misturas de 5 gamas:
#enunciado  0.1ga(3,0.1)+0.2ga(3,0.2)+0.2ga(3,0.2)+0.3ga(3,0.3)+0.2ga(3,0.2)
 
n<-1000
p<-c(0.1,0.2,0.2,0.3,0.2)

lambda<-c(0.1,0.2,0.2,0.3,0.2)

k<-sample(1:5, size=n, replace=TRUE, prob=p)

rate<-lambda[k]  #vetor, com elementos de lambda indexados por k

x<-rgamma(n,shape=3,rate=rate)
x


#plot da densidade de mistura
plot(density(x),xlim=c(-2,80),ylim=c(0,0.15),lwd=3,xlab="x", main="",col="grey20")
text(10,0.08,"1000 NPA�s mistura", col="grey2")


#cada gama
for(i in 1:5)
lines(density (rgamma(n,3,lambda[i])), lty=2, col=i)

#usando uma sequencia
t<-seq (-2,80,by=0.1)
lines(t,0.1*dgamma(t,3,0.1)+0.2*dgamma(t,3,0.2)+0.2*dgamma(t,3,0.2)+0.3*dgamma(t,3,0.3)+0.2*dgamma(t,3,0.2),lwd=1,col="red")



###############################  sugerir como trabalho para casa 
#gerar misturas de 5 gamas:
#outro caso  0.1gama(3,1)+0.2gama(3,1.5)+0.2gama(3,2)+ 0.3gama(3,2.5)+0.2gama(3,3))


# 1� gerar misturas de 5 gamas: 0.1gama(3,1)+0.2gama(3,1.5)+0.2gama(3,2)+ 0.3gama(3,2.5)+0.2gama(3,3))

  n<-500
  p<-c(0.1,0.2,0.2,0.3,0.2)
  p
  lambda<-c(1,1.5,2,2.5,3)
  k<-sample(1:5, size=n,replace=TRUE, prob=p)  #gerar valores de 1 a 5
  k

  rate<-lambda[k]   # vector, contem elementos de lambda indexados por k 
  x<- rgamma(n,shape=3,rate=rate)
  x

  #representa��o gr�fica da densidade da mistura: f=soma(pi*fi)
  
   # calculo da fdp da mistura para um unico valor da amostra x
 
	f<-function(x,lambda,theta){
    	sum(dgamma(x,3,lambda)*theta)
    	}    

    x<- seq(0,8,length=200)
    dim(x)<-length(x)  #necessario colocar para usar a fun��o apply
    dim(x)
    y<-apply(x,1,f,lambda=lambda,theta=p)       # calcula a densidade da mistura ao longo de x                
    y
    length(y)
 

    #Grafico da fdp da mistura

    plot(x,y,type="l",ylim=c(0,0.85), lwd=3, ylab="Density")

    for (j in 1:5) {
         y<-apply(x,1,dgamma, shape=3,rate=lambda[j])
         lines(x,y)
     }

    
 
#############################################################################

################  exercicio 8   ##############################################

############################################################################

 ####exercicio  Beta-binomial


#########alinea b: gerar valores da beta.binomial


####### 1. Usando o facto de ser uma mistura e analisando o integral, gerar valores 
da beta-binomial

   #1� gerar 100 valores da beta(22,4)

#alpha=22, beta=4, nrep=100



xbeta<-rbeta(100,22,4)
xbeta

   #2� gerar valores de binomial (10, xbeta)

bebi<-rbinom(100,10,xbeta)  #valores pretendidos
bebi

summary(bebi)
mean(bebi)
hist(bebi) # apropriado?
barplot(table(bebi))

barplot(table(bebi)/length(bebi))


####### 2. calcular valores da fun��o de probabilidade num ponto
 (u=5)

#factorial(3): calcula factorial de 3
#choose(4,2): combina��es de 4, 2 a 2
#gamma(3): fun��o gama calculada em
#beta(2,3): fun��o beta calculada em (2,3)
 

massa<-function(u,alpha,betta,n){
1/(beta(alpha,betta)*gamma(alpha+betta+n))*choose(n,u)*gamma(u+alpha)*gamma(betta+n-u)}

m<-massa(5,22,4,10)
m  # valor da fdp da mistura no ponto u=5


######################################################################
#### alternativamente podia-se ter definido aparte alguns valores parciais a introduzir nas gamas 
ga1<-function(alpha,betta,n){alpha+betta+n}
g1<-ga1(22,4,10)
g1

ga2<-function(u,alpha){u+alpha}
g2<-ga2(5,22)
g2

ga3<-function(u,betta,n){betta+n-u}
g3<-ga3(5,4,10)
g3

massa<-function(u,alpha,betta,n){
1/(beta(alpha,betta)*gamma(g1))*choose(n,u)*gamma(g2)*gamma(g3)}

m<-massa(5,22,4,10)
m  # valor da fdp da mistura no ponto u=5

######################################################################
#######  Esbo�ar a fun��o de probabilidade da bB(22,4,10)
    
###sugest�o: considerar uma sequencia de valores no suporte


ga1<-function(alpha,betta,n){alpha+betta+n}
g1<-ga1(22,4,10)
g1

ga2<-function(t,alpha){t+alpha}
g2<-ga2(t,22)
g2

ga3<-function(t,betta,n){betta+n-t}
g3<-ga3(t,4,10)
g3

massa<-function(u,alpha,betta,n){
1/(beta(alpha,betta)*gamma(g1))*choose(n,t)*gamma(g2)*gamma(g3)}

t<-seq(1,10, by=1)
t

fmp<-massa(t,22,4,10)
fmp

plot(fmp)
lines(t,fmp,type="h",lwd=3,col="red")



##################################################################################
##################################################################################
########  gerar valores da beta binomial usando fun��es do R

## NOTA
## as distribui��es podem estar reparametrizadas; existe mais do que uma forma de escrever a betabinomial;  
## no R est� definida � custa da m�dia da distribui��o e da correla��o rho entre as vari�veis beta e binomial

#Require(VGAM)

install.packages("VGAM")
library("VGAM")

N<-10
alpha<-22
beta<-4
mu<-alpha/(alpha+beta)  #m�dia da batabinomial
rho<-1/(1+alpha+beta) 


betabinom2<-rbetabinom(100,N,prob=mu, rho=rho)
betabinom2

barplot(table(betabinom2))#grafico do diagrama de barras da beta binomial gerada, com frequencia absoluta
barplot(table(betabinom2)/length(betabinom2))#usando frequencias relativas

### tamb�m se pode calcular fp da batabinomial:
dbetabinom(5,N,mu,rho)# apresenta valor da fmp da betabinomial(22,4,10) calculada no ponto 5

