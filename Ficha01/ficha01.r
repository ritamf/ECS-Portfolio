### ex1 ####

# gerar valores de dist N(0,1)
rnorm(100) # rnorm(100,0,1)

# gerar valores de dist N(2,4)
gerados_cem = rnorm(100,2,2) # mostrar dados

# alinea a)
summary(gerados_cem)
sd(gerados_cem) # desvio padrao
var(gerados_cem)

# alinea b)
fdp = dnorm(gerados_cem,2,2) # valores da fdp dos valores gerados
fdp
plot(gerados_cem,fdp,main="valores da fdp dos valores gerados de N(2,4)")

fd = pnorm(gerados_cem,2,2) # valores da fd dos valores gerados
fd
plot(gerados_cem, fd, main="valores da fd de valores gerados de N(2,4)")

# alinea c)
hist(gerados_cem) # frequencias absolutas
hist(gerados_cem, prob=TRUE, main=NULL, ylim=c(0,0.25)) # frequencias relativas
text(1.5, 0.25, "100 NPA's NORMAL (2,2)")

#alternativa
hist(gerados_cem, prob=TRUE, main="100 NPA's NORMAL (2,2)", ylim=c(0,0.25))

# alinea d) i)
seq_valores = seq(-2.5,8,.5)
seq_valores

densidade_seq = dnorm(seq_valores,2,2)
densidade_seq
plot(densidade_seq) # nota: index != seq_valores !!

# alinea d) ii

# amostra de dimensao 100, designada gerados_cem
hist(gerados_cem) # frequencias absolutas
hist(gerados_cem, prob=TRUE, main="100 NPA's NORMAL(2,2)",ylim=c(0,.25)) # frequencias relativas

# tpc -> repetir alinea d)ii para N = 1000
gerados_mil = rnorm(1000,2,2)
hist(gerados_mil) # frequencias absolutas
hist(gerados_mil, prob=TRUE, main="1000 NPA's NORMAL(2,2)",ylim=c(0,.25)) # frequencias relativas

### ex2 ####

#Limpar variaveis
rm(list=ls())

generated_binomial=binom(200, 200, 0.75)
plot(generated_binomial)
table(generated_binomial) # opcional

## a)
summary(generated_binomial)

# Calculate the df
generated_binomial_df<-pbinom(generated_binomial, 100, 0.25)
generated_binomial_df
generated_binomial_df_max<-pbinom(200, 100, 0.25)
generated_binomial_df_max
generated_binomial_df_min<-pbinom(0, 100, 0.25)
generated_binomial_df_min

# Calculate the PDF
generated_binomial_pdf<-dbinom(generated_binomial, 100, 0.25)
generated_binomial_pdf
generated_binomial_pdf_max<-dbinom(200, 100, 0.25)
generated_binomial_df_max
generated_binomial_pdf_min<-dbinom(0, 100, 0.25)
generated_binomial_pdf_min

## b)
# Create Barplot
barplot(generated_binomial,ylim = c(0,45)) # Hist only used for very large samples

# Subplot
par(mfrow=c(1,2))

#Absolute frequencies
barplot(table(generated_binomial))

#Relative frequencies
barplot(table(generated_binomial)/length(generated_binomial))

## b) HomeWork
n=5000

generated_binomial_10=rbinom(n, 10, 0.25)
generated_binomial_50=rbinom(n, 50, 0.25)
generated_binomial_100=rbinom(n, 100, 0.25)
par(mfrow=c(1,3)) # allows 3 subplots on same window
barplot(table(generated_binomial_10))
barplot(table(generated_binomial_50))
barplot(table(generated_binomial_100))

## c)
par(mfrow=c(1,2))
hist(generated_binomial_100)
generated_normal_25=rnorm(n, 25, sqrt(18.75))
hist(generated_normal_25)

### ex3 ###

install.packages("extremefit")
library(extremefit)

dens.paret = function(u, alpha, beta) {
  alpha*(beta**alpha)*u**(-(alpha+1))
}

dens.paret(5,3,1.5)
dpareto(5,3,0,1.5)
z = seq(1.5,10,.01)
z

w = dens.paret(z,3,1.5)
w

plot(z,w,main="dens.pareto",xlab="z",ylab="f(z)")


dens.paret = function(u, alpha, beta) {
  1-beta**alpha*x**(-alpha)
}

dens.paret(5,3,1.5)
dpareto(5,3,0,1.5)
z = seq(1.5,10,.01)
z

w = dist.paret(z,3,1.5)
w

plot(z,w,main="dist.pareto",xlab="z",ylab="F(z)")

#a3 - quantis
### escrevendo expressao que permite achar quantis

quant.paret = function(p,alpha,beta) {
  beta * (1-p) ** (-1/alpha)
}

q1 = quant.paret(1/4,3,1.5)
q1

mediana = quant.paret(1/2,3,1.5)
mediana

q3 = quant.paret(3/4,3,1.5)

d3 = quant.paret(0.3,3,1.5) # 3º decil
d3

qpareto(0.3,3,0,1.5) # 3º decil

d3 = quant.paret(0.3,3,1.5)
qpareto(0.3,3,0,1.5)

# a4 - geração de números aleatórios - método da transformação inversa

uniforme = runif(100)
uniforme

val.paret = function(u,alpha,beta) {
  beta*(1-u)**(-1/alpha)
}

valor_pareto = val.paret(u,3,1.5)
valor_pareto

# alinea b

# tpc
# gerar sequencia e obter esboco do grafico
# fazer o grafico tomando os valores gerados e calculando os respetivos valores da densidade de probabilidade

uniforme = runif(1000)
uniforme

alpha = 3
beta = 1.5

val.paret = function(y,alpha,beta) {
  beta * (1-y)**(-1/alpha) 
}

x = val.paret(y,alpha,beta)

mean(x)
var(x)

valormed = (alpha*beta)/(alpha-1)
valormed

variancia = alpha*beta**2 / ((alpha-2)*(alpha-1)**2) # exp de V(X)
variancia

###3c)

y<-runif(1000)
alpha<-3
beta<-1.5

val_paret<-function(y,alpha,beta)
{+ beta*(1-y)**(-1/alpha)}

x<-val_paret(y,alpha,beta)

valormed<-(alpha*beta)/(alpha-1) #expression of E(x)
#variancia<- ver no enunciado var(x)!!!

#alternativamente: instalei o 'extremefit'
library(extremefit)
#shape,location,scale
#ppareto(q,a=1,loc=0,scale=1)
#dpareto(x,a=1,loc=0,scale=1)
#qpareto(p,a=1,loc=0,scale=1)
#rpareto(n,a=1,loc=0,scale=1)

#existence of mean value of pareto dist?
paretodata<-rpareto(1000,0.5,0,1.5) #alpha <1
mean(paretodata); var(paretodata)
#instability of sample mean and variance

paretodata<-rpareto(1000,1.5,0,1.5)
mean(paretodata); var(paretodata)

paretodata<-rpareto(1000,2.5,0,1.5)
mean(paretodata); var(paretodata)
#alpha >2 much more stability

#histogram
x_rpareto<-rpareto(1000,3,0,1.5)
hist(x_rpareto,prob=TRUE,main=NULL,ylim=c(0,2))
summay(x_rpareto)
sequence<-seq(0.5,40,by=0.5)
lines(sequence,dpareto(x_rpareto,3,0,1.5),main=NULL,ylim=c(0,2)) #makes sense?? no
plot(x_rpareto,dpareto(x_rpareto,3,0,1.5),main=NULL,ylim=c(0,2))
lines(sequence,dpareto(sequence,3,0,1.5),col="red")


### ex4 ####

#Limpar variaveis
rm(list=ls())

###a
rm(list=ls())

f_logis <- function(x,a,b)
{+ b^(-1)*(exp(-(x-a)/b))/((1+exp((-x+a)/b))^2)}

x <- seq(1.5,10,0.01)
f <- f_logis(x,1,2) # alph=1, beta=2

plot(x,f,main="PDF logística",xlab="z",ylab="f(z)") #pdf graph

###b

dist.logis <- function(x, alpha, beta){1/(1+exp(-(x-alpha)/(beta)))}
y <- dist.logis(x, 3, 1.5)

leg="distribuição logistica"
plot(x,y,main=leg, xlab = "z", ylab="F(z)")

###c

quant.logis <- function(p,a,b){a+(b*log(p))/(1-p)}
q1=quant.logis(1/4,3,1.5)
med=quant.logis(1/2,3,1.5)
q3=quant.logis(3/4,3,1.5)
#quarto decil
d4=quant.logistic(4/10,3,1.5)

#Gráfico da função quantil
p=seq(0,1,length=100)
plot(p,quant.logis(p,3,1.5), xlab="p", ylab="Quantil(p)",main="Quantil Function")

###d

val.logistic=function(u, alpha, beta){alpha+(beta*log(u))/(1-u)}
u=runif(100)
x=val.logistic(u,3,1.5)

### ex 5 ###

# alínea a
X = runif(1000)
plot(X[1:999],X[2:1000],xlab=expression(X[i]),ylab=expression(X[i+1])) # não há padrão

acf(X) # função importante para mostrar o comportamento periódico, padrão é menos evidente

# alínea b
LCG = function(n,m,a,c,XO) {
X = c()
Xn = XO
for (i in 1:n){
  Xn = (a*Xn+c) %% m
  X[i] = Xn
}
return(X)
}

m=8
a=5
c=1
seed=0
X=LCG(1000,m,a,c,seed)/m

# há padrão
plot(X[1:999],X[2:1000],asp=1,cex=0.5,xlab=expression(X[i]),ylab=expression(X[i+10]))

acf(X) # padrão nítido, e valores de acf já não são próximos de zero
    
### ex 6 ###

rm(list=ls())

## alinea a - resolvida em papel ##

# alinea b
c = 3/2
n = 10000
k = 0
j = 0
x = numeric(n)

while (k < n) {
  y = runif(1)
  u = runif(1) # va uniforme
  j = j+1
  if (u<4*y*(1-y)) {
    # aceita y
    k = k+1
    x[k] = y
  }
}

x
j # contador de iteracoes

# alinea c
qbeta(0.25,2,2) # primeiro quartil
qbeta(0.5,2,2) # segundo quartil = mediana
qbeta(0.75,2,2) # terceiro quartil
summary(x) # valores dos quartis são identicos

var(x) # variancia

## alinea d ##

# prob=TRUE: valores relativos (de 0 ao desvio padrao)
# prob=FALSE: valores absolutos
hist(x,prob=TRUE,main="1000 NPA's Beta(2,2)",,ylim=c(0,2))

t=seq(0,1,by=0.01)
lines(t,dbeta(t,2,2),col="red")
text(0.8,1.5,"fdp Beta(2,2)", col="red")

## alinea e - feita em papel ##

### ex 7 ###

## alinea a ##

n = 1000
p = c(0.3,0.7)

lambda = c(1.5,5)

k = sample(1:2,size=n, replace=TRUE, prob=p)

k
shape = lambda[k]
shape # em vez de imprimir valores gerados de p, imprime respetivos valores de lambda
x=rgamma(n,shape=shape,rate=3)
x

# plot da densidade de mistura
plot(density(x),xlim=c(-1,6),ylim=c(0,1.5),lwd=3,xlab="x",main="1000 NPA's mistura")

# cada gamma
for(i in 1:3)
  # plots não deviam ser mt parecidos..nem devia haver valores positivos de densidade 
  # para x<0
  lines(density(rgamma(n,lambda[i],3)),lty=2)

# global mas com uma sequencia
t=seq(0,5,by=0.1)
lines(t,0.3*dgamma(t,1.5,3)+0.7*dgamma(t,5,3),lwd=1,col="red")

## alinea b ##
# semelhante a alinea a

### ex 8 ###

## alinea a - feita em papel ##

## alinea b ##
# script disponivel no moodle


