#### Alunos: Rita Ferrolho (88822) e Diogo Pedrosa (94358) ####

### ex 1a ###

c = 2*exp(0.5)/sqrt(2*pi)
n = 1000 # numero de geracoes
n_counter = 0 # contador do numero de observacoes aceites (numero de NPA's gerados)
x = numeric()


for(j in 1:n){
  y = rexp(1) #geracao de valores da funcao proponente
  u = runif(1) #geracao de valores da distribuicao uniforme
  if (u<exp(-0.5*y^2+y-0.5)){
    #aceitar y
    n_counter = n_counter+1
    x[n_counter] = y
  }
}

hist(x,prob=TRUE,main="1000 NPA's de X, com método rejeicao",ylim=c(0,0.8))

fdp = function(x){
  (2/sqrt(2*pi))*exp(-0.5*x^2)
}

t = seq(0,5,by=0.01)
lines(t,fdp(t),col="red")
text(2, .3, "f.d.p. f(x)", col="red")


# Analise a eficiencia do metodo, analisando o numero
# esperado de observacoes para as n=1000 geracoes e o que efetivamente foi necessario

# numero esperado de observacoes para as n=1000 geracoes

observacoesEsperadas = n*c
n*c

#numero de observacoes que efetivamente foram verificadas

n_counter = numeric() # counter for the accepted observations (numero de NPA's gerados)

for(tentativa in 1:500){
  
  n_counter[tentativa] = 0
  
  for(j in 1:n){
    y = rexp(1) # geracao de valores da funcao proponente
    u = runif(1) # geracao de valores da distribuicao uniforme
    if (u<exp(-0.5*y^2+y-0.5)){
      # aceitar y
      n_counter[tentativa] = n_counter[tentativa]+1
    }
  }
}

n_mean = mean(n_counter)
n_mean # media

n_sd = sd(n_counter)
n_sd # desvio padrao

n_var = var(n_counter)
n_var # variancia

n_erro = sqrt(n_var/length(n_counter))
n_erro # erro

## ex 1b ###

# gerar n valores da distribuição normal N(0,1), para x > 0

n = 1000
k = 0 # contabiliza o numero de observacoes aceites
j = 0 # contabiliza o numero de iteracoes
y = numeric(n) # array com n posicoes

while(k<n){
  x = rnorm(1) # v.a da distribuição N(0,1)
  j = j+1
  if(x>=0){
    # aceitar x como valor do modelo truncado
    k = k+1
    y[k] = x
  }
}

y # 1000 observacoes do modelo truncado

j # numero de iterações efetuadas

valor_esperado = 1/(1-pnorm(0))
valor_esperado

# histograma com a f.d.p de X sobreposta
hist(y,prob=TRUE,main="1000 NPAs de X, com modelo truncado",ylim=c(0,0.8))

f = function(x){
  (2/sqrt(2*pi))*exp(-(x^2)/2)
}

t = seq(0,5,by=0.01)
lines(t, f(t), col="red")
text(2, .3, "f.d.p. f(x)", col="red")

n_mean = mean(y)
n_mean # media

n_sd = sd(y)
n_sd # desvio padrao

n_var = var(y)
n_var # variancia

n_erro = sqrt(n_var/length(y))
n_erro # erro


### ex 2a - nao envolve codigo ###

### ex 2bi ###

rm(list = ls()) # remover lista de variaveis

# dimensao 100
n = 100
normal_gama100 = numeric(100)
for (i in 0:n) {
  taugamma = rgamma(n,shape = 2,rate = 2)
  xnorm = rnorm(n,0,1/taugamma)
  normal_gama100[i] = mean(xnorm)
}
mean(normal_gama100)

# dimensao 1000
n = 1000
normal_gama1000 = numeric(1000)
for (i in 0:n) {
  taugamma = rgamma(n,shape = 2,rate = 2)
  xnorm = rnorm(n,0,1/taugamma)
  normal_gama1000[i] = mean(xnorm)
}
mean(normal_gama1000)

# dimensao 10000
n = 10000
normal_gama10000 = numeric(10000)
for (i in 0:n) {
  taugamma = rgamma(n,shape = 2,rate = 2)
  xnorm = rnorm(n,0,1/taugamma)
  normal_gama10000[i] = mean(xnorm)
}
mean(normal_gama10000)

## ex 2bii ##

tstudent100 = rt(100, 4)
mean(tstudent100)

tstudent1000 = rt(1000, 4)
mean(tstudent1000)

tstudent10000 = rt(10000, 4)
mean(tstudent10000)

## ex 2biii ##

summary(normal_gama100)
summary(tstudent100)

summary(normal_gama1000)
summary(tstudent1000)

summary(normal_gama10000)
summary(tstudent10000)

## ex 2biv ##

# para fazer plot da distribuicao T-Student de n = 100
hist(tstudent100, prob=TRUE, ylim=c(0,0.4))
t = seq(-4,4,by=0.01)
lines(t,dt(t,4),col="red")
text(2, 0.5, "f.d.p. f(x)", col="red")

# para fazer plot da distribuicao T-Student de n = 1000
hist(tstudent1000, prob=TRUE, ylim=c(0,0.4))
t = seq(-4,4,by=0.01)
lines(t,dt(t,4),col="red")
text(2, 0.5, "f.d.p. f(x)", col="red")

# para fazer plot da distribuicao T-Student de n = 10000
hist(tstudent10000, prob=TRUE, xlim=c(-5,5), ylim=c(0,0.4))
t = seq(-4,4,by=0.01)
lines(t,dt(t,4),col="red")
text(2, 0.5, "f.d.p. f(x)", col="red")


# mistura da normal gama de n = 100
mean(normal_gama100)
hist(normal_gama100) # histograma
plot(normal_gama100)

# para fazer plot de mistura da normal gama de n = 100
n = 100
p = c(0.3,0.7)
lambda = c(1.5,5)
k = sample(1:2,size=n, replace=TRUE, prob=p)
k
shape = lambda[k]
shape # em vez de imprimir valores gerados de p, imprime respetivos valores de lambda

plot(density(normal_gama100),xlim=c(-1,2),ylim=c(0,1.5),lwd=3,xlab="x",main="Mistura da distribuição normal gama normal_gama100")


# mistura da normal gama de n = 1000
mean(normal_gama1000) 
hist(normal_gama1000) # histograma
plot(normal_gama1000)

# para fazer plot de mistura da normal gama de n = 1000
n = 1000
p = c(0.3,0.7)
lambda = c(1.5,5)
k = sample(1:2,size=n, replace=TRUE, prob=p)
k
shape = lambda[k]
shape # em vez de imprimir valores gerados de p, imprime respetivos valores de lambda

plot(density(normal_gama1000),xlim=c(-1,2),ylim=c(0,3.5),lwd=3,xlab="x",main="Mistura da distribuição normal gama normal_gama1000")


# mistura da normal gama de n = 10000
mean(normal_gama10000) 
hist(normal_gama10000) # histograma
plot(normal_gama10000)

# para fazer plot de mistura da normal gama de n = 10000
n = 10000
p = c(0.3,0.7)
lambda = c(1.5,5)
k = sample(1:2,size=n, replace=TRUE, prob=p)
k
shape = lambda[k]
shape # em vez de imprimir valores gerados de p, imprime respetivos valores de lambda

plot(density(normal_gama10000),xlim=c(-1,2),ylim=c(0,9),lwd=3,xlab="x",main="Mistura da distribuição normal gama normal_gama10000")



# mean(tstudent100)
# hist(tstudent100) # histograma
# plot(tstudent100)
# 
# # para fazer plot T student para n = 100
# n = 100
# p = c(0.3,0.7)
# lambda = c(1.5,5)
# k = sample(1:2,size=n, replace=TRUE, prob=p)
# k
# shape = lambda[k]
# shape # em vez de imprimir valores gerados de p, imprime respetivos valores de lambda
# 
# plot(density(tstudent100),xlim=c(-1,2),ylim=c(0,0.4),lwd=3,xlab="x",main="10000 NPA's T-Student")
# 
# 
# 
# mean(tstudent1000)
# hist(tstudent1000) # histograma
# plot(tstudent1000)
# 
# # para fazer plot de T-Student para n = 1000
# n = 1000
# p = c(0.3,0.7)
# lambda = c(1.5,5)
# k = sample(1:2,size=n, replace=TRUE, prob=p)
# k
# shape = lambda[k]
# shape # em vez de imprimir valores gerados de p, imprime respetivos valores de lambda
# 
# plot(density(tstudent1000),xlim=c(-1,2),ylim=c(0,0.4),lwd=3,xlab="x",main="10000 NPA's T-Student")
# 
# 
# 
# mean(tstudent1000)
# hist(tstudent1000) # histograma
# plot(tstudent1000)
# 
# # para fazer plot de T-Student para n = 10000
# n = 10000
# p = c(0.3,0.7)
# lambda = c(1.5,5)
# k = sample(1:2,size=n, replace=TRUE, prob=p)
# k
# shape = lambda[k]
# shape # em vez de imprimir valores gerados de p, imprime respetivos valores de lambda
# 
# plot(density(tstudent10000),xlim=c(-1,2),ylim=c(0,0.4),lwd=3,xlab="x",main="10000 NPA's T-Student")



### ex 3a ###

rm(list = ls()) # remover lista de variaveis

n=5000

#beta(3,3)
xbeta33 = rbeta(n,3,3)
mu33 = mean(xbeta33)
car33 = var(xbeta33)
sd33 = sd(xbeta33)
hist(xbeta33, prob = TRUE, main = "B(3,3)")
t = seq(0.0,1,0.01)
lines(t,dbeta(t,3,3), col="red")

# beta(3,2)
xbeta32 = rbeta(n,3,2)
mu32 = mean(xbeta32)
var32 = var(xbeta32)
sd32 = sd(xbeta32)
hist(xbeta32, prob = TRUE,main = "B(3,2)")
t = seq(0.0,1,0.01)
lines(t,dbeta(t,3,2), col="red")

# beta(2,3)
xbeta23 = rbeta(n,2,3)
mu23 = mean(xbeta23)
var23 = var(xbeta23)
sd23 = sd(xbeta23)
hist(xbeta23, prob = TRUE,main = "B(2,3)")
t = seq(0.0,1,0.01)
lines(t,dbeta(t,2,3), col="red")

### ex 3b ##

n = 50

# beta(3,3)
coefs = numeric(n)
media = numeric(n)
mediana = numeric(n)

for (i in 1:n) {
  xbeta1 = rbeta(5000,3,3)
  media[i] = mean(xbeta1)
  mediana[i] = median(xbeta1)
  
  soma = 0
  for (j in 1:5000) {
    soma = soma + (xbeta1[j]-media[i])**3
  }
  
  coefs[i] = (1/5000)*soma/sd(xbeta1)**3 # coeficiente de assimetria
}
coefs

summary(coefs) # média corresponde ao coeficiente de assimetria experimental
summary(media)
summary(mediana)

# beta(3,2)
coefs = numeric(n)
media = numeric(n)
mediana = numeric(n)

for (i in 1:n) {
  xbeta1 = rbeta(5000,3,2)
  media[i] = mean(xbeta1)
  mediana[i] = median(xbeta1)
  
  soma = 0
  for (j in 1:5000) {
    soma = soma + (xbeta1[j]-media[i])**3
  }
  
  coefs[i] = (1/5000)*soma/sd(xbeta1)**3 # coeficiente de assimetria
}
coefs

summary(coefs) # média corresponde ao coeficiente de assimetria experimental
summary(media)
summary(mediana)

# beta(2,3)
coefs = numeric(n)
media = numeric(n)
mediana = numeric(n)

for (i in 1:n) {
  xbeta1 = rbeta(5000,2,3)
  media[i] = mean(xbeta1)
  mediana[i] = median(xbeta1)
  
  soma = 0
  for (j in 1:5000) {
    soma = soma + (xbeta1[j]-media[i])**3
  }
  
  coefs[i] = (1/5000)*soma/sd(xbeta1)**3 # coeficiente de assimetria
}
coefs

summary(coefs) # média corresponde ao coeficiente de assimetria experimental
summary(media)
summary(mediana)
