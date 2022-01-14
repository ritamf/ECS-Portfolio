### ficha3

##ex1

# 1) definir densidade de distribuição Rayleigh
f = function(x,sigma) {
  x/sigma^2*exp(-x^2/(2*sigma^2)) # densidade de dist Rayleigh
}

# 2) definir algoritmo M-H
m=20000 # comprimento da cadeia
sigma = 4
x = numeric(m)

x[1] = rchisq(1,df=1) # valor inicial para a 1ª observação de x

# OBS: em cada transição o ponto candidato y é gerado de qui-quadrado em g1 = x(i-1)

k = 0
u = runif(m)

for(i in 2:m) {
  xt = x[i-1]
  y = rchisq(1,df=xt) # distribuicao proponente
  num = f(y, sigma) * dchisq(xt,df=y)
  den = f(xt, sigma) * dchisq(y,df=xt)
  if (u[i]<=num/den) x[i] = y else {
    x[i] = xt # y é rejeitado
    k = k+1
  }
}

print(k)

index = 9500:10000
y1 = x[index]
y1

par(mfrow=c(1,1))
hist(y1)
lines(density(y1))

hist(y1,freq=FALSE)
lines(density(y1))

##ex2a
m = 10000 # comprimento da cadeia
sigma = 0.05
x = numeric(m)

n = 4 # grau de liberdade de tstudent

x[1] = rnorm(1,0,sigma)
k=0
u=runif(m)

for(i in 2:m) {
  xt = x[i-1]
  y = rnorm(1,xt,sigma) # porque y|x_t ~N(x_t,sigma)
  num = dt(y,n)
  den = dt(xt,n)
  if (u[i]<=num/den) x[i]=y else {
    x[i]=xt
    k=k+1
  }
}
k

k/10000*100

# o sucesso desta taxa ta baseado na semelhanca entre distribuicao normal e t student

##ex2b
# repeticao ex2a para σ^2 = 0.05; 0.5; 2; 16.

sigmas = c(0.05,0.5,2,16)

for (si in sigmas) {
  m = 10000 # comprimento da cadeia
  x = numeric(m)
  
  n = 4
  
  x[1] = rnorm(1,0,si)
  k=0
  u=runif(m)
  
  for(i in 2:m) {
    xt = x[i-1]
    y = rnorm(1,xt,si) # porque y|x_t ~N(x_t,sigma)
    num = dt(y,n)
    den = dt(xt,n)
    if (u[i]<=num/den) x[i]=y else {
      x[i]=xt
      k=k+1
    }
  }
  print(k)
  print(k/10000*100)
}


##ex3

# amostrador independente

# misturador de normais e quer-se estimar o parametro do peso da mistura

m = 10000 # comprimento da cadeia
xt= numeric(m)
a = 1 # parametro da distribuicao proponente beta(a,b)
b = 1 # parametro da distribuicao proponente beta(a,b)

p = 0.2 # parametro da mistura
n = 30 # tamanho da amostra
mu = c(0,5) # parametro de densidades normais
sigma = c(1,1)

# gerar amostra observada da mistura
i = sample(1:2, size=n, replace=TRUE, prob=c(p,1-p))
i
x = norm(n,mu[i],sigma[i])
x

# implementar M-H usando amostrador da cadeia independente

u = runif(m)
y = rbeta(m,a,b) # distribuicao proponente
xt[1] = 0.5 # valor inicial

for (i in 2:m) {
  fy = y[i] * dnorm(x,mu[1], sigma[ 1 + (1-y[i])*dnorm(x,mu[2],sigma[2]) ])
  
  fx = xt[i-1]*dnorm(x,mu[2],sigma[2])
  
  r = prod(fy/fx) * dbeta(xt[i-1],a,b)/(dbeta(y[i],a,b))

  if (u[i]<=r) 
    xt[i]=y[1]
  else
    xt[i]=xt[i-1]
}
xt
mean(xt) # estimativa do peso da mistura

plot(xt, type="1", ylab="p")
hist(xt[1000:m],main="",xlab="p",prob=TRUE) # hist baseado freq absolutas
print(mean(xt[1001:m]))


##ex5

# inicializar constantes e parametros

N = 10000 # comprimento da cadeia
burn = 2000 # aquecimento

X = matrix(0,N,2) # cadeia, amostra bivariada
X

rho = 0.75
mu1 = 0
mu2 = 2
sigma1 = 1
sigma2 = 0.5

s1 = sqrt(1-rho^2)*sigma1 # desvio padrao da condicional completa de X1
s2 = sqrt(1-rho^2)*sigma2 # desvio padrao da condicional completa de X2

# gerar a cadeia

X[1,] = c(mu1,mu2) # valor inicial
X[1,]

for (i in 2:N) {
  x2 = X[i-1,2]
  m1 = mu1 + rho * (x2-mu2) * sigma1 / sigma2 # media da cond. comp1 de X1|x2
  X[i,1] = rnorm(1,m1,s1) # gerar valor de x1, usando a correspondente cond
  x1 = X[i,1] # valor gerado anteriormente para x1
  m2 = mu2 + rho * (x1-mu1) * sigma2/sigma1 # media da cond.comp1 de X2|x1
  X[i,2] = rnorm(1,m2,s2) # gerar valor de x2, usando a correspondente cond 
}
X

# a geracao de valores pares da normal multivariada esta concluida

# usar library (coda) para ver trajetoria, estudar convergencia da CM

install.packages("coda")
library(coda)

mcmcX = mcmc(X)
plot(mcmcX) # trace - valor de x, tem aleatoriedade, media ≃ 0

# plot de percentis (evolução)
cumuplot(mcmc(X))

geweke.diag(mcmc(X)) # valor do teste (compara medias)

# este grafico mostra 2 bandas (≃96%)
# quase todos os pontos estao dentro da banda
geweke.plot(mcmc(X),frac1=0.1,frac2=0.5)

b = 2000

X = X[b:N,]

colMeans(X)
summary(X)
cov(X)
cor(X)

plot(X)
boxplot(X)

