### Ficha 2 ###

## ex 1a ##

rm(list=ls())

N=10**3

z=rcauchy(N)
p.hat = sum(z>2)/N
p.hat

p = pcauchy(-2) # fd de cauchy calculada em -2
p

dif = abs(p.hat - pcauchy(-2))
dif

## ex 1b ##

x = runif(1000,0,2) # gerar valores de U(0,2)
integl = function(x) {1/(pi*(1+x**2))}
z = integl(x)
mean(z)
est.int=1/2-2*mean(z)
est.int
dif=abs(est.int - pcauchy(-2))
dif

## ex 1c ##

N = 10**3

p.hat

p = pcauchy(-2)
p

variancia = p*(1-p)/N
variancia
est.variancia = p.hat*(1-p.hat)/N
est.variancia

est.variancia = est.int*(1-p.hat)/N
est.variancia

## ex 2a ##

x = rexp(1000,3)
estimativa = 1/3 * mean(x**5*log(x))

v = var(1/3 * x**5*log(x))
erro = sqrt(v/1000)
erro

## ex 2b ##
c = 3**6/gamma(6)
estimativa.gamma = 1/c * mean(log(x))
v.gamma = var(1/c * log(x))
erro.gamma = sqrt(v.gamma/1000)
erro.gamma

## ex3 ##


# tpc: comparar erros das funcoes

## ex 4 ##

# SEM metodo das replicas
n = 1000

x = rnorm(n,1,2)
varx = var(x)
varx

qchisq(0.95,n-1) # 90%
qchisq(0.05,n-1)

qchisq(0.975,n-1)
qchisq(0.025,n-1)

LI = (n-1) * varx/qchisq(0.975,n-1)
LS = (n-1) * varx/qchisq(0.025,n-1)

LI
LS

# COM método das réplicas (10000) (para ficar menos dependente)

varx = numeric(10000)

for (i in 1:10000) {
  x = rnorm(n,1,2)
  varx[i] = var(x)
}

mean(varx)

LI = (n-1) * mean(varx)/qchisq(0.975,n-1)
LS = (n-1) * mean(varx)/qchisq(0.025,n-1)

LI
LS

## ex5a ## 
N = 100

x = numeric(N)
y = numeric(N)

y = rnorm(N)
y

x[1] = y[1]
x[1]
y[1]

for (n in 2:N) {
  x[n] = 0.3 * x[n-1] + y[n]
}
x

# calcular estimativa particular para esta geracao
num = numeric(N)
den = numeric(N)
t = numeric(N)

num[1] = 0
den[1] = 0

for (n in 2:N) {
  num[n] = num[n-1]+x[n]*x[n-1]
  den[n] = den[n-1]+(x[n-1])**2
  t[n] = num[n]/den[n]
}

t[n]

# aplicando metodo replicas para estimar a
t2 = numeric(10000)
for (i in 1:10000) {
  x = numeric(N)
  y = numeric(N)
  num = numeric(N)
  den = numeric(N)
  t = numeric(N)
  
  y = rnorm(N)
  
  x[1] = y[1]
  
  num[1] = 0
  den[1] = 0
  
  for (n in 2:N) {
    x[n] = 0.3*x[n-1]+y[n]
    num[n] = num[n-1]+x[n]*x[n-1]
    den[n] = den[n-1] + (x[n-1])**2
    t[n] = num[n]/den[n]
  }
  t2[i] = t[n] # estimativa de a apenas usando uma replica
}

t2
mean(t2) # estimativa do parametro

## ex5b ## 

# vies = E(teta_hat-teta) = E(teta_hat - teta)
mean(t2-0.3)
mean(t2)-0.3

sd(t2)
var(t2)

# MSE: E((teta_hat - teta)**2)
eqm = mean((t2-0.3)^2)
eqm

#boxplot de vies
bias_sample = t2-0.3
boxplot(bias_sample) # mediana proxima de zero, consistencia dos estimadores

## ex 6c ##
LI = mean(t2)-qnorm(0.975,0,1)*sd(t2)/sqrt(10000)
LS = mean(t2)+qnorm(0.975,0,1)*sd(t2)/sqrt(10000)

LI
LS

# alternativa:
LI = mean(t2)-qnorm(0.975,9999)*sd(t2)/sqrt(10000)
LS = mean(t2)+qnorm(0.975,9999)*sd(t2)/sqrt(10000)

LI
LS

mean(t2) # confianca

## ex6a ##

n = 100

valor_critico = qchisq(0.95,n-1)
valor_critico

## ex6ai ##
estat = function(x) {
  (n-1)*var(x)/4
}

N = 100000
v = N

testar_regra = numeric(N)

for (j in 1:N) {
  x = rnorm(n,1,2) # sob H0
  testar_regra[j] = as.integer(estat(x) >= valor_critico)
  v[i] = var(x)
}
testar_regra

p.reject = mean(testar_regra)
p.reject # 0.04 -> bom, compativel com os dados

## ex6ii ##

estat = function(x) {
  (n-1)*var(x)/4
}

N = 100000
v = N

testar_regra = numeric(N)

for (j in 1:N) {
  x = rnorm(n,1,sqrt(10)) # sob H0
  testar_regra[j] = as.integer(estat(x) >= valor_critico)
  v[i] = var(x)
}
testar_regra

p.reject = mean(testar_regra)
p.reject # 1 -> horrivel, nao e compativel com dados

## ex6iii ##

estat = function(x) {
  (n-1)*var(x)/4
}

N = 100000
v = N

testar_regra = numeric(N)

for (j in 1:N) {
  x = rnorm(n,1,1) # sob H0
  testar_regra[j] = as.integer(estat(x) >= valor_critico)
  v[i] = var(x)
}
testar_regra

p.reject = mean(testar_regra)
p.reject # 0 -> nao é compativel com dados
