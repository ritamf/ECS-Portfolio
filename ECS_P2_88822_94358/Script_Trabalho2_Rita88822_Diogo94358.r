#### Alunos: Rita Ferrolho (88822) e Diogo Pedrosa (94358) ####

library(cubature)

### ex 1 ###

# I_1

f = function(x,y){
  exp(-1/2*(x**2+y**2))*(x>0)*(x<1)*(y>0)*(y<1)
}
n=1000
ValorReal = 2*pi*(pnorm(1)-pnorm(0))**2

# Tecnica de Amostragem de Importancias Uniforme, f_1
x=runif(n)
y=runif(n)
fg=f(x,y)

theta.hat = mean(fg) # Estimativa
theta.hat

desvioPadrao = sd(fg) # Desvio Padrao
desvioPadrao

erroMC = sqrt(var(fg)/1000) # Erro Monte Carlo
erroMC

# Exponencial, f_2
g = function(x,y){
  exp(-1*(x+y))
}
x = rexp(n,1) # usar f2
y= rexp(n,1)
fg = f(x,y)/g(x,y)

theta.hat = mean(fg) # Estimativa
theta.hat

desvioPadrao = sd(fg)
desvioPadrao

erroMC = sqrt(var(fg)/1000) # Erro Monte Carlo
erroMC


# I_2 

testFn0 = function(x){
  exp(1/2*(x**2))*(x>-2)*(x<2)
}
adaptIntegrate(testFn0, -2,2, tol=1e-4)

# Amostragem de Importancia
f2 = function(x,y){
  16*exp(1/2*(x**2+y**2))*(x>-2)*(x<2)*(y>-2)*(y<2)
}
n=1000

x=runif(n,-2,2)
y=runif(n,-2,2)
fg=f2(x,y)
theta.hat = mean(fg) # Estimativa
theta.hat

desvioPadrao = sd(fg)
desvioPadrao

erroMC = sqrt(var(fg)/1000) # Erro Monte Carlo
erroMC


# Metodo de Monte Carlo
n=1000
x=rnorm(n)
y=rnorm(n)
xaux = numeric(n)
yaux = numeric(n)
for (i in 1:n) {
  if(x[i]>-2 && x[i]<2){
    xaux[i] = x[i]
  }
  if(y[i]>-2 && y[i]<2){
    yaux[i] = y[i]
  }
}
f2 = function(x,y){
  2*pi*exp((x**2+y**2))*(x>-2)*(x<2)*(y>-2)*(y<2)
}

fg2=f2(xaux,yaux)
theta.hat = mean(fg2) # Estimativa
theta.hat

desvioPadrao = sd(fg2)
desvioPadrao

erroMC = sqrt(var(fg2)/1000) # Erro Monte Carlo
erroMC



## ex 2a ##

# parametros a serem usados na serie temporal
param_a = 0.4 
param_b = 0.1

t2 = numeric(1000)
for (i in 1:1000) {
  x=numeric(100)
  y=numeric(100)
  y=rnorm(100)
  x[1] = y[1] # seed
  
  num = numeric(100)
  den = numeric(100)
  t = numeric(100)
  
  num[1] = 0
  den[1] = 0
  
  for (n in 2:100) {
    x[n] = param_a*x[n-1]+param_b*x[n-1]*y[n-1] + y[n] # serie temporal
    num[n] = num[n-1]+x[n]*x[n-1]
    den[n] = den[n-1] + (x[n-1])**2
    t[n] = num[n]/den[n]
  }
  
  t2[i] = t[n] 
}

t2

a.hat=mean(t2) # Media Estimada
a.hat

vies = a.hat - param_a # Vies
vies

sd(t2) # Desvio padrao

eqm = mean((t2-0.3)^2) # Erro quadratico medio
eqm

vies_sample = t2 - param_a
boxplot(vies_sample)

# Calculo do IC de a, para alpha = 2%
LI = mean(t2) - qnorm(0.98, 0,1)*sd(t2)/sqrt(1000)
LS = mean(t2) + qnorm(0.98,0,1)*sd(t2)/sqrt(1000)

LI
LS

## ex 2b ##
#Verificar se a amostra esta enviesada
set.seed(1)

x<-numeric(100) #100 observacoes de x
y<-numeric(100) #100 observacoes de y
den<-numeric(100) #inicializacao do denominador
num<-numeric(100) #inicializacao do numerador
s<-numeric(100) #estimador de a

y<-rnorm(100) #y segue uma distribuicao normal (0,1)
x[1]<-y[1]

num[1]<-0
den[1]<-0

for (j in 2:100) {
  x[j]<-0.4*x[j-1]+0.1*x[j-1]*y[j-1]+y[j]
  num[j]<-num[j-1]+x[j]*x[j-1]
  den[j]<-den[j-1]+x[j-1]**2
  s[j]<-num[j]/den[j]
}

s[j]

# bootstrap

n = length(x)

B = 1000 # numero de replicas de bootstrap

theta.b = numeric(B)

for (b in 1:B) {
  i = sample(1:n, size = n, replace = TRUE)
  num[i] = num[i-1]+x[i]*x[i-1]
  den[i] = den[i-1] + (x[i-1])**2
  theta.b[b]=num[i]/den[i]
}
mean(theta.b)
vies = mean(theta.b) - param_a
vies
sd(theta.b)

# jackknife

J = 1000 # numero de replicas de jacknife

theta.jack = numeric(J)
for (p in 1:J) {
  x=numeric(100)
  y=numeric(100)
  y=rnorm(100)
  x[1] = y[1] # seed
  num = numeric(99)
  den = numeric(99)
  t = numeric(99)
  num[1] = 0
  den[1] = 0
  
  for (a in 2:100) {
    x[a] = param_a*x[a-1]+param_b*x[a-1]*y[a-1] + y[a] # serie temporal
  }
  jack <- numeric(length(x)-1)
  for (i in 1:length(x)){ 
    for (j in 1:length(x)){
      if(j < i) 
        jack[j] <- x[j] 
      else if(j > i) 
        jack[j-1] <- x[j]
    }
  }
  for (n in 2:99) {
    num[n] = num[n-1]+jack[n]*jack[n-1] 
    den[n] = den[n-1] + (jack[n-1])**2
    t[n] = num[n]/den[n]
  }
  
  theta.jack[p] = t[n]
  
}
mean(theta.jack)
mean(theta.jack)-param_a
sd(theta.jack)

