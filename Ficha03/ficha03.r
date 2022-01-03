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
