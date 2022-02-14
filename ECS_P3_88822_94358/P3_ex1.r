#Alunos: Diogo Pedrosa - 94358, Rita Ferrolho - 88822

library(stats4)
caudais = c(23.8,8.5,44.8,51.0,4.8,19.1,47.8,25.4,14.3,66.2,37.0,48.6,28.3,21.0,72.5,28.3,7.9,47.1,64.4,10.7,19.6,19.0,16.9,22.7,65.3,33.3,31.0,14.2,7.3,73.4,44.8,50.2,40.4,57.6,32.6,24.0,31.0,73.4,33.9,84.0,17.0,23.7,74.7,31.2,33.1)	
x = caudais

# estimativas iniciais
sigma = (sqrt(6)/pi)*sd(caudais)
mu = mean(caudais)-0.5772*sigma
est_iniciais = c(mu,sigma)
est_iniciais


# Problema bidimensional - optim

LL <- function(theta,N){
  med <- theta[1]
  sig <- theta[2]
  return (-(-N*log(sig) +  sum(-(x-med)/sig - exp(-(x-med)/sig)) ))
}

a = optim(c(mu,sigma),LL,N=length(x))

optim_mu = a$par[1]
optim_mu  # 26.33968
optim_sig = a$par[2] 
optim_sig # 16.65487 

abcissas = seq(min(caudais),max(caudais), length.out=1000)
par(mfrow = c(1,1))
hist(caudais,breaks=20,main="",freq = FALSE,ylim=c(0,0.03),xlab="Caudais",ylab="Frequencia relativa")

library(extraDistr)
lines(abcissas, dgumbel(x=abcissas,mu=optim_mu,sigma=optim_sig), col = "red")
lines(abcissas, dgumbel(x=abcissas,mu=mu,sigma=sigma), col="blue")
legend("topright",legend=c("Maxima Verosimilhanca, optim", "Metodo dos Momentos"),col=c("red", "blue"), lty=1, cex=0.8,box.lty=0)



# Problema unidimensional - optimize

MD <- function(sig){
  return (sig*log(length(x)/(sum(exp(-x/sig)))))
}

logL <- function(sig) { #  log-verosimilhanca
  med = MD(sig)
  return( -length(x)*log(sig) +  sum( -(x-med)/sig - exp(-(x-med)/sig) ) )
}

optimize_mu = MD(optimize_sig)
optimize_mu # 26.34149
optimize_sig = optimize(logL,lower=0,upper=100,maximum=T)$maximum # 16.65789
optimize_sig # 16.65789


par(mfrow = c(1,2))
hist(caudais,breaks = 20,main="", freq = FALSE, ylim=c(0,0.03),xlab="Caudais",ylab="Frequência relativa")
abcissas = seq(min(caudais),max(caudais), length.out=1000)
lines(abcissas, dgumbel(abcissas, mu, sigma), col = "blue")
lines(abcissas, dgumbel(abcissas, optimize_mu, optimize_sig), col = "orange")
legend("topright",legend=c("Maxima Verosimilhança, optimize()", "Metodo dos Momentos"),col=c("orange","blue"),lty=1,cex=0.8,box.lty=0)

m = seq(from=10,to=20,by=0.01)
y_logL = numeric(length(m))
for(i in 1:length(m)){
  y_logL[i] = logL(m[i])
}
plot(m,y_logL, type = "l",xlab="sigma",ylab="logL")




# Problema unidimensional - uniroot

dlogL <- function(sig) { # equacao de verosimilhanca, d_L/d_sigma=0
  med = MD(sig)
  return( -(length(x)/sig)+(1/sig^2)*sum((x-med)*(1-exp(-(x-med)/sig))) )
}

uniroot_mu = MD(uniroot_sig)
uniroot_mu # 26.34149
uniroot_sig = uniroot(dlogL,lower=1,upper=100)$root 
uniroot_sig # 16.65789


par(mfrow = c(1,2))
hist(caudais,breaks = 20,main="", freq = FALSE, ylim=c(0,0.03),xlab="Caudais",ylab="Frequencia relativa")
abcissas = seq(min(caudais),max(caudais), length.out=1000)
lines(abcissas, dgumbel(abcissas, mu, sigma), col = "blue")
lines(abcissas, dgumbel(abcissas, uniroot_mu, uniroot_sig), col = "orange")
legend("topright",legend=c("Maxima Verosimilhanca, uniroot()", "Metodo dos Momentos"),col=c("gray","blue"),lty=1,cex=0.8,box.lty=0)

m = seq(from=10,to=20,by=0.01)
y_dlogL = numeric(length(m))
for(i in 1:length(m)){
  y_dlogL[i] = dlogL(m[i])
}
plot(m,y_dlogL, type = "l",xlab="sigma",ylab="dlogL") 
abline(h=0, col="blue")


### Analise de Resultados

#Metodo dos Momentos
rbind(mu,sigma)

#Rotinas R
mu.hat = c(optim_mu,optimize_mu,uniroot_mu)
mu.hat
sigma.hat = c(optim_sig,optimize_sig,uniroot_sig)
sigma.hat
mu_erro = 100*abs(mu-mu.hat)/mu
mu_erro
sigma_erro = 100*abs(sigma-sigma.hat)/sigma
sigma_erro

rbind(mu.hat,mu_erro,sigma.hat,sigma_erro)

#Resultado Final
mu_final = mean(mu.hat)
sigma_final = mean(sigma.hat)
var_mu = var(mu.hat)
var_sigma = var(sigma.hat)

rbind(mu_final,sigma_final)

