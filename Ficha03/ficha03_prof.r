
######################## Folha pratica 3  2021/22 ##################



################################################################
###################### exercicio 1 #############################
################################################################


### Metropolis-Hastings simulando da distribui��o Rayleigh, com a proponente  q(y|x_t) 
###   com qui-quadrado(x_t)

    ### 1� definir a densidade da distribui��o Rayleigh

   f<-function(x,sigma){
    x/sigma^2*exp(-x^2/(2*sigma^2))      # Densidade da dist. Rayleigh 
   }

		#f<-function(x,sigma){   #se quisesse colocar restri��es no suporte e parametros
 				#if (any (x<0))return (0)
  				#stopifnot(sigma>0)
  			#return (x/sigma^2*exp(-x^2/(2*sigma^2)))      # Densidade da dist Rayleigh 
  		    #}
  

    ### 2� definir o algoritmo Metropolis-Hastings

  
  m<-20000   #comprimento da cadeia
  sigma<-4
  x<-numeric(m)


x[1]<-rchisq(1, df=1)  #valor inicial para a 1� observ de x
 
# OBS: em cada transi��o o ponto candidato y � gerado de qui-quadrado com gl=x(i-1)

k<-0
u<-runif(m)


for (i in 2:m){
   xt<-x[i-1]
   y<-rchisq(1,df=xt) #distribui��o proponente
   num<-f(y,sigma)*dchisq(xt,df=y)
   den<-f(xt,sigma)*dchisq(y,df=xt)
   if (u[i]<=num/den) x[i]<-y else{
      x[i]<-xt  #y � rejeitado
	k<-k+1         }
   }


print(k)
#8128/20000, freq. relativa das rejei��es. aprox 40,1% rejei�oes



index<-9500:10000 
y1<-x[index]    #apenas estamos a pedir os valores a partir da itera��o 9500-10000
y1


par(mfrow=c(1,2))
#par(mfrow=c(1,1))
hist(y1)# frequencias absolutas 
lines(density(y1))


     #plot(density(y1))   #kernel-  linha-fdp estimada



#sobrepondo:

hist(y1, fre=FALSE)
lines(density(y1))  




par(mfrow=c(1,2))
#par(mfrow=c(1,1))
hist(x)# frequencias absolutas 
lines(density(y))


     #plot(density(y1))   #kernel-  linha-fdp estimada



#sobrepondo:

hist(x, fre=FALSE)
lines(density(x))  




#########################################################################
####################   exercicio 2  #####################################
#########################################################################
  
############  alinea a 

###### OBS: a distribui��o proponente q(y|x_n)= N(x_n,sigma^2)


## Tomar sigma=0.05
  
m<-10000   #(comprimento da cadeia)
sigma<-0.05
x<-numeric(m)
n<-4    #gl da t student


x[1]<-rnorm(1, 0,sigma)  #normalmente usa-se a dist proponente do Passeio Aleat�rio

k<-0
u<-runif(m)

for (i in 2:m){
   xt<-x[i-1]
   y<-rnorm(1,xt,sigma) #porque  y|x_t ~ N(x_t,sigma)
   num<-dt(y,n)
   den<-dt(xt,n)
   if (u[i]<=num/den) x[i]<-y else{
      x[i]<-xt
      k<-k+1   #y � rejeitado
      }
   }
print(k)   

(k/10000)*100 #%percentagem de rejei�oes


#####################################################

#################  alinea b 


#### 1. sigma=sqrt(0.05)
  
m<-10000   #(comprimento da cadeia)
sigma<-sqrt(0.05)
x<-numeric(m)
n<-4    #gl da t student

x[1]<-rnorm(1, 0,sigma)  # em cada transi��o o ponto candidato y � gerado daqui
k<-0
u<-runif(m)

xt<- x[i-1]

for (i in 2:m){
   xt<-x[i-1]
   y<-rnorm(1,xt,sigma)
   num<-dt(y,n)
   den<-dt(xt,n)
   if (u[i]<=num/den) x[i]<-y else{
      x[i]<-xt
      k<-k+1   #y � rejeitado
      }
   }
print(k)   #n� de rejei��es

sigma<-sqrt(0.05)



#####  2.  sigma=sqrt(0.5)



m<-10000   #(comprimento da cadeia)
sigma<-sqrt(0.5)
x<-numeric(m)
n<-4

x[1]<-rnorm(1, 0,sigma)  # em cada transi��o o ponto candidato y � gerado daqui
k<-0
u<-runif(m)

xt<- x[i-1]

for (i in 2:m){
   xt<-x[i-1]
   y<-rnorm(1,xt,sigma)
   num<-dt(y,n)
   den<-dt(xt,n)
   if (u[i]<=num/den) x[i]<-y else{
      x[i]<-xt
      k<-k+1   #y � rejeitado
      }
   }
print(k)   #n� de rejei��es

sigma<-sqrt(0.5)



#####  3.  sigma=sqrt(2)

m<-10000   #(comprimento da cadeia)
sigma<-sqrt(2)
x<-numeric(m)
n<-4

x[1]<-rnorm(1, 0,sigma)  # em cada transi��o o ponto candidato y � gerado daqui
k<-0
u<-runif(m)

xt<- x[i-1]

for (i in 2:m){
   xt<-x[i-1]
   y<-rnorm(1,xt,sigma)
   num<-dt(y,n)
   den<-dt(xt,n)
   if (u[i]<=num/den) x[i]<-y else{
      x[i]<-xt
      k<-k+1   #y � rejeitado
      }
   }
print(k)   #n� de rejei��es

sigma<-sqrt(2)




##### sigma=4

m<-10000   #(comprimento da cadeia)
sigma<-4
x<-numeric(m)
n<-4

x[1]<-rnorm(1, 0,sigma)  # em cada transi��o o ponto candidato y � gerado daqui
k<-0
u<-runif(m)

xt<- x[i-1]

for (i in 2:m){
   xt<-x[i-1]
   y<-rnorm(1,xt,sigma)
   num<-dt(y,n)
   den<-dt(xt,n)
   if (u[i]<=num/den) x[i]<-y else{
      x[i]<-xt
      k<-k+1   #y � rejeitado
      }
   }
print(k)   #n� de rejei��es
sigma



####### alternativa 
#gerar 4 cadeias com diferentes valores da variancia na proponente

n<-4 #graus de liberdade para a distribui��o alvo, t-Student

sigma<-c(0.05,0.5,2,16)
sigma

#colocar o algoritmo anterior 


 #k:  666 2018 3613  6614


#########################################################################
################  alinea c   


#comparar os quantis da distribui��o alvo (t-student) com a proponente  Normal


#1� caso de sigma =0.05



  
m<-10000   #(comprimento da cadeia)
sigma<-sqrt(0.05)
x<-numeric(m)
n<-4

x[1]<-rnorm(1, 0,sigma)  # em cada transi��o o ponto candidato y � gerado daqui
k<-0
u<-runif(m)

xt<- x[i-1]

for (i in 2:m){
   xt<-x[i-1]
   y<-rnorm(1,xt,sigma)
   num<-dt(y,n)
   den<-dt(xt,n)
   if (u[i]<=num/den) x[i]<-y else{
      x[i]<-xt
      k<-k+1   #y � rejeitado
      }
   }
#print(k)   #n� de rejei��es


###### Compara��o da distribui��o te�rica e emp�rica
b <- 1001 # burn in sample fora
dados.x <- x[b:10000]

summary (dados.x)





#####  2� caso sigma=sqrt(0.5)


m<-10000   #(comprimento da cadeia)
sigma<-sqrt(0.5)
x<-numeric(m)
n<-4

x[1]<-rnorm(1, 0,sigma)  # em cada transi��o o ponto candidato y � gerado daqui
k<-0
u<-runif(m)

xt<- x[i-1]

for (i in 2:m){
   xt<-x[i-1]
   y<-rnorm(1,xt,sigma)
   num<-dt(y,n)
   den<-dt(xt,n)
   if (u[i]<=num/den) x[i]<-y else{
      x[i]<-xt
      k<-k+1   #y � rejeitado
      }
   }
print(k)   #n� de rejei��es


###### Compara��o da distribui��o te�rica e emp�rica
b <- 1001 # burn in sample fora
y <- x[b:10000]

summary (y)




# 3� caso sigma=2

 
m<-10000   #(comprimento da cadeia)
sigma<-sqrt(2)
x<-numeric(m)
n<-4

x[1]<-rnorm(1, 0,sigma)  # em cada transi��o o ponto candidato y � gerado daqui
k<-0
u<-runif(m)

xt<- x[i-1]

for (i in 2:m){
   xt<-x[i-1]
   y<-rnorm(1,xt,sigma)
   num<-dt(y,n)
   den<-dt(xt,n)
   if (u[i]<=num/den) x[i]<-y else{
      x[i]<-xt
      k<-k+1   #y � rejeitado
      }
   }
#print(k)   #n� de rejei��es


###### Compara��o da distribui��o te�rica e emp�rica
b <- 1001 # burn in sample fora
dados.x <- x[b:10000]

summary (dados.x)

qt(0.25,4)
qt(0.5,4)
qt(0.75,4)

#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-3.3010 -0.5260  0.1219  0.1558  0.8744  4.3380 

 #Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-6.52900 -0.75960 -0.01237 -0.05555  0.69220  5.80200 
> 


     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
-8.253000 -0.716200  0.009281  0.006351  0.759300  6.743000 
> 
> qt(0.25,4)
[1] -0.7406971
> qt(0.5,4)
[1] 0
> qt(0.75,4)
[1] 0.7406971


##############################################################
#######################  exercicio 3  #########################
###############################################################

#######################  Amostrador independente  ############
###############################################################
 
 #misturas de normais e quer-se estimar o parametro do peso da mistura


m<-10000  #comprimento da cadeia
xt<-numeric(m)
a<-1 #parametro da dist. proponente beta(a,b)
b<-1  #parametro da dist. proponente beta(a,b)

p<-0.2  # par�metro da mistura
n<-30 #tamanho da amostra
mu<-c(0,5) #parametro de densidades normais
sigma<-c(1,1)


   #gerar amostra observada da mistura
i<-sample(1:2,size=n, replace=TRUE,prob=c(p,1-p))
i
x<-rnorm(n,mu[i],sigma[i])
x

   # implementar M-H usando amostrador da cadeia independente 

u<-runif(m)
y<-rbeta(m,a,b)  #distribui��o proponente
xt[1]<-0.5  #valor inicial

  for(i in 2:m){
     fy<-y[i]*dnorm(x,mu[1],sigma[1])+
         (1-y[i])*dnorm(x,mu[2],sigma[2])
     fx<-xt[i-1]*dnorm(x,mu[1],sigma[1])+
         (1-xt[i-1])*dnorm(x,mu[2],sigma[2])

     r<-prod(fy/fx)*
         (dbeta(xt[i-1],a,b))/(dbeta(y[i],a,b))


     if (u[i]<=r) xt[i]<-y[i] else
          xt[i]<-xt[i-1]
     }
xt
mean(xt) #estimativa do peso da mistura


plot (xt, type="l", ylab="p")
hist(xt[1000:m],main="", xlab="p",prob=TRUE) #hist baseado freq absolutas
print(mean(xt[1001:m]))


###### Sugest�o de TPC: Mudar para beta  (5,2)



######################################################################
############################ Exercicio 4 ############################
####################################################################
  
##### Sugerir como trabalho para casa




##########################################################################
################### exercicio 5 #####################################
######################################################################


#inicializar constantes e par�metros

N<-10000 #comprimento da cadeia
burn<-2000  #aquecimento

X<-matrix(0,N,2)  #cadeia, amostra bivariada
X

rho=-0.75   #correla��o
mu1<-0
mu2<-2
sigma1<-1
sigma2<-0.5

s1<-sqrt(1-rho^2)*sigma1  #desvio padrao da condicional completa de X1
s2<-sqrt(1-rho^2)*sigma2   #desvio padrao da condicional completa de X2



    #gerar a cadeia


X[1,]<-c(mu1,mu2)   #valor inicial
X[1,]


for (i in 2:N){
     x2<-X[i-1,2]
     m1<-mu1+rho*(x2-mu2)*sigma1/sigma2 #media da cond.compl. de X1|x2
     X[i,1]<-rnorm(1,m1,s1) #gerar valor de x1, usando a correspondente cond compl.
     x1<-X[i,1] #valor gerado anteriormente para x1
     m2<-mu2+rho*(x1-mu1)*sigma2/sigma1 #media da cond.compl. de X2|x1
     X[i,2]<-rnorm(1,m2,s2) #gerar valor de x2, usando a correspondente cond compl
     }

X


##### concluida a gera�ao de valores (pares) da normal multivariada


###usar library(coda)para ver trajetoria, estudar a convergencia da CM

install.packages("coda")
library(coda)

mcmc(X)
plot(mcmc(X))

#---------- plot dos percentis (evolu��o)
cumuplot(mcmc(X))


geweke.diag(mcmc(X))#valor do teste (compara m�dias)

geweke.plot(mcmc(X),frac1=0.1,frac2=0.5)


b<-2000
x<-X[b:N,]

geweke.diag(mcmc(x), frac1=0.1, frac2=0.5)

geweke.plot(mcmc(x),frac1=0.1,frac2=0.5)

#####Determinar caracteristicas amostrais importantes:



 colMeans(x)  #m�dias por colunas

summary(x)#matriz de covariancias

 cov(x)
#  V(X1)=0.991 (comparar com 1)
#  V(X2)=0.2521 (comparar com 0.25)
  
 cor(x)#-0.7457 (comparar com 0.75)

plot (x,main="",cex=0.5,xlab=X[1],ylab=X[2],ylim=range(x[,2]))
plot(x)





##############################################################
##############    exercicio 6   #############################
######## Zero Inflated Poisson model - (ZIP) model ###########
#############################################################

#############################  alinea b  #####################
## gerar amostra com 100 valores do modelo ZIP

p<-0.3
lambda<-2
n<-100


y<-rpois(n,lambda)
y 
r<-rbinom(n,1,p)
r

x<-y*r
x #dados de Poisson com infla��o de zeros (Zero Inflated Poisson model) 


N<-10000


M<-matrix(0,N,2)
M  #contem os valores gerados de  lambda e  p
#d<-matrix(0,N,2)    #contem os valores da fdp  dos valores gerados (n�o necessario no GIBBS)
a<-10^(-4)  #valor atribuido ao hiperparametro da dist. gama 
b<-10^(-4)   #valor atribuido ao hiperparametro da dist. gama

somax<-sum(x)
somar<-sum(r)

M[1,]<-c( 2.2,0.33)  #valores iniciais para lambda e p, proximos dos exatos
M[1,]
#d[1,]<-c(dgamma(M[1,1],a+somax,b+somar),dbeta(M[1,2],1+somar,n+1-somar))  #nao necessario para GIBBS 
#d[1,]

#pa:parametro da Bernoull
for (i in 2:N){
  pa<-(M[i-1,2]*exp(-M[i-1,1]))/(M[i-1,2]*exp(-M[i-1,1])+((1-M[i-1,2])*(x==0)))
  r<-rbinom(n,1,pa)#gear valores dos ri
  somar<-sum(r)
  M[i,1]<-rgamma(1,a+somax,b+somar)  #gerar lambda 
# d[i,1]<-dgamma(M[i,1],a+somax,b+somar)  #nao necessario para  Gibbs   
  M[i,2]<-rbeta(1,1+somar,n+1-somar) #gerar p
# d[i,2]<-dbeta(M[i,2],1+somar,n+1-somar)	#nao necessario para  Gibbs    
}

M  #valores da matriz 10000 linhas e 2 colunas


######################## alinea c ##########################
#### Gibbs 
   
#estudar se a CM � convergente

install.packages("coda")
library(coda)

mcmc(M)

plot(mcmc(M))  #trajetorias da cadeia e densidade

#---------- plot dos percentis
cumuplot(mcmc(M)) #evolution of quantiles
#com base na analise da evolu�ao da mediana es colher o periodo burn-in b  

geweke.diag(mcmc(M), frac1=0.1, frac2=0.5) #teste de convergencia de Geweke
#comparar |valores obtidos| com o quantil 1.96 (alfa==.05)
 



b<-3000
z<-M[b:N,]#amostra que vamos considerar para estimar os parametros(lambda , p) 
z  #valores gerados


colMeans(z)

summary(z[,1])
summary(z[,2])
summary(z) #escrevendo de uma vez s�

#os valores m�dios obtidos (ou valores das medianas se distribui�ao muito assim�trica), 
#s�o as estimativas bayesianas para os parametros


par(mfrow=c(1,2))
	

hist(z[,1], fre=FALSE)
lines(density(z[,1]),col=2)
hist(z[,2], fre=FALSE)
lines(density(z[,2]),col=2)



#OBS: se quisessemos obter os valores da fdp dos valores gerados para depois se fazer o grafico, teria 
#de se usar  as instru��es que est�o em comentarios e associadas � matriz d

	#dd<-d[b:N,]   #valores da fdp dos gerados  n�o � precisa no Gibbs
	#dd
	#plot(z[,1],dd[,1])
	#lines(density(z[,1]),col=2)
	#plot(z[,2],dd[,2])
	#lines(density(z[,2]),col=2)



######################## exercicio 7   #################
####################### Pereira e Turkman

####################  alinea b)  #########################

# carregar os valores para o R

			# se estivesse em ficheiro teriamos de usar p.e.:
                  #dados <- data.matrix(read.table("dados.txt"))
                  #head(dados)
                  #x <- c(dados[,1])


x<-c(4.0,4.1,3.9,4.4,3.2,4.0,3.7,4.2,4.5,4.3,3.6,1.9,3.3,1.9,2.9,2.7,2.4,2.9,
3.8,3.5,2.7,3.9,2.8,3.3,2.9,3.8,4.4,5.1,5.2,7.2,6.2,4.8,4.0,2.7,4.4,3.4,4.2,
4.8,5.3,4.5,4.1,4.0,2.9,0.8,5.2,7.3,5.1,5.3,7.1,8.1,7.8,6.9,7.5,6.0,5.0,5.3,
4.8,4.3,5.8,4.6,4.5,4.1,4.6,6.4,6.3,6.2,6.2,6.8,7.5,7.4,7.0,6.7,7.5,6.1,5.7,
5.4,5.3,4.0,3.7,2.5,0.8,1.3,3.3,4.1,5.7,4.3,3.5,3.8,2.0,3.8,4.1,1.8,3.0,4.7,
6.2,6.0,5.3,4.4,3.4,4.7,4.5,3.7,4.3,1.6,2.9,3.6,3.7,3.9,4.6,5.0,5.3,4.7,6.5, 
5.7,5.8,8.0,7.4,6.1,7.6)


n<-length(x)

n


#valores iniciais dos par�metros
N<-7000   #11000           
M<-matrix(0,N,3) 
M    

M[1,1]<- alfa0 <- 0.25
M[1,2]<- gama0 <- 0.05
M[1,3]<- rho0 <- 0.25 

#somas s0 e s1
x0<-x[2:n]
S0<-sum(x0)
S0

x1<-x[1:(n-1)]
S1<-sum(x1)
S1



###############  

#### express�o que gera valores com dist. exponencial � esquerda, usando o m�todo da transforma�ao inversa
 
  f <- function(u,b,d){
     	( log(1+(exp(b*d) - 1 ) * u )) / b  
     }

# alternativamente para obrigar a satisfazer as restri��es do suporte 
#  f <- function(u,b,d){
#     	((log(  1+(exp(b*d) - 1 ) * u )) / b)*(((log(  1+(exp(b*d) - 1 ) * u )) / b)>0)*
#(((log(  1+(exp(b*d) - 1 ) * u )) / b)<d)  
#     }



##### continuando 

gama_ast <- function(r){
	for (j in 2:n)
		T[j-1] <-x[j]-r*x[j-1]
	min(T)
}
rho_ast <- function(g){
	T[1] <- 1
	for (j in 2:n)
		T[j] <- (x[j]-g)/x[j-1]
	min(T)
}

#constru��o da cadeia
for (i in 2:N){

	#gerar valores de alfa
	M[i,1] <- rgamma(1, n ,S0-M[i-1,3]*S1-(n-1)*M[i-1,2] )
	
	u<-runif(1)
	#gerar valores de gama
	M[i,2]<-f(u, (n-1)*M[i,1],gama_ast(M[i-1,3]))
	
	#valores de rho
	M[i,3]<-f(u,M[i,1]*S1,rho_ast(M[i,2]))	

}

#####head(M)

#burn-in 
b <- 2001  #ajusta-se usando o cumplot (� frente)
V<-M[b:N,]
V


######### avaliar a convergencia - package CODA
library(coda)

mc<-mcmc(M)
#mc
plot(mcmc(M))#trajetorias da cadeia e densidade

cumuplot(mcmc(M)) # evolu��o dos quantis



geweke.diag(mc, frac1=0.1, frac2=0.5)
  #comparar |valores obtidos| com o quantil 1.96 (alfa==.05)
  
  #pelo teste de igualdade de m�dias, n�o h� motivos para rejeitar a hipotese 
   #nula, logo n�o h� motivos para rejeitar a igualdade de m�dias, passa no teste
   #de converg�ncia
  
geweke.plot(mc,frac1=0.1,frac2=0.5)
  #pelos gr�ficos, a maior parte dos valores ( ou todos ) est�o entre as bandas
   #ou seja,n�o h� motivos para rejeitar a hipotese nula

#as duas partes da cadeia s�o da mesma distribui��o.



#estimativas
colMeans(V) 



summary(V[,1])
summary(V[,2])
summary(V[,3])

summary(V)


par(mfrow=c(3,1))
hist(V[,1]) #relativ. a alfa
hist(V[,2]) #relativ. a gama
hist(V[,3]) #relativ. a rho





##########################################################
###################  exercicio 8
##########################################################



			# Defini�ao dos par�metros/vari�veis
				y<-matrix(0,4,4)
				y[1,]=c(3.28,3.09,3.03,3.03) #1st line, type 1
				y[2,]=c(3.52,3.48,3.38,3.38)		
				y[3,]=c(2.88,2.8,2.81,2.76)
				y[4,]=c(3.34,3.38,3.24,3.26)
				y

				k=4
				n1=n2=n3=n4=n=4
				N=n1+n2+n3+n4
				T=20000
				sa<-numeric(T)
				s<-numeric(T)
				u<-numeric(T)
				alfa<-matrix(0,T,k)

			# Inicializar a cadeia
				sa[1]=0.5
				s[1]=0.5
				u[1]=2
				alfa[1,]=c(0,0,0,0)


				soma<-function(u,alfa){
					soma=0
					for(w in 1:4){
						for(r in 1:4){
							soma=soma+(y[w,r]-u-alfa[w])^2	
						}
					}			
				return(soma)
				}


			# Gerar a cadeia
				for(i in 2:T){
					# Gerar alfas
						m<-numeric(4)
						v<-numeric(4)
						for(j in 1:4){
							m[j]=n*sa[i-1]*(mean(y[j,])-u[i-1])/(n*sa[i-1]+s[i-1])
							v[j]=(sa[i-1]*s[i-1])/(n*sa[i-1]+s[i-1])
							alfa[i,j]<-rnorm(1,m[j],sd=sqrt(v[j]))
						}				
						
					# Gerar miu
						m1=mean(y)-mean(alfa[i,])
						v1=s[i-1]/(N*k)
						u[i]<-rnorm(1,m1,sd=sqrt(v1))
										

					# Gerar sa
						a=k/2-1
						b=0.5*sum(alfa[i,]^2)
						g<-rgamma(1,a,b)
						sa[i]=1/g
					# Gerar s
						c=N*k/2-1
						d=0.5*soma(u[i],alfa[i,])
						h<-rgamma(1,c,d)
						s[i]=1/h
						
				} 

			# Determina��o do periodo burn in
				y<-matrix(0,T,7)  # matriz dos par�metros
				y[,1]=sa			
				y[,2]=s
				y[,3]=u
				y[,4]=alfa[,1]
				y[,5]=alfa[,2]
				y[,6]=alfa[,3]
				y[,7]=alfa[,4]

				library(coda)
				cumuplot(mcmc(y))


			# Remo��o dos valores iniciais
				burn=10000
				sa=sa[burn:T]
				s=s[burn:T]
				u=u[burn:T]
				l1=alfa[burn:T,1]
				l2=alfa[burn:T,2]
				l3=alfa[burn:T,3]
				l4=alfa[burn:T,4]
				
				z<-matrix(0,(T-burn+1),7)
				z[,1]=sa;	z[,2]=s;	z[,3]=u;	z[,4]=l1
				z[,5]=l2;	z[,6]=l3;	z[,7]=l4
				

			# Dian�stico de Geweke		
				geweke.diag(z,frac1=0.1, frac2=0.5)
				alfa=0.05
				qnorm(1-alfa/2)
				
			# Representa��o gr�fica das densidades
				plot(mcmc(x))

				plot(density(sa*(sa<2)),main="sa")
				plot(density(s),main="s")
				plot(density(u),main="u")
				plot(density(l1),main="l1")
				plot(density(l2),main="l2")
				plot(density(l3),main="l3")
				plot(density(l4),main="l4")


			# Obten��o das carateristicas da amostra
				summary(z)
