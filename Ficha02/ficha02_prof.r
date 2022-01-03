#######################################################################
#######################  FP 2   2021/22  #############################
#######################################################################


#################################################################
######## exercicio 1  ###########################################
##################################################################

##### 1 a  

 N=10**3
  z<-rcauchy(N)
  p.hat<-sum(z>2)/N #frequencia relativa do n� observa�oes>2
  p.hat

  p<-pcauchy(-2) #fd da Cauchy calculada em (-2)
  p

dif<-abs(p.hat-pcauchy(-2))
dif

### or, for several size values  and comparing with true value

for(i in 3:5){
  N=10**i
  z<-rcauchy(N)
  p.hat<-sum(z>2)/N #relative frequency, estimates
  p<-pcauchy(-2)
  dif<-abs(sum(z>2)/N-pcauchy(-2))
  mylist<-list(N=N,p.hat=p.hat,p=p,dif=dif) #list the information
  print(mylist)
}


###### 1 b

x<-runif(1000,0,2)  #gerar valores da uniforme U(0,2) 
 integ1<-function(x){1/(pi*(1+ x**2))}
z<-integ1(x)
mean(z) 
est.int<-1/2-2*mean(z) # estimativa de Monte Carlo para  p
est.int
dif<-abs(est.int-pcauchy(-2))
dif


x<-runif(10^5,0,2)   
 integ1<-function(x){1/(pi*(1+ x**2))}
z<-integ1(x)
mean(z)
est.int<-1/2-2*mean(z)
est.int
dif<-abs(est.int-pcauchy(-2))
dif



###### 1 c

#########  estimar a variancia
################ usando valor de 1a

#N=10**3

  p.hat

  p<-pcauchy(-2)
  p

variancia<-p*(1-p)/N
variancia #valor real da variancia
est.variancia<-p.hat*(1-p.hat)/N
est.variancia #estimativa da variancia


################ usando 1 b

estimativa.variancia<-est.int*(1-est.int)/1000

estimativa.variancia



#qual o melhor m�todo???


###############################################################################
############################### exercicio 2  ##################################
###############################################################################



######## 2 a   , gerar da Exp(3)

 x<-rexp(1000,3)   
 mean((1/3)*(log(x))*x**5) #estimativa
 
      
v<- var((1/3)*(log(x))*x**5)# variancia amostral dos valores transformados
erro<-sqrt(v/1000)  # estimativa do erro  de Monte Carlo 
erro




######## 2 b   , usar Gama(6,3)

x<-rgamma(1000,6,3)

 # poderiamos proceder como anteriormente e definir a fun��o, fazendo:   
 #integ2<-function(x){(gamma(6)/3**6)*(log(x))}
 #z2<-integ2(x)
 #mean(z2)


#ou calculando diretamente, a m�dia pretendida:
mean((gamma(6)/3**6)*(log(x)))# estimativa de MC do integral

var((gamma(6)/3**6)*(log(x))) #sample variance 
z2<-var((gamma(6)/3**6)*(log(x)))
erro<-sqrt(z2/1000)#Monte carlo error
erro


########  2 c , m�todo das replicas, replicando 500 vezes 

I<-numeric(500)
integ2<-function(x){(gamma(6)/3**6)*(log(x))}
for(i in 1:500){
x<-rgamma(1000,6,3)
I[i]<-mean(integ2(x))
}

mean(I)
var(I)



##### compare resultados




##############################################################################################
######################  Exercicio 3  #######################################################
############################################################################################


n<-10000

#definir fun��o integranda  g
g<-function(x){
     exp(-x-log(1+x^2))*(x>0)*(x<1)
     }

x<-runif(n)   #usar f1
fg<-g(x)

theta.hat<-mean(fg)
theta.hat




# REsolver em conjunto



#definir fun��o integranda  g

g<-function(x){
     exp(-x-log(1+x^2))*(x>0)*(x<1)
     }
n<-10000

theta.hat<-numeric(5) #definir a dimens�o 

se<-numeric(5) #variabilidade das estimativas 
erroMC<-numeric(5)  #erro de monte carlo




x<-runif(n)   #usando f1
fg<-g(x)
theta.hat[1]<-mean(fg)
theta.hat[1]
se[1]<-sd(fg)
erroMC[1]<-sqrt(var(fg)/10000)
erroMC[1]



x<-rexp(n,1)   #using f2
fg<-g(x)/exp(-x)
theta.hat[2]<-mean(fg)
theta.hat[2]
se[2]<-sd(fg)
se[2]
erroMC[2]<-sqrt(var(fg)/10000)
erroMC[2]

x<-rcauchy(n)   #usando f3
i<-c(which(x>1), which(x<0))
x[i]<-2  #para n�o se obter erros de overflow 
fg<-g(x)/dcauchy(x)
theta.hat[3]<-mean(fg)
theta.hat[3]
se[3]<-sd(fg)
se[3]
erroMC[3]<-sqrt(var(fg)/10000)
erroMC[3]

u<-runif(n)   #usar f4, metodo da transforma��o inversa
x<- - log(1-u*(1-exp(-1)))

# ou podia-se simular da exponencial e usar os valores  entre 0 e 1.

#n <- 10000
#k <- 0 # contador para o n� de observa��es
#j <- 0 # itera��es
#x <- numeric(n)
#while (k < n) {
# y<- rexp(1) # gerar valores da distribui��o proponente 
#  j <- j + 1
# if (0<y & y<1 ) {
#     # accept y
#     k <- k + 1
#     x[k] <- y
#     }
# }

fg<-g(x)/(exp(-x)/(1-exp(-1)))
theta.hat[4]<-mean(fg)
theta.hat[4]
se[4]<-sd(fg)
se[4]
erroMC[4]<-sqrt(var(fg)/10000)
erroMC[4]


u<-runif(n)   #usar f5, metodo da transforma��o inversa
x<- tan(pi*u/4) #express�o geral  

#or we could simulate from C(0,1) and keep values between o and 1

#n <- 10000
#k <- 0 # counter for the accepted observations
#j <- 0 # iterations
#x <- numeric(n)
#while (k < n) {
#y<- rcauchy(1) # generating values from proponent 
# j <- j + 1
# if (0<y & y<1 ) {
#     # accept y
#     k <- k + 1
#     x[k] <- y
#     }
# }


fg<-g(x)/(4/((1+x^2)*pi))
theta.hat[5]<-mean(fg)
theta.hat[5]
se[5]<-sd(fg)
se[5]
erroMC[5]<-sqrt(var(fg)/10000)
erroMC[5]


rbind(theta.hat,se,erroMC)


##############################################################################################
######################  Exercicio 4  #######################################################
############################################################################################


###### SEM o metodo das replicas

########### considerando Normal (1,2), sd=2 e usando amostra com 1000 observa��es

x<-rnorm(1000,1,2)
var(x)# sample variance S**2
v<-var(x)# 

qchisq(0.95,999)#90%
qchisq(0.05,999)



LI<-999*v/qchisq(0.95,999)
LS<-999*v/qchisq(0.05,999)
LI
LS

LI<-999*v/qchisq(0.975,999)
LS<-999*v/qchisq(0.025,999)
LI
LS

##### aplicando o m�todo das replicas (10000 replicas)

v<-numeric(10000)

for(i in 1:10000){

x<-rnorm(1000,1,2)

v[i]<-var(x)
}
v


mean(v)

qchisq(0.95,999)
qchisq(0.05,999)

LI<-999*mean(v)/qchisq(0.95,999)
LS<-999*mean(v)/qchisq(0.05,999)
LI
LS



########### ver o que acontece se aumentarmos ou diminuirmos a dimens�o da amostra: n=10, n=10000






##############################################################################################
######################  Exercicio 5  #######################################################
############################################################################################

############ 5a: estimar o parametro

###### SEM usar o m�todo das replicas

#### gerar a amostra

x<-numeric(100)
y<-numeric(100)

y<-rnorm(100)
y

x[1]<-y[1]
x[1]
y[1]

for(n in 2:100){        #gerar valores da serie temporal
x[n]<-0.3*x[n-1]+y[n]	}
x
 
###### calcular a estimativa particular para esta gera��o

num<-numeric(100)
den<-numeric(100)
t<-numeric(100)

num[1]<-0
den[1]<-0

for(n in 2:100){ 
num[n]<-num[n-1]+x[n]*x[n-1]
den[n]<-den[n-1]+(x[n-1])**2
t[n]<-num[n]/den[n]
}

t[n]  


######### Aplicando o m�todo das replicas para estimar "a"
 t2<-numeric(10000)
for ( i in 1:10000){
	x<-numeric(100)
	y<-numeric(100)
	num<-numeric(100)
	den<-numeric(100)
	t<-numeric(100)
	
	y<-rnorm(100)
	
	x[1]<-y[1]
	
	num[1]<-0
	den[1]<-0
	
		for(n in 2:100){
		x[n]<-0.3*x[n-1]+y[n]
		num[n]<-num[n-1]+x[n]*x[n-1]
		den[n]<-den[n-1]+(x[n-1])**2
		t[n]<-num[n]/den[n]
		}
	t2[i]<-t[n]   #estimativa de "a" apenas usando 1 replica
	}
 t2
mean(t2)  #estimativa do parametro 

############  5b

#vies:E(teta_hat-teta)

    #t2-0.3 sample with the bias

mean(t2-0.3)  
mean(t2)-0.3  

sd(t2)    
var(t2)


# MSE:E((teta_hat-teta)**2)
eqm<-mean((t2-0.3)^2)
eqm
  #eqm<-1/10000*sum((t2[i]-0.3)**2)# alternativa, mas ter� de estar dentro do ciclo for 

#boxplot do vi�s

bias_sample<-t2-0.3

boxplot(bias_sample)  



############ 6c:   95% IC para "a"

LI<- mean(t2)-qnorm(0.975,0,1)*sd(t2)/sqrt(10000)
LS<- mean(t2)+qnorm(0.975,0,1)*sd(t2)/sqrt(10000)

LI
LS

#ou

LI<- mean(t2)-qt(0.975,9999)*sd(t2)/sqrt(10000)
LS<- mean(t2)+qt(0.975,9999)*sd(t2)/sqrt(10000)

LI
LS



######################################################################
################ Exerc�cio 6 ##############################
####################################################################

######## 7a: TESTAR H0: sigma^2=4 vs  sigma^2>4 

# estimar o nivel de significancia, 
#usando uma amostra gerada de cada uma das seguintes normais 


   #i) N(1,4)
   #ii) N(1,10)
   #iii) N(1,1)

n <- 100 # dimens�o da amostra

#express�o da estat�stica do teste (n-1)*Var_amostral/sigma**2, sob H_0. 
vcrit <- qchisq(.95, n-1)  #valor critico do teste
		#vcrit <- round(vcrit,2)
vcrit

 
 #i) usar valores da N(1,4)

estat<-function(x){(n-1)*var(x)/4}

N <- 100000 # n�mero de replicas 
v <- 100000
testrule <- numeric(N) # variavel regra do teste
for (j in 1:N) {
	x <- rnorm(n,1,2) #sob H0 
			# decis�o -- 1 (rejei��o) ou 0 (n�o rejei��o)
	testrule[j] <- as.integer(estat(x) >= vcrit )
 #     v[i]<-var(x)
	}
testrule
p.reject<- mean(testrule)    # propor��o de rejei��es

 p.reject   #0.0501  faz sentido??


#ii) gerar valores da N(1,10)

#estatistica do teste
estat<-function(y){(n-1)*var(y)/4} 

N <- 100000 # n�mero de replicas 

testrule <- numeric(N) # variavel regra do teste
for (j in 1:N) {
	y <- rnorm(n,1,sqrt(10)) 
			# decis�o -- 1 (rejei��o) ou 0 (n�o rejei��o)
	testrule[j] <- as.integer(estat(y) >= vcrit ) 
	}
testrule
p.reject<- mean(testrule)    # propor��o de rejei��es

 p.reject   #1  faz sentido??



#iii) gerar valores da N(1, 1)


estat<-function(z){(n-1)*var(z)/4}

N <- 100000 # n�mero de replicas 

testrule <- numeric(N) # variavel que define regra do teste
for (j in 1:N) {
	z<- rnorm(n,1,1) #sob H0 
			# decis�o -- 1 (rejei��o) ou 0 (n�o rejei��o)
	testrule[j] <- as.integer(estat(z) >= vcrit ) 
	}
testrule
p.reject<- mean(testrule)    # propor��o de rejei��es

p.reject   #0 faz sentido??


#com estes dados faria sentido fazer o teste 
# H0: sigma =4, vs H1:  sigma <4, com os valores z de  N(1,1)
# o que vai alterar? a regra do teste


n <- 100 # sample size

vcrit <- qchisq(.05, n-1)  #critical value of the test
vcrit

#expression of the test statistics
estat<-function(w){(n-1)*var(w)/4}

N <- 100000 # replicates 

testrule <- numeric(N) # rule of the test
for (j in 1:N) {
	w <- rnorm(n,1,1) #under H0 
			# decision -- 1 (rejection) or 0 (not rejection)
	testrule[j] <- as.integer(estat(w) <= vcrit ) 
	}
testrule
p.reject<- mean(testrule)    # proportion of rejections

 p.reject   

####################################################

####### 6 b IC para sigma^2
#Testar igualdade contra desigualdade 

## dados x de  N(1,4)

n <- 100 # dimens�o da amostra 
vcrit1 <- qchisq(.975, n-1)
  
vcrit2 <- qchisq(.025, n-1)  




#i) usar valores da N(1,4)

estat<-function(x){(n-1)*var(x)/4}

N <- 100000 # n�mero de replicas 
v <- 100000
testrule <- numeric(N) # variavel regra do teste
for (j in 1:N) {
	x <- rnorm(n,1,2) #sob H0 
			# decis�o -- 1 (rejei��o) ou 0 (n�o rejei��o)
	testrule[j] <- as.integer(estat(x) >= vcrit1  | estat(x) <= vcrit2)
      v[j]<-var(x)
	}
testrule
p.reject<- mean(testrule)    # propor��o de rejei��es

 p.reject   #0.0501  faz sentido??

#v
mean(v)



LI<-99*mean(v)/qchisq(0.975,99)
LS<-99*mean(v)/qchisq(0.025,99)
LI
LS


#ou 
LI1<- mean(v)-qnorm(0.975,0,1)*sd(v)/sqrt(100000)
LS2<- mean(v)+qnorm(0.975,0,1)*sd(v)/sqrt(100000)

LI1
LS2

#conclus�o?






##############################################################################
#######################################    Ex.7  ############################
##############################################################################


########## Ler os dados


#######escrever os dados



xtabaco <- c(77,137,117,94,116,102,111,93,88,102,91,104,107,112,113,110,125,133,115,105,87,91,76,66)

ytabaco<- c(84,116,123,128,155,101,118,113,104,88,104,129,86,96,144,139,113,146,128,115,79,85,60,51)




####outra forma, fazendo a leitura atrav�s de um ficheiro

#####	Leitura dos dados de um ficheiro de texto

tabaco <- data.frame(read.table("tabaco.txt",header=T))

        
xtabaco <- c(tabaco[,1])
ytabaco <- c(tabaco[,2])
xtabaco
ytabaco


######   alinea a)	Representa��o gr�fica dos dados

plot(xtabaco,ytabaco, main="Diagrama de Dispers�o",
xlab = "N�mero de cigarros por dia", ylab = "Taxa de Mortalidade",col="purple" )

cor(xtabaco,ytabaco)


#####   alinea b 

############programar o Bootstrap:

theta.hat <- cor(xtabaco, ytabaco)
theta.hat

B <- 10000 # n�mero de r�plicas bootstrap

n <- length(xtabaco)

theta.b <- numeric(B)
for (b in 1:B) {
i <- sample(1:n, size = n, replace = TRUE) # i � o vector dos �ndices, amostrsa n�i
x <- xtabaco[i]
y <- ytabaco[i]
theta.b[b] <- cor(x,y)
}

theta.b


mean(theta.b) #estimativa bootstrap de theta.hat=0.7225272
# 0.7107367, B=1000, 0.7116035, B=10000

sd(theta.b)# estimativa do desvio padr�o

bias <- mean(theta.b) - theta.hat
bias
#[1] -0.01092369<0 , estimativa de bootstrap subestima o valor da correla��o


######	Uma outra forma de aplicar o bootstrap	aplicar a livraria TPC

#aula
#library(boot)
#boot(dados, estat�stica a estudar, n� de amostras)


?boot
library(boot)
tabaco
x <- c(tabaco[,1])
y <- c(tabaco[,2])
cor(x,y)

#pode-se usar esta fun��o boot e definindo a estatistica previamente 
#b<-boot(tabaco,estatistica,10000)


##############################################################################
#######################################    Ex.8  ############################
##############################################################################




#  intervalo de confian�a pelo met reamostragem de bootstrap



library(MASS)
?faithful
  eruptions<-faithful$eruptions
  eruptions #amostra

mean(eruptions) #estimativa do parametro a estimar (media da popula��o), usando amostra
#[1] 3.487783 m�dia amostral

n<-length(eruptions)

B <- 1000 # n�mero de r�plicas bootstrap

theta.b <- numeric(B) #amostra bootstrap

for (b in 1:B) {
i <- sample(1:n, size = n, replace = TRUE) # i � o vector dos �ndices
x <- eruptions[i]
theta.b[b] <- mean(x)

}

theta.b

 mean(theta.b)
#[1] 3.488124 estimativa bootstrap da m�dia da amostra original


#pedir 2 quantis e seriam esses os extremos do ic
q1<- quantile(theta.b,prob=0.025)
q1
q2<- quantile(theta.b,prob=0.975)
q2

#IC=(3.357695  , 3.61683 )  M�dia da amostra original 3.487783


TPC: calcular ic para desvio padr�o da amostra

##############################################################################
#######################################    Ex.9  ############################
##############################################################################




xtabaco <- c(77,137,117,94,116,102,111,93,88,102,91,104,107,112,113,110,125,133,115,105,87,91,76,66)

ytabaco <- c(84,116,123,128,155,101,118,113,104,88,104,129,86,96,144,139,113,146,128,115,79,85,60,51)



####### lendo os dados do ficheiro

  ###data <- data.frame(read.table("tabaco.txt",header=T))
  ###xtabaco<- c(data[,1])
  ###ytabaco <- c(data[,2])

########################################


cor(xtabaco, ytabaco)

n <- length(xtabaco)
theta.hat <- cor(xtabaco, ytabaco)
theta.hat


#calcular as replicas Jacknife, deixando uma observa��o de fora
theta.jack <- numeric(n)
for (i in 1:n){
theta.jack[i] <- cor(xtabaco[-i],ytabaco[-i])
}

theta.jack  # amostra dos valores de theta (correla��se para cada amostra Jack)

mean(theta.jack)  #estimativa de Jackknife para a correla��o


#[1] 0.7220034
#corr [1] 0.7225272
bias <- (n - 1) * (mean(theta.jack) - theta.hat) # estimativa jackknife do vi�s
bias
#[1] -0.01204873


#bootrt
# -0.01092369



