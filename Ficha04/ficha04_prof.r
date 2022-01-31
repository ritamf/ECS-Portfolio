            ################# FP 4: 2021-22

################# 1. Pareto model ############################

########## 1 a
#######one parameter unknown
###  alpha-shape parameter unknown, scale parameter fixed

 ## generate a sample with n=1000 from Pareto(shape=alfa,scale=1), 



install.packages("extraDistr")

library(extraDistr)


x<- rpareto(1000, 2,1)#Pa(shape, scale)- pdf f=a*(b^a)*(x^(-(a+1))



#or, using inverse tranform method

	#n=1000
	#alpha=2

	#u<-runif(1000)
	#x <- 1/(( 1-u)**(1/alfa))
	#x

###### 1. Using  the expression of mle for alpha

alpha.mv<-1/mean(log(x))
alpha.mv
#[1] 2.070856


###### 2. using  R commands

install.packages("stats4")
library(stats4)

  ###2.1:  mle (calculates minimum, consider -loglikelihood)

# theta=alpha		
mlogL <- function(theta) { # - log-verosimilhan�a , ie, -L*
return( -(length(x) * log(theta)-(theta+1)*sum(log(x))))
}

fit <- mle(mlogL, start=list(theta=1))  
fit
#2.070855 
summary(fit)


######2.2 using  optimize (optimize command calculates  maximums)

logL <- function(theta) { #  log-verosimilhan�a , ie, L*
	return( length(x) * log(theta)-(theta+1)*sum(log(x)))
	}

####we need to know the limits of parameter variation, so make a plot
alpha <- seq(0, 5, .1)
plot(alpha,logL(alpha)) 
plot(alpha,logL(alpha), type = "l") #to get a line

optimize(logL,lower=1,upper=3,maximum=T)
2.070872


######2.3 using uniroot (gives solutions of  dLogveros/dtheta=0)

dlogL <- function(theta) {  
return( length(x)/theta -sum(log(x)))
     }

theta <- seq(0, 6, .1)
plot(theta,dlogL(theta), type = "l" );abline(h=0, col="grey")


uniroot(dlogL,lower=1,upper=3)
#2.070871


############# 1 b
######### general case: both parameters unknown 


##### 1bi) generate values from Pareto(2,5)


n=1000
alfa=2
beta<-5
 


x<- rpareto(1000, 2,5)#Pa(shape, scale)- pdf f=a*(b^a)*(x^(-(a+1))

x
hist(x, freq=F)


####### mle estimation using expressions 
beta.mv<-min(x)#mle for beta
beta.mv
betta<-beta.mv

#5.007387

alfa.mv<-n/(sum(log(x))-n*log(betta)) #mle for alfa
alfa.mv
# 2.033058

###### 2. using R commands: mle, optimize and uniroot 
###### these commands can be used just for univariate functions, so you must substitute  beta expression in the function 

###### 2.1 mle (min loglik)

mlogL <- function(alpha) { # - log-verosimilhan�a , ie, -L*
return( -(length(x) * log(alpha)+(length(x))*alpha*log(beta.mv)-(alpha+1)*sum(log(x))))
}

fit <- mle(mlogL, start=list(alpha=1))  #veros s� pode depender de um par�metro
fit
summary(fit)
#2.033058 
###### 2.2 using optimize   (max loglik)


logL <- function(alpha) { # log-verosimilhan�a , ie, L*
return( (length(x) * log(alpha)+(length(x))*alpha*log(beta.mv)-(alpha+1)*sum(log(x))))
}
				

#it is needed to give values to the limits of the parameter interval variation  
alfa <- seq(0, 5, .1)

plot(alfa,logL(alfa), type = "l") #to get a line

optimize(logL,lower=1,upper=3,maximum=T)
#2.033038


###### 2.3. using uniroot  (likelihood equation, dL*/dtheta=0)


dlogL <- function(theta) {   

	return( length(x)/theta + length(x)*log(beta.mv) -sum(log(x)))
	}

theta <- seq(0, 6, .1)
plot(theta,dlogL(theta), type = "l" );abline(h=0, col="grey")

uniroot(dlogL,lower=1,upper=3)

# 2.033081


#############################################################################
########## 1b ii)  application to the data

x<-c(1.2,1.8,2.5,3.2,4.5,5.9,13.5,22.3,96.1)


betta.hat<-min(x)
betta.hat #mle of beta


##### 1. using the expression
 
alfa.hat<-length(x)/(sum(log(x))-length(x)*log(betta.hat))  #estim mv dando express�o
alfa.hat


##### 2. using mle 

mlogL <- function(alpha) { # - log-verosimilhan�a , ie, -L*
return( -(length(x) * log(alpha)+length(x)*alpha*log(betta.hat)-(alpha+1)*sum(log(x))))
}

fit <- mle(mlogL, start=list(alpha=0.5))  #veros s� pode depender de um par�metro
fit 



##### 3. using  optimize 

logL <- function(theta) { #  log-verosimilhan�a , ie, L*
	return( length(x) * log(theta)+length(x)*theta*log(betta.hat)-(theta+1)*sum(log(x)))
				}

### limits of variation  pra o intervalo de varia��o dos parametros 
  
alfa <- seq(0, 5, .1)
plot(alfa,logL(alfa)) 
plot(alfa,logL(alfa), type = "l") #para ficar uma linha


optimize(logL,lower=0,upper=2,maximum=T)



##### 4. using   uniroot

dlogL <- function(alpha) { ##equa��o de verosimilhan�a, dL*/dalpha=0

return( length(x)/alpha +length(x)*log(betta.hat)-sum(log(x)))
	}

alfa <- seq(0, 6, .1)
plot(alfa,dlogL(alfa), type = "l" );
abline(h=0, col="blue")


uniroot(dlogL,lower=0,upper=1)


#############################################################################
####################    Exercise 2 
######  MLE for parameters of  beta distribution
##############################################################################

###### beta (3,2)
a<-3
b<-2
n<-100

x<-rbeta(n,a,b)

slogx<- sum(log(x))
slogx
slog1<-sum(log(1-x))
slog1


LL<-function (theta,slogx,slog1,n){
	a<-theta[1]
	b<-theta[2]
	loglik<-n*log(gamma(a+b))-n*log(gamma(a))-n*log(gamma(b))+(a-1)*slogx+(b-1)*slog1
                     +(a-1)*slogx+(b-1)*slog1
	-loglik
	}

optim(c(1,1),LL, slogx=sum(log(x)),slog1=sum(log(1-x)),n=n)



maxveros<-replicate(10000,expr = {
	x<-rbeta(100,3,2)
	optim(c(1,1),LL,slogx=sum(log(x)),slog1=sum(log(1-x)),n=n)  $par
	})


colMeans(t(maxveros))


########################################################################
###########  Exercise 3  
########################################################################

### mixture,  lambda1=0.6, lambda2=0.25, lambda3=0.15

########### 3 a
 
######mle
n<-200
lambda<-c(0.6,0.25,0.15)

#define likelihood function
LL<- function(lambda,y){
	lambda3<- 1-sum(lambda[1:2])
	f1<-dgamma(y,shape=1/2,rate=1/(2*lambda[1]))
	f2<-dgamma(y,shape=1/2,rate=1/(2*lambda[2]))
	f3<-dgamma(y,shape=1/2,rate=1/(2*lambda3))
	f<-f1/3+f2/3+f3/3  #density of the mixture
	return(-sum(log(f)))          # -loglikelihood
	}

#define mixture
lamb<-sample(lambda, size=200,replace=TRUE) #lambdas of the mixture, with same weight
lamb

#sample of values y
y<-rgamma(n,shape=0.5, rate=1/(2*lamb))
y

opt<-optim(c(0.5,0.3),LL,y=y)
opt

       
theta<-c(opt$par, 1-sum(opt$par))
theta


#at home, repeat the process, and calculate sample means. 
 #colMeans(t(maxveros))
