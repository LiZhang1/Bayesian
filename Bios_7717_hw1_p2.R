############ HW1 Problem2 #################
#### 9.22.2014 updated ###################
## estimate gamma prior
#Getting a rough estimate by the method of moments (matching up the mean=shape*scale and variance=shape*scale^2) we have
par(mfrow=c(2,3))
x=(0:450)/10
plot(x,dgamma(x,shape=60,rate=3),main="a=60 b=3",type="l",ylab="density", xlab='theta')
plot(x,dgamma(x,shape=30,rate=1.5),main="a=30 b=2",type="l",ylab="density", xlab='theta')
plot(x,dgamma(x,shape=20,rate=1),main="a=20 b=1",type="l",ylab="density", xlab='theta')
plot(x,dgamma(x,shape=10,rate=.5),main="a=10 b=0.5",type="l",ylab="density", xlab='theta')
plot(x,dgamma(x,shape=5,rate=.25),main="a=5 b=0.25",type="l",ylab="density", xlab='theta')
plot(x,dgamma(x,shape=1,rate=.05),main="a=2 b=0.05",type="l",ylab="density", xlab='theta')
par(mfrow=c(1,1))
plot(x,dgamma(x,shape=18,rate=1.111),main="a=10 b=2",type="l",ylab="density", xlab='theta')
#find Pr(X < 35):
pgamma(32,10,.5)
#0.9567017
pgamma(32,20,1)
#0.9906581
pgamma(32,30,1.5)
#0.997821
y = c(24,25,31,31,22,21,26,20,16,22)
sum(y)
mean(y)
sd(y)

### plot prior,  posterior ###
par(mfrow=c(1,1))
y = c(24,25,31,31,22,21,26,20,16,22)
poisgamp(y,20,1, plot = TRUE, suppressOutput = FALSE)
par(new=TRUE)
plot(lambda.range, L, type="l", lty=2, lwd=1.5,
   axes=FALSE, xlab="", ylab="")
axis(side=4, cex.axis=1.3)
#The following R code is used to compute the posterior distribution:
# Gelman, q. 13 page 69-70
# Poisson with gamma prior
deaths<- c(24,25,31,31,22,21,26,20,16,22)
lambda.range<-seq(0,40)
# prior
prior<-dgamma(lambda.range, shape=20, scale=1)
# log likelihood (likelihood values are too large)
log.likelihood<-sum(deaths)*log(lambda.range)-length(deaths)*lambda.range
# posterior
alpha.star<-20+sum(deaths)
beta.star<- 1+length(deaths)
post<-dgamma(lambda.range, shape=alpha.star, scale=1/beta.star)
a <- sum(deaths)+1
n <- 10
L <- dgamma(lambda.range, shape=a, scale=1/n)
ylim <- c(0, 0.3)
x=(10:35,0.01)/10
xlim <- c(10,35)
plot(lambda.range, prior,type="l",ylim=ylim,xlim=xlim, xlab=expression(theta),
     ylab="Probability density",main="Triplot for Fatal Accident",
     lty=1, lwd=1.5, cex.axis=1.3, cex.lab=1.3, bty="L",col="blue" )
lines(lambda.range, post,col="red",type="l",lty=2, lwd=1.5)
lines(lambda.range, L,col="black",type="l", lty=3, lwd=1.5)
legend(x=10,y= .3, c("Prior", "Likelihood", "Posterior"), lty = c(1, 2, 2),
       lwd = c(1.5,1.5,1.5), col = c("blue","black",  "red"))














# simulate predictive posterior data
years <- c(1976,1977,1978,1979,1980,1981,1982,1983,1984,1985)
crashes <- c(24,25,31,31,22,21,26,20,16,22)
numyears <- length(years)
sumcrashes <- sum(crashes)
# posterior from updated noninformative (Jeffrey) prior
theta <- rgamma(10000,shape=(sumcrashes+0.5),rate=(numyears+0.0001))
plot(density(theta))
theta <- rgamma(10000,shape=258,rate=11)
plot(density(theta))
# posterior predictive distribution for crashes in next year
y.star <- rep(NA,10000) # vector to hold simulations
# sample one observation from the posterior distribution
for (i in 1:10000){
  y.star[i] <- rpois(1,theta[i])
}
# plot histograms for data, posterior and posterior predictive on same scale
par(mfrow=c(3,1))
hist(crashes,col="gray",xlim=c(0,50),breaks=10)
hist(theta,col="gray",xlim=c(0,50))
hist(y.star,col="gray",xlim=c(0,50))
#posterior distribution vs. the posterio predictive distribution
mean(theta)
quantile(theta, probs=c(0.05,0.95))
mean(y.star)
quantile(y.star, probs=c(0.05,0.95))
sum(theta>30)/10000
sum(y.star>30)/10000
# from C. DiMaggio (Columbia University)




#par(new=TRUE)
#plot(lambda.range, log.likelihood, type="l", lty=2, lwd=3,
#     axes=FALSE, xlab="", ylab="")
#axis(side=4, cex.axis=1.3)

#x <- seq(from=5, to=35, length.out=100)              # Define the density domains
#ylim <- c(0, 0.15)
#plot(x, dgamma(x,shape=20,rate=1), main="Prior,posterior and likelihood",     # Plot a gamma density
#     type='l', ylim=ylim)







t=1
r=20
y.obs=y/t
likelihood<-function(r){
  prod(dpois(y.obs*t,lambda=r*t))
}
like=as.numeric(lapply(r,FUN=likelihood))
lines(r,like/sum(like),col="black",lwd=2)




a.prior=20
b.prior=1
prior=dgamma(r*t,shape=a.prior,scale=b.prior)

plot(r,prior,type="l",lwd=2,lty=3,ylab="Probability Density")



plot(x,prior/sum(prior),type="l",lwd=2,ylab="Probability Density")



a.post=(a.prior+sum(y.obs*t))
s.post=(s.prior/(8*s.prior+1))
post=dgamma(r*t,shape=a.post,scale=s.post)
a.post3=(a.prior3+sum(y.obs*t))
s.post3=(s.prior3/(s.prior3*8+1))
post3=dgamma(r*t,shape=a.post3,scale=s.post3)
quartz() OR windows()
par(mfrow=c(1,2))
plot(r,prior/sum(prior),type="l",ylim=c(0,0.015),lwd=2,ylab="Scaled Probability Density")
lines(r,like/sum(like),col="red",lwd=2)
lines(r,post/sum(post),col="blue",lwd=2)
############End of HW1 Problem2 #################
##################online examples#####################
#Defining some priors
t=100
r=seq(0,0.1,0.0001)
mu.prior=0.024*t
var.prior=0.01*t
a.prior=mu.prior^2/var.prior
s.prior=var.prior/mu.prior
prior=dgamma(r*t,shape=a.prior,scale=s.prior)
mu.prior=0.024*t
var.prior=0.1*t
a.prior2=mu.prior^2/var.prior
s.prior2=var.prior/mu.prior
prior2=dgamma(r*t,shape=a.prior2,scale=s.prior2)
mu.prior=0.024*t
mu.prior=0.024*t
var.prior=0.001*t
a.prior3=mu.prior^2/var.prior
s.prior3=var.prior/mu.prior
prior3=dgamma(r*t,shape=a.prior3,scale=s.prior3)
plot(r,prior3,type="l",lwd=2,lty=3,ylab="Probability Density")
lines(r,prior,lwd=2)
lines(r,prior2,lwd=2,lty=2)


y.obs=c(2,4,0,1,0,0,1,3)/t
likelihood<-function(r){
prod(dpois(y.obs*t,lambda=r*t))
}
like=as.numeric(lapply(r,FUN=likelihood))
plot(r,prior/sum(prior),type="l",ylim=c(0,0.015),lwd=2,ylab="Probability Density")
lines(r,like/sum(like),col="red",lwd=2)


a.post=(a.prior+sum(y.obs*t))
s.post=(s.prior/(8*s.prior+1))
post=dgamma(r*t,shape=a.post,scale=s.post)
a.post3=(a.prior3+sum(y.obs*t))
s.post3=(s.prior3/(s.prior3*8+1))
post3=dgamma(r*t,shape=a.post3,scale=s.post3)
quartz() OR windows()
par(mfrow=c(1,2))
plot(r,prior/sum(prior),type="l",ylim=c(0,0.015),lwd=2,ylab="Scaled Probability Density")
lines(r,like/sum(like),col="red",lwd=2)
lines(r,post/sum(post),col="blue",lwd=2)
plot(r,prior3/sum(prior3),type="l",ylim=c(0,0.015),lwd=2,ylab="Scaled Probability Density")
lines(r,like/sum(like),col="red",lwd=2)
lines(r,post3/sum(post3),col="blue",lwd=2)
post==max(post)
#The following R code is used to compute the posterior distribution:
  # Gelman, q. 13 page 69-70
  # Poisson with gamma prior
  deaths<- c(734,516,754,877,814,362,764,809,223,1066)
lambda.range<-seq(0,3000)
# prior
prior<-dgamma(lambda.range, shape=7, scale=1/.01)
# log likelihood (likelihood values are too large)
log.likelihood<-sum(deaths)*log(lambda.range)-length(deaths)*lambda.range
# posterior
alpha.star<-7+sum(deaths)
beta.star<- .01+length(deaths)
post<-dgamma(lambda.range, shape=alpha.star, scale=1/beta.star)
plot(lambda.range, prior,type="l",lwd=1.5, xlab=expression(lambda),
     ylab="Probability density",main="Triplot for fatal accident", cex.axis=1.3, cex.lab=1.3, bty="L", ylim=c(0,0.3))
lines(lambda.range, post)
par(new=TRUE)
plot(lambda.range, log.likelihood, type="l", lty=2, lwd=1.5,
     axes=FALSE, xlab="", ylab="")
axis(side=4, cex.axis=1.3)
 
  poisgamp(deaths,alpha.star,1/beta.star)
  ## simplest call with an observation of 4 and a gamma(1,1), i.e. an exponential prior on the
  ## mu
  poisgamp(4,1,1)
  
  ##  Same as the previous example but a gamma(10,1) prior
  poisgamp(4,10,1)
  
  ##  Same as the previous example but an improper gamma(1,0) prior
  poisgamp(4,1,0)
  
  ## A random sample of 50 observations from a Poisson distribution with
  ## parameter mu = 3 and  gamma(6,3) prior
  y = rpois(50,3)
  poisgamp(y,6,3)
  
  ## In this example we have a random sample from a Poisson distribution
  ## with an unknown mean. We will use a gamma(6,3) prior to obtain the
  ## posterior gamma distribution, and use the R function qgamma to get a
  ## 95% credible interval for mu
  y = c(3,4,4,3,3,4,2,3,1,7)
  results = poisgamp(y,6,3)
  ci = qgamma(c(0.025,0.975),results$r, results$v)
  cat(paste("95% credible interval for mu: [",round(ci[1],3), ",", round(ci[2],3)),"]\n")

  
  ## Not run:
  data(quine)
  posterior <- MCpoissongamma(quine$Days, 15, 1, 5000)
  summary(posterior)
  plot(posterior)
  grid <- seq(14,18,0.01)
  plot(grid, dgamma(grid, 15, 1), type="l", col="red", lwd=3, ylim=c(0,1.3),
       xlab="lambda", ylab="density")
  lines(density(posterior), col="blue", lwd=3)
  legend(17, 1.3, c("prior", "posterior"), lwd=3, col=c("red", "blue"))
  ## End(Not run)

set.seed(600)
x <- rgamma(500,shape=8,scale=0.1)
mean(x)
hist(x,prob=T,main='Gamma,scale=0.1')
lines(density(x),col='red',lwd=2)
lines( sort(x) , y = dgamma( sort(x) , shape = 8 , scale = 0.1 ) , col = "blue" , lty = 2 , lwd = 2 )
##Poisson log likelihood estimation of lambda.
library("asbio" )
library("tcltk2", lib.loc="~/R/win-library/3.1")
X<-c(1,3,4,0,2,3,4,3,5)
loglik.plot(X,dist="poi")


x <- seq(0,35, .1)
plot(x, dgamma(x, scale=1, shape=20), type="l", ylim=c(0,.5), ylab="y")
for(shape in 2:8){
  lines(x, dgamma(x, scale=2, shape=shape), col=shape)
}


lambda<-seq(0.00,5.00,0.01)
prior<-dgamma(lambda,a,b)
posterior<-dgamma(lambda,c,d)
plot(lambda,posterior,xlab=expression(lambda),ylab="Density",type="l")
lines(lambda,prior,lty=2)

#90% posterior credible interval for the birth rate Λ is [1.403, 2.285] births per
#hour,with p = 0.05 and 0.95
 qgamma(p,shape=46,scale=0.0396)  
# triplot for,ylab="Probability density"

#Poisson log-likelihood 
poisson.LL<-function(lambda) sum(log(dpois(y,lambda)))
poisson.LL(1)
poisson.LL(2)
poisson.LL(3)
poisson.LL(4)

#must use sapply to get correct values when given a vector
poisson.LL(2:3)
sapply(2:3,poisson.LL)
#plot log-likelihood
plot(seq(2,40,.01),sapply(seq(2,40,.01),poisson.LL),type='l',xlab=expression(lambda),ylab='log-likelihood')
#construct negative log-likelihood for use with mlm
poisson.negloglik<-function(lambda) -poisson.LL(lambda)
#obtain minimum negative log-likelihood
nlm(poisson.negloglik,4)->out.pois
out.pois
#compare to graph of negative log-likelihood
plot(seq(2,5,.01),sapply(seq(2,5,.01),function(x) -poisson.LL(x)),type='l',xlab=expression(lambda),ylab='likelihood')

#plot likelihood
poisson.L<-function(lambda) prod(dpois(aphid.data,lambda))
plot(seq(2,5,.01),sapply(seq(2,5,.01),poisson.L),type='l',xlab=expression(lambda),ylab='likelihood')
