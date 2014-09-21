 ##################################################
# Task: Bios 7717: Bayesian Biostatistics, HW 1 ##
# Author: Li Zhang                              ##
# Date created: 9/16/2014                       ##
# Date modified:                                ##
##################################################
library (splines)
library (stats4)
library (VGAM)
library (binom)
 library("LearnBayes")
## Problem 1, Lyme test, specificity
# data
y <- 19
n <- 25
# 1.a, compute an exact (frequentist) 95% condence interval for (the so-called
# Clopper-Pearson interval)
binom.test(19,25,.5)
# or
binom.cloglog(19,25,conf.level=0.95,methods=exact)
# large sample 
# large sample confience interval;
phat <- y/n
UL <- 0.76+1.96*sqrt(phat*(1-phat)/n)
LL <- 0.76-1.96*sqrt(phat*(1-phat)/n)
LL
UL

#1.b A popular alternative frequentist
#method was proposed by Agresti and Coull (1998), who suggested that the large
#sample interval be computed after augmenting the data with 2 \successes" and 2
#\failures". Compute their interval and compare it to the two intervals computed in
#a. The binom package computes a slight variation on Agresti-Coull.

#binom.cloglog(19,25,conf.level=0.95,methods=agresti-coull)
#binom.confint(y, n, conf.level = 0.95, methods = agresti-coull)
binom.confint(y, n, conf.level = 0.95)
 
 curve(dbeta(x,2,2)) # plot the prior
# plot the likelihood
 calcLikelihoodForProportion(y, n)
 # triplot
 calcPosteriorForProportion <- function(successes, total, a, b)
 {
   # Adapted from triplot() in the LearnBayes package
   # Plot the prior, likelihood and posterior:
   likelihood_a = successes + 1; likelihood_b = total - successes + 1
   posterior_a = a + successes;  posterior_b = b + total - successes
   theta = seq(0.005, 0.995, length = 500)
   prior = dbeta(theta, a, b)
   likelihood = dbeta(theta, likelihood_a, likelihood_b)
   posterior  = dbeta(theta, posterior_a, posterior_b)
   m = max(c(prior, likelihood, posterior))
   plot(theta, posterior, type = "l", ylab = "Density", lty = 2, lwd = 3,
        main = paste("beta(", a, ",", b, ") prior, B(", total, ",", successes, ") data,",
                     "beta(", posterior_a, ",", posterior_b, ") posterior"), ylim = c(0, m), col = "red")
   lines(theta, likelihood, lty = 1, lwd = 3, col = "blue")
   lines(theta, prior, lty = 3, lwd = 3, col = "green")
   legend(x=0.8,y=m, c("Prior", "Likelihood", "Posterior"), lty = c(3, 1, 2),
          lwd = c(3, 3, 3), col = c("green", "blue", "red"))
   # Print out summary statistics for the prior, likelihood and posterior:
   calcBetaMode <- function(aa, bb) { BetaMode <- (aa - 1)/(aa + bb - 2); return(BetaMode); }
   calcBetaMean <- function(aa, bb) { BetaMean <- (aa)/(aa + bb); return(BetaMean); }
   calcBetaSd   <- function(aa, bb) { BetaSd <- sqrt((aa * bb)/(((aa + bb)^2) * (aa + bb + 1))); return(BetaSd); }
   prior_mode      <- calcBetaMode(a, b)
   likelihood_mode <- calcBetaMode(likelihood_a, likelihood_b)
   posterior_mode  <- calcBetaMode(posterior_a, posterior_b)
   prior_mean      <- calcBetaMean(a, b)
   likelihood_mean <- calcBetaMean(likelihood_a, likelihood_b)
   posterior_mean  <- calcBetaMean(posterior_a, posterior_b)
   prior_sd        <- calcBetaSd(a, b)
   likelihood_sd   <- calcBetaSd(likelihood_a, likelihood_b)
   posterior_sd    <- calcBetaSd(posterior_a, posterior_b)
   print(paste("mode for prior=",prior_mode,", for likelihood=",likelihood_mode,", for posterior=",posterior_mode))
   print(paste("mean for prior=",prior_mean,", for likelihood=",likelihood_mean,", for posterior=",posterior_mean))
   print(paste("sd for prior=",prior_sd,", for likelihood=",likelihood_sd,", for posterior=",posterior_sd))
 }
 a = 2
 b=2
 calcPosteriorForProportion(y,n,a,b)
 
 ### Prior beta(2,8)
 calcPosteriorForProportion <- function(successes, total, a, b)
 {
   # Adapted from triplot() in the LearnBayes package
   # Plot the prior, likelihood and posterior:
   likelihood_a = successes + 1; likelihood_b = total - successes + 1
   posterior_a = a + successes;  posterior_b = b + total - successes
   theta = seq(0.005, 0.995, length = 500)
   prior = dbeta(theta, a, b)
   likelihood = dbeta(theta, likelihood_a, likelihood_b)
   posterior  = dbeta(theta, posterior_a, posterior_b)
   m = max(c(prior, likelihood, posterior))
   plot(theta, posterior, type = "l", ylab = "Density", lty = 2, lwd = 3,
        main = paste("beta(", a, ",", b, ") prior, B(", total, ",", successes, ") data,",
                     "beta(", posterior_a, ",", posterior_b, ") posterior"), ylim = c(0, m), col = "red")
   lines(theta, likelihood, lty = 1, lwd = 3, col = "blue")
   lines(theta, prior, lty = 3, lwd = 3, col = "green")
   
   # Print out summary statistics for the prior, likelihood and posterior:
   calcBetaMode <- function(aa, bb) { BetaMode <- (aa - 1)/(aa + bb - 2); return(BetaMode); }
   calcBetaMean <- function(aa, bb) { BetaMean <- (aa)/(aa + bb); return(BetaMean); }
   calcBetaSd   <- function(aa, bb) { BetaSd <- sqrt((aa * bb)/(((aa + bb)^2) * (aa + bb + 1))); return(BetaSd); }
   prior_mode      <- calcBetaMode(a, b)
   likelihood_mode <- calcBetaMode(likelihood_a, likelihood_b)
   posterior_mode  <- calcBetaMode(posterior_a, posterior_b)
   prior_mean      <- calcBetaMean(a, b)
   likelihood_mean <- calcBetaMean(likelihood_a, likelihood_b)
   posterior_mean  <- calcBetaMean(posterior_a, posterior_b)
   prior_sd        <- calcBetaSd(a, b)
   likelihood_sd   <- calcBetaSd(likelihood_a, likelihood_b)
   posterior_sd    <- calcBetaSd(posterior_a, posterior_b)
   print(paste("mode for prior=",prior_mode,", for likelihood=",likelihood_mode,", for posterior=",posterior_mode))
   print(paste("mean for prior=",prior_mean,", for likelihood=",likelihood_mean,", for posterior=",posterior_mean))
   print(paste("sd for prior=",prior_sd,", for likelihood=",likelihood_sd,", for posterior=",posterior_sd))
 }
 
 a = 2
 b=8
 calcPosteriorForProportion(y,n,a,b)
 legend(x=0.2,y=m, c("Prior", "Likelihood", "Posterior"), lty = c(3, 1, 2),
        lwd = c(3, 3, 3), col = c("green", "blue", "red"))
 
 ### Prior beta(8,2)
 a = 8
 b = 2
 calcPosteriorForProportion(y,n,a,b)
 legend(x=0.1,y=m, c("Prior", "Likelihood", "Posterior"), lty = c(3, 1, 2),
        lwd = c(3, 3, 3), col = c("green", "blue", "red"))
 ### Prior beta(11,11)
 a = 11
 b = 11
 calcPosteriorForProportion(y,n,a,b)
 legend(x=0.2,y=m, c("Prior", "Likelihood", "Posterior"), lty = c(3, 1, 2),
        lwd = c(3, 3, 3), col = c("green", "blue", "red"))
 ##########################################################
 ## Take a look at the prior, likelihood, and posterior
 ##########################################################
 #priorToPosterior = function(y, n, a, b)  {
   ## Note the rule of succession
   likelihood.a = y + 1
   likelihood.b = n - y + 1
a = 2
b=2
   ## Create posterior
   posterior.a = a + y;
   posterior.b = b + n - y
   theta = seq(0.005, 0.995, length = 500)
   
   ## Calc density
   prior = dbeta(theta, a, b)
   likelihood = dbeta(theta, likelihood.a, likelihood.b)
   posterior = dbeta(theta, posterior.a, posterior.b)
   
   ## Plot prior, likelihood, and posterior

 ## Can be used to scale down the graph if desired.
   ## However, the density is different for each prior, likelihood, posterior
   m.orig = apply( cbind(prior, likelihood, posterior), 2, max)
   m = max(c(prior, likelihood, posterior))
   
   plot(theta, posterior, type = "l", ylab = "Density", lty = 2, lwd = 3,
        main = paste("Prior: beta(", round(a,2), ",", round(b,2), "); Data: B(", n, ",", y, "); ",
                     "Posterior: beta(", round(posterior.a,2), ",", round(posterior.b,2), ")", sep=""), ylim = c(0, m), col = 1)
   lines(theta, likelihood, lty = 1, lwd = 3, col = 2)
   lines(theta, prior, lty = 3, lwd = 3, col = 3)
   legend("topleft",y=m, c("Prior", "Likelihood", "Posterior"), lty = c(3, 1, 2),
          lwd = c(3, 3, 3), col = c(3, 2, 1))
   
   #####################################
 prior.mode = calcBetaMode(a, b)
   likelihood.mode = calcBetaMode(likelihood.a, likelihood.b)
   posterior.mode = calcBetaMode(posterior.a, posterior.b)
   prior.mean = calcBetaMean(a, b)
   likelihood.mean = calcBetaMean(likelihood.a, likelihood.b)
   posterior.mean = calcBetaMean(posterior.a, posterior.b)
   prior.med = calcBetaMedian(a, b)
   likelihood.med = calcBetaMedian(likelihood.a, likelihood.b)
   posterior.med = calcBetaMedian(posterior.a, posterior.b)
   prior.var = calcBetaVar(a, b)
   likelihood.var = calcBetaVar(likelihood.a, likelihood.b)
   posterior.var = calcBetaVar(posterior.a, posterior.b)
   prior.skew = calcBetaSkew(a, b)
   likelihood.skew = calcBetaSkew(likelihood.a, likelihood.b)
   posterior.skew = calcBetaSkew(posterior.a, posterior.b)
   
   print(paste("Mode: prior=",prior.mode,"; Likelihood=",likelihood.mode,"; Posterior=",posterior.mode))
   print(paste("Mean: prior=",prior.mean,"; Likelihood=",likelihood.mean,"; Posterior=",posterior.mean))
   print(paste("~Approx Median (for a and b > 1): prior=",prior.med,"; Likelihood=",likelihood.med,", for Posterior=",posterior.med))
   print(paste("Var: prior=",prior.var,"; Likelihood=", likelihood.var,"; Posterior=",posterior.var))
   print(paste("Skewness: prior=",prior.skew,"; Likelihood=",likelihood.skew,"; Posterior=",posterior.skew))
   return(list(a=posterior.a,b=posterior.b))
 }
 
 posterior.out = priorToPosterior(y,n, prior.dist$a, prior.dist$b) # 25/50 is current data
 beta.sim = rbeta(1000000,posterior.out$a, posterior.out$b)
 abline(v=quantile(beta.sim, prob=c(.05/2, 1-.05/2)), col='#000000', lwd=2)
 abline(v=quantile(beta.sim, prob=c(.01/2, 1-.01/2)), col='#EEEEEE', lwd=2)
 
####plot B together
 colors = c("red","blue","green","orange","purple")
 
 n = 10
 N = 10
 theta = .2
 
 x = rbinom(n,N,theta)
 grid = seq(0,2,.01)
 
 
 alpha = c(.5,5,1,2,2)
 beta = c(.5,1,3,2,5)
 
 plot(grid,grid,type="n",xlim=c(0,1),ylim=c(0,4),xlab="",ylab="Prior Density",
      main="Prior Distributions", las=1)
 for(i in 1:length(alpha)){
   prior = dbeta(grid,alpha[i],beta[i])
   lines(grid,prior,col=colors[i],lwd=2)
 }
 
 legend("topleft", legend=c("Beta(0.5,0.5)", "Beta(5,1)", "Beta(1,3)", "Beta(2,2)", "Beta(2,5)"),
        lwd=rep(2,5), col=colors, bty="n", ncol=3)
 
 for(i in 1:length(alpha)){
   dev.new()
   plot(grid,grid,,type="n",xlim=c(0,1),ylim=c(0,10),xlab="",ylab="Density",xaxs="i",yaxs="i",
        main="Prior and Posterior Distribution")
   
   alpha.star = alpha[i] + sum(x)
   beta.star = beta[i] + n*N - sum(x)
   prior = dbeta(grid,alpha[i],beta[i])
   post = dbeta(grid,alpha.star,beta.star)
   
   lines(grid,post,lwd=2)
   lines(grid,prior,col=colors[i],lwd=2)
   legend("topright",c("Prior","Posterior"),col=c(colors[i],"black"),lwd=2)
   
 }

