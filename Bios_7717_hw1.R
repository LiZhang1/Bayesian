<<<<<<< HEAD
##################################################
=======
 ##################################################
>>>>>>> 6c525ead0ecc02913aabb60f048ca09af5b69622
# Task: Bios 7717: Bayesian Biostatistics, HW 1 ##
# Author: Li Zhang                              ##
# Date created: 9/16/2014                       ##
# Date modified:                                ##
##################################################
library (splines)
library (stats4)
library (VGAM)
library (binom)
<<<<<<< HEAD
=======
 library("LearnBayes")
>>>>>>> 6c525ead0ecc02913aabb60f048ca09af5b69622
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
<<<<<<< HEAD
=======
 
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
 
