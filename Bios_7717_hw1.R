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
