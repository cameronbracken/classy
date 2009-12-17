#!/usr/bin/env Rscript

#load packages, functions and data
source('setup.R')

q <- 2 #years back
r <- 2 #years ahead, 2 or greater

nsims <- 250
paleo <- meko
paleo.b <- meko.b
hist <- lees
hist.b <- lees.b

n <- length(paleo)
nh <- length(hist)

    #binary combinations
b <- > expand.grid( rep( list(c(T,F)), r ) )

hist.bqr <- lagnext(hist.b,q,r)
hist.qr <- lagnext(hist,q,r)
paleo.bqr <- lagnext(paleo.b,q,r)

sim <- matrix(NA,nh-q,nsims)

	
for(i in (m+1):nh){
	
	state <- paleo.bqr$x[i,]
	
	is.in <- !logical(nh-q-1)
	for(j in 1:q)
		is.in <- is.in & (hist.bqr$x[,j] == state[j])
	
	hist.prev <- hist.qr$x[is.in,]
	hist.next <- hist.qr$y[is.in,]
	
	K  <- nrow(hist.next)
	W <- 1/(1:K)
	W <-W/sum(W)
	#browser()
	
	#for(ns in 1:nsims){
		
		
		
	#}
	
	
}