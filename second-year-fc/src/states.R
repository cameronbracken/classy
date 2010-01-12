#!/usr/bin/env Rscript

#load packages, functions and data
source('setup.R')

q <- 2 #years back
r <- 2 #years ahead, 2 or greater
nsims <- 250

#Paramters to implement
mode <- 'drop-one' # or 'retroactive'
hist.cdf <- 'ecdf' # or 'gamma'

paleo <- wood
paleo.b <- wood.b
hist <- lees
hist.b <- lees.b

n <- length(paleo)
nh <- length(hist)

    #all binary combinations of length r
b <- as.matrix( expand.grid( rep( list(c(T,F)), r ) ) )

	#all state tansitions of paleo
paleo.bqr <- laglead(paleo.b,q,r)
	
	#block transition probabilities
tr.paleo <- trprob(paleo.bqr$lag,paleo.bqr$lead)

	#number of states
nfrom <- nrow(unique(tr.paleo$from))
nto <- nrow(unique(tr.paleo$to))

	#Quantiles of paleo and historical
Qp <- ecdf(paleo)(paleo)
Qp.qr <- laglead(Qp,q,r)

sim.y1 <- sim.y2 <- matrix(NA,nsims,nh-q+1)

pools.from <- pools.to <- list()
for(i in 1:length(tr.paleo$p))
	pools.to[[i]] <- pools.

	#y is the year to forecast FROM
for(y in q:nh){
	cat('Forecasting From Year',time(hist)[y],'\n')
	Qh <- ecdf(hist[-((y-q+1):y)])(hist)
	Qh.qr <- laglead(Qh,q,r)

		#current binary and quantile state
	state <- hist.b[(y-q+1):y]
	Qstate <- Qh[(y-q+1):y]
	Qsum <- sum(Qstate)
		#all the probabilities of coming from the current state
	this.p <- tr.paleo$p[rows.equal(tr.paleo$from,state)]	
	
	cat('\tThe Current State is', state,'\n')
	cat('\tThe Quantile State is', Qstate,'\n')
	
	this.sim <- matrix(NA,nsims,r)	
		
	for(i in 1:nsims){
	
			#simulate transition
		rand <- runif(1)
		which.to <- rank(c(rand,cumsum(this.p)))[1]
		state.to <- tr.paleo$to[which.to,]
	
			#conditional pool, quantiles given state.to
		pool.from <- Qp.qr$lag[rows.equal(paleo.bqr$lead, state.to),]
		pool.to <- Qp.qr$lead[rows.equal(paleo.bqr$lead, state.to),]

			#current and pool's quantile sum 
		pool.Qsum <- apply(pool.from,1,sum)

			#number of neighbors
		k <- round(sqrt(nrow(pool.to)))

			#weight function
		w <- numeric(k)
		for(j in 1:k)
			w[j] <- (1/j)/(sum(1/(1:j)))
		w <- cumsum(w/sum(w))	

			#find nearest quantile neighbors
		d <- sqrt(apply((pool.from - Qstate)^2,1,sum))
		neighbors <- order(d)[2:(k+1)]
	
		rand <- runif(1)
		this.neighbor <- rank(c(rand,w))[1]
		this.quant <- pool.to[neighbors,][this.neighbor,]
		#this.quant <- pool.to[sample(1:nrow(pool.to),1),]
	
		this.sim[i,] <- this.quant
	
	}
	
	sim.y1[,y-q+1] <- quantile(hist,this.sim[,1])
	sim.y2[,y-q+1] <- quantile(hist,this.sim[,2])
}
colnames(sim.y1) <- (start(hist)[1]+q-1):(end(hist)[1])+1
colnames(sim.y2) <- (start(hist)[1]+q-1):(end(hist)[1])+2
hist.y1 <- window(hist,start(hist)[1]+q)
hist.y2 <- window(hist,start(hist)[1]+q+1)

 layout(rbind(1,2))
 boxplot(sim.y1,outline=F)
 lines(1:length(hist.y1),hist.y1,col='red',lwd=2)
 boxplot(sim.y2,outline=F)
 lines(1:length(hist.y2),hist.y2,col='red',lwd=2)

save(sim.y1,sim.y2, hist.y1, hist.y2 ,file='data/fc.Rdata')