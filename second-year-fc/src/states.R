#!/usr/bin/env Rscript

#load packages, functions and data
source('setup.R')

q <- 3 #years back
r <- 2 #years ahead, 2 or greater
nsims <- 250

#Paramters to implement
mode <- 'drop-one' # or 'retroactive'
hist.cdf <- 'ecdf' # or 'gamma'
conditional.pools <- FALSE

paleo <- meko
paleo.b <- meko.b
hist <- lees
hist.b <- lees.b

n <- length(paleo)
nh <- length(hist)

	#all state tansitions of paleo
paleo.bqr <- laglead(paleo.b,q,r)
	
	#block transition probabilities
tr.paleo <- trprob(paleo.bqr$lag, paleo.bqr$lead )

    #all binary combinations of length r
b <-binary.combos( q )
b.i <- unique( tr.paleo$from )

	#number of states
nfrom <- nrow(unique(tr.paleo$from))
nto <- nrow(unique(tr.paleo$to))

	#Quantiles of paleo and historical
Qp <- ecdf(paleo)(paleo)
Qp.qr <- laglead(Qp,q,r)

pools.from <- pools.to <- W <-  vector('list',nfrom)
for(i in 1:nfrom){
	
	pools.from[[i]] <- Qp.qr$lag[ tr.paleo$pools$from[[i]], ]
	pools.to[[i]] <- Qp.qr$lead[ tr.paleo$pools$from[[i]], ]
		
	k <- round(sqrt(nrow(pools.to[[i]])))
	
		#weight function dependent on number of points
	w <- numeric(k)
	for(j in 1:k)
		w[j] <- (1/j)/(sum(1/(1:j)))
	w <- cumsum(w/sum(w))
	W[[i]] <- w
}

sim.y1 <- sim.y2 <- matrix(NA,nsims,nh-q+1)

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
	which.from.p <- rows.equal(tr.paleo$from,state)
	this.p <- tr.paleo$p[which.from.p]	
	
	cat('\tThe Current State is', state,'\n')
	cat('\tThe Quantile State is', Qstate,'\n')
	
	this.sim <- matrix(NA,nsims,r)	
	
	which.from <- rows.equal(b.i,state)
	pool.from <- pools.from[[ which.from ]]
	pool.to <- pools.to[[ which.from ]]
	
		#current and pool's quantile sum 
	pool.Qsum <- apply(pool.from,2,sum)	
	w <- W[[which.from]]
	k <- length(w)
	
		#find nearest quantile neighbors
	d <- sqrt(apply((pool.from - Qstate)^2,1,sum))
	neighbors <- order(d)[1:k]
	
	for(i in 1:nsims){
	
		rand <- runif(1)
		this.neighbor <- rank(c(rand,w))[1]
		this.pool.to <- matrix(pool.to[neighbors,],,r)
		this.quant <- this.pool.to[this.neighbor,]
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