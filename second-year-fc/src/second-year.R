#!/usr/bin/env Rscript

#load packages, functions and data
source('setup.R')

ns <- 2 # states
q <- 2 #years back
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
	# ns^q from states and 
	# ns^p to states
tr.paleo <- trprob(paleo.bqr$lag, paleo.bqr$lead )

    #all binary combinations of length r
b <- binary.combos( q )
b.i <- unique( tr.paleo$from )

	#number of states
nfrom <- nrow(unique(tr.paleo$from))
nto <- nrow(unique(tr.paleo$to))

	#Quantiles of paleo and historical
Qp <- ecdf(paleo)(paleo)
Qp.qr <- laglead(Qp,q,r)

	# All of the trasitions from a particular state
	# in the paleo and the associated states that 
	# came after, also the knn weight functions
pools.from <- pools.to <- W <-  vector('list',nfrom)
for(i in 1:nfrom){
	
		#for each beginning state, get the quantile pools to and from
	pools.from[[i]] <- Qp.qr$lag[ tr.paleo$pools$from[[i]], ]
	for(j in 1:nto){
		ito <- (i-1)*nto+j
		pools.to[[ito]] <- Qp.qr$lead[ tr.paleo$pools$to[[ito]], ]
	}
		
	k <- round(sqrt(nrow(pools.to[[i]])))
	
		#weight function dependent on number of points
	w <- numeric(k)
	for(j in 1:k)
		w[j] <- (1/j)/(sum(1/(1:j)))
	w <- cumsum(w/sum(w))
	W[[i]] <- w
}

	# allocate matricies to store simualations
	# simualte each historical year except the ones 
	# where there is not q past years
sim.y1 <- sim.y2 <- matrix(NA,nsims,nh-q+1)

	#y is the year to forecast FROM
for(y in q:nh){
	cat('Forecasting From Year',time(hist)[y],'\n')
	
		# The historical quantiles removing the current year
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
		
		# find which state are we coming from so we know which 
		# transition pool to choose from
	which.from <- rows.equal(b.i,state)
	pool.from <- pools.from[[ which.from ]]
	these.pool.to <- pools.to[ which.from.p ]	
	
		# current and pool's quantile sum 
		# (used to determine nearness of states) 
	pool.Qsum <- apply(pool.from,2,sum)	
		
		# Weight function and nn for the current state 
	w <- W[[ which.from ]]
	k <- length(w)
	
	for(i in 1:nsims){
			
			# 1. Simulate a transition from the current state 
			# 2. Select the corresponding pool
			# 3. Knn from the pool 
			# 4. Repeat 
			
			# 1. Simulate a transition from the current state 
		rand <- runif(1) 
		which.to <- rank(c(rand,cumsum(this.p)))[1]
		state.to <- tr.paleo$to[which.to,]
			
			# 2. corresponding pool
		this.pool.to <- these.pool.to[[which.to]]
		those.to <- tr.paleo$pools$to[ which.from.p ][[which.to]]
		this.pool.from <- pool.from[those.to,]
		
			# 3. Knn 
		rand <- runif(1)
		this.neighbor <- rank(c(rand,w))[1]
		
			#find nearest quantile neighbors
		d <- sqrt(apply((this.pool.from - Qstate)^2,1,sum))
		neighbors <- order(d)[1:k]
		
		#browser()
		this.quant <- this.pool.to[neighbors,][this.neighbor,]
		#this.quant <- this.pool.to[sample(1:nrow(this.pool.to),1),]
	
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