#!/usr/bin/env Rscript

#load packages, functions and data
source('setup.R')

cat('Second Year Model using the', dataset, ' dataset\n\t',
    ns,'States\n\t',
    q, 'Years Back\n\t',
    r, 'Years Ahead\n\t',
    m, 'Year Running Mean\n')
flush.console()

paleo   <- switch(dataset, meko = meko,   woodhouse = woodhouse,   nino3 = cook)
paleo.b <- switch(dataset, meko = meko.b, woodhouse = woodhouse.b, nino3 = cook.b)

hist <-   switch(dataset, meko = lees,   woodhouse = lees,   nino3 = nino3)
hist.b <- switch(dataset, meko = lees.b, woodhouse = lees.b, nino3 = nino3.b)

n <- length(paleo)
nh <- length(hist)

	#all state tansitions of paleo
paleo.qr <- laglead(paleo,q,r)
paleo.bqr <- laglead(paleo.b,q,r)
hist.bqr <- laglead(hist.b,q,r)
cat('Building Historical State Transitions...\n')
flush.console()
tr.hist <- trprob( hist.bqr$lag, hist.bqr$lead )

	# block transition probabilities 
	# ns^q from states and 
	# ns^p to states
tr.file <- paste("data/tr_ns",ns,"_q",q,"_r",r,"_m",m,'_',dataset,".Rdata",sep="")
if(file.exists(tr.file) & cache){
  load(tr.file)
}else{
  cat('Building Paleo State Transitions...\n')
  flush.console()
  tr.paleo <- trprob( paleo.bqr$lag, paleo.bqr$lead )
  save(tr.paleo,file=tr.file)
}

    #all binary combinations of length q
b.i <- permutations(ns,q,0:(ns-1),repeats.allowed=T)

	#number of states
nfrom <- nrow(unique(tr.paleo$from))
nto <- nrow(unique(tr.paleo$to))

	#Quantiles of paleo and historical
Qp <- ecdf(paleo)(paleo)
Qp.qr <- laglead(Qp,q,r)
paleo.qr <- laglead(paleo,q,r)

	# All of the trasitions from a particular state
	# in the paleo and the associated states that 
	# came after, also the knn weight functions
  # for each beginning state, get the quantile pools to and from	
pools.from.Q <- build.pools.from(tr.paleo,Qp.qr)
pools.to.Q <- build.pools.to(tr.paleo,Qp.qr)
pools.from.paleo <- build.pools.from(tr.paleo,paleo.qr)
pools.to.paleo <- build.pools.to(tr.paleo,paleo.qr)
	

	# allocate matricies to store simualations
	# simulate each historical year except the ones 
	# where there is not q past years
	# We can always forecast one more year thatn the data we have
sim <- list()
for( s in 1:r ) sim[[s]] <- matrix(NA,nsims,nh-q+1)

no.analog <-logical(length((q+1):(nh+1)))

cat('Forecasting...\n');flush.console()

pb <- txtProgressBar(q+1,nh+1,style=3)
	#y is the year to forecast FROM
for(y in (q+1):(nh+1)){
  
  #setTxtProgressBar(pb,y)
	cat('Forecasting From Year',time(hist)[y-1],'for',time(hist)[y-1]+1:r,'\n')
	
		# The historical quantiles removing the current year
	this.hist <- hist[-y]
	Qh <- ecdf(this.hist)(hist)
	Qh.qr <- laglead(Qh,q,r)

		# current binary state
	state <- hist.b[(y-q):(y-1)]
	  # current state in terms of historical quantiles
	Q.state <- Qh[(y-q):(y-1)]
	  # current state in terms of paleo flow by applying 
	  # the *historical* quantiles
	paleo.flow.state <- quantile(paleo,Q.state)
	
		# all the transtition probabilities coming from the current state
	which.from <- rows.equal(b.i,state)
	this.p <- tr.paleo$p[which.from,]	
	
	cat('\tThe Current State is', state,'\n')
	cat('\tThe Quantile State is', sprintf('%4.2f',Q.state),'\n')
	#cat('\tThe Magnitude State is', sprintf('%6.3f',flow.state),'\n')
	
	this.sim <- matrix(NA,nsims,r)	
	
		# find which state are we coming from so we know which 
		# transition pool to choose from
	pool.from.Q <- pools.from.Q[[ which.from ]]
	pool.from.paleo <- pools.from.paleo[[ which.from ]]
	these.pool.to.Q <- pools.to.Q[[ which.from ]]
	these.pool.to.paleo <- pools.to.paleo[[ which.from ]]
    
    # check if there are no years to sample from 
	if(nrow(rbind(pool.from.paleo))==0){
	  cat('\tWarning: No analog year found!\n')
	  no.analog[y-q] <- TRUE
	}
	
	for(i in 1:nsims){
			
			# 1. Simulate a transition from the current state 
			# 2. Select the corresponding pool
			# 3. Knn from the pool 
			# 4. Repeat 
			
			# 1. Simulate a transition from the current state 
		rand1 <- runif(1) 
		which.to <- rank(c(rand1,cumsum(this.p)))[1]
		state.to <- tr.paleo$to[which.to,]
			
			# 2. corresponding pool
		this.pool.to.paleo <- rbind(these.pool.to.paleo[[which.to]])
		this.pool.to.Q <- these.pool.to.Q[[which.to]]
		those.to <- tr.paleo$pools$to[[ which.from ]][[which.to]]
		this.pool.from.paleo <- rbind(rbind(pool.from.paleo)[those.to,])
		this.pool.from.Q <- rbind(pool.from.Q)[those.to,]
		
		if(is.na(tr.paleo$cor[which.from,which.to]) & 
		  nrow(this.pool.from.paleo) > 1){
		  
		  #xx <- apply(this.pool.from.paleo,1,mean)
  		#yy <- apply(this.pool.to.paleo,1,mean)
  		xx <- this.pool.from.paleo[,q]
  		yy <- this.pool.to.paleo[,2]
  		co <- cor(xx,yy)

      if(nrow(this.pool.from.paleo) > 1)
  		fit <- summary(lm(this.pool.to.paleo[,1]~this.pool.from.paleo))
  		pval <- pf(fit$fstatistic[1L], fit$fstatistic[2L], fit$fstatistic[3L], 
  		  lower.tail = FALSE)
		  
		  tr.paleo$pts[which.from,which.to] <- length(this.pool.to.paleo[,1])
		  tr.paleo$cor[which.from,which.to] <- co
		  tr.paleo$pval[which.from,which.to] <- pval
		  tr.paleo$sig[which.from,which.to] <- 
		    ifelse(abs(co) > sigcor(tr.paleo$pts[which.from,which.to]),T,F) 
		}
		#cat('From:',state,'  To: ',state.to,'  ',co,' n = ',length(xx),'\n')
		
			# Weight function and nn for the current state 
  		# weight function dependent on number of points
  	k <- floor(sqrt(nrow(this.pool.from.paleo)))
  	w <- cumsum(oneoverk(k))
  	
			# 3. Knn 
		
		if(nrow(this.pool.from.paleo) == 0 ){
		    
		    # then there is no analog year in the paleo record :(
		  this.sim[i,] <- NA
		  
		}else{
		 
		  	# find nearest quantile neighbors 
  			# (rbind to make sure that it is a row vector)
  		neighbors <- find.matches(rbind(paleo.flow.state),
  		    this.pool.from.paleo, 
  		    tol = rep(max(this.pool.from.paleo),q),
  		    maxmatch = length(paleo))$matches[1:k]
  		#d <- sqrt(apply((this.pool.from.paleo - paleo.flow.state)^2,1,sum))
  		#d <- sqrt(apply((rbind(this.pool.from.Q) - Q.state)^2,1,sum))
  		#neighbors <- order(d)[1:k]
  		#nn <- 1/sort(neighbors)

  		# a less drastic weight function
  	  #	w <- cumsum(nn/sum(nn))

  		rand2 <- runif(1)
  		this.neighbor <- rank(c(rand2,w))[1]


  		#browser()
  		  # need cbind to make sure we're using matricies, could use two cases, 
  		  # but this is less lines of code.
  		this.quant <- cbind(cbind(this.pool.to.Q)[neighbors,])[this.neighbor,]
  		#this.quant <- this.pool.to[sample(1:nrow(this.pool.to),1),]

  		this.sim[i,] <- this.quant 
		  
		}
		#cat("Chose",state.to, 'and', quantile(hist,this.quant),'from',state,'and',
		# quantile(hist,Q.state),'real',hist[y:(y+r-1)],'\n')
	}
	#browser()
	
	for(s in 1:r)
	  sim[[s]][,y-q] <- 
	    if(all(is.na(this.sim[,s]))) rep(NA,nsims) else quantile(hist,this.sim[,s])
	  
}
close(pb)

hist.plot <- list()
layout(cbind(1:r))
for( s in 1:r ){
  
  colnames(sim[[s]])  <- (start(hist)[1]+q-1):(end(hist)[1]) + s
  hist.plot[[s]] <- window(hist,start(hist)[1] + q + s -1)
  boxplot(sim[[s]],outline=F) #[,2:ncol(sim[[s]])]
  lines(1:length(hist.plot[[s]]),hist.plot[[s]],col='red',lwd=2)
  
}
rpss <- RPSS(hist.plot[[s]],sim[[1]][,-ncol(sim[[1]])])
#cat('Median RPSS:',median(rpss[rpss>-3.5]),'\n')
cat(median(rpss[!no.analog[-length(no.analog)]],na.rm=T),'\n')

save(sim, hist.plot ,file='data/fc.Rdata')