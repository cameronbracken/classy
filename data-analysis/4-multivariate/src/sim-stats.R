sim.stats <- 
function(sim,nsims,nyears=nrow(sim[,,1]),start=c(1906,1)){

	sim.ann <- apply(sim,c(1,3),mean)
	stats <- list()
	stats$mean <- stats$sd <- 
	stats$skew <- stats$lag1 <- matrix(NA,nsims,13)
	
	# seasonal simulated stats
	for(i in 1:12){
		this.mon <- sim[,i,]
		stats$mean[,i] <- apply(this.mon,2,mean)
		stats$sd[,i]   <- apply(this.mon,2,sd)
		stats$skew[,i] <- apply(this.mon,2,skew)

	}
	#lag 1 correlation
	for(i in 1:nsims){
		this.sim <- sim[,,i]
		this.sim.ts <- ts(array(t(this.sim)),start=start,frequency=12)
		stats$lag1[i,1:12] <- peacf(this.sim.ts,plot=FALSE,lag.max=1)$acf
	}		

	# aggregate seasonal statistics
	stats$mean[,13] <- apply(sim.ann,2,mean)
	stats$sd[,13] <-   apply(sim.ann,2,sd)
	stats$skew[,13] <- apply(sim.ann,2,skew)
	stats$lag1[,13] <- apply(sim.ann,2,mylag,lag=1,docor=T)

	#setup for plotting
	stats <- lapply(stats,as.data.frame)
	mon <- c('Jan','Feb','Mar','Apr','May','Jun',
		'Jul','Aug','Sep','Oct','Nov','Dec','Ann')
	for(i in 1:length(stats)) names(stats[[i]]) <- mon
	
	return(stats)
}

