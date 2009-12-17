annual.stats <- function(x.sim){
	x.sim.stats <- matrix(NA,nrow=nrow(x.sim),ncol=4)
	x.sim.stats[,1] <- apply(x.sim,1,mean)
	x.sim.stats[,2] <- apply(x.sim,1,sd)
	x.sim.stats[,3] <- apply(x.sim,1,skew)
	x.sim.stats[,4] <- apply(x.sim,1,mylag,1,docor=T)
	return(x.sim.stats)
}
