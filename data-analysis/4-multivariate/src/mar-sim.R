mar_sim <- function(Z,A,B,nsims = 250, ny = length(Z[,1,1])){
	
	#Now Simulate
	sims <- array(NA,dim=c(dim(Z),nsims))

	for(i in 1:nsims){

		this.sim <- array(NA,dim(Z))

		## Start with a random Dec value
		Z.last = cbind( Z[sample(1:ny,1), 12, ] )
		#browser()
		
		for(j in 1:ny)
			for(month in 1:12){
				e <- cbind(rnorm(2))
				Zt <- A[[month]] %*% Z.last + B[[month]] %*% e
				this.sim[j,month,] <- Zt
				Z.last <- Zt
			}
		sims[,,,i] <- this.sim
	}
	return(sims)
}
