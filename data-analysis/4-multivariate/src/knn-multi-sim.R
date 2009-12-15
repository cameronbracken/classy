knn.multi.sim <- function(Z, ny = dim(Z)[1]){
	
	sims <- array(0,dim=c(dim(Z),nsims))

	K  <- sqrt(ny)
	W <- 1/(1:K)
	W <- cumsum( W/sum(W) )

	for(i in 1:nsims){

		this.sim <- array(0,c(ny*12,2))
			## Start with a previous Dec value..
		Z.last <- rbind( Z[sample(1:ny,1), 12, ] )

		for(j in 1:(ny*12)){
			
			month = j %% 12    ## the month we are simulating
			if(month == 0) month = 12
			
				if(month == 1) {
					neighbors <- rbind(Z[1:(ny-1),12,])
					neighbors <- rbind(Z.last,neighbors) 
					N <- ny - 1
				}else{
					neighbors <- rbind(Z[,month-1,]) 
					neighbors <- rbind(Z.last,neighbors)
					N <- ny
				}
				ordered.distance <- 
					order(as.matrix(dist(neighbors))[1,2:(N+1)])

				rand <- runif(1)
				nearest <- rank(c(rand,W))[1]
				this.neighbor <- ordered.distance[nearest]
				if(month == 1)this.neighbor <- this.neighbor + 1

				Zt = t(Z[this.neighbor,month,])
				this.sim[j,] = Zt
					## set the previous value and continue.
				Z.last = Zt
		}
		# Put back in sim array
		for(s in 1:dim(Z)[3])
			sims[,,s,i] <- matrix(this.sim[,s],ncol=12,byrow=T)
	}
	return(sims)
}
