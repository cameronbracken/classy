sim.knn <- function(dat, nsim, l = length(dat), plot=F, quiet=F){
	
	if(!quiet) cat('Simulating...')
	years <- time(dat)

		# number of years in observed data, 
		# which will also be length of each sequence
	nyear <- length(years)
	
		# data to be used to calculate distances
		# dont use the last year since we dont know
		# the future state in that year
	dat.pool <- window(dat,start=start(dat),end=end(dat)[1]-1)
	years.sim <- time(dat.pool)

		#number of neighbors
	k <- as.integer(ceiling(sqrt(nyear)))
	W <- numeric(k)
	
		#fills weighting matrix
	for(j in 1:k)
		W[j] <- 1/j

		#divides weights by sum of weights so cumulative probability = 1
	W <- W/sum(W)

		#defines matrix to hold flow sequences
	sims <- matrix(nrow=l, ncol=nsim) 

	for(n in 1:nsim){

			# randomly select one of the observed years
			# as an initial condition
		this.year <- sample(years, 1, replace = TRUE) 
		
			#Fills sequence matrix with each iteration (provides year) 
		sims[1,n] <- dat[years == this.year] 

		for(i in 2:l){
	 
				#Flow corresponding to the "current" year
			this.flow <- dat[years == this.year] 
				# calculates difference between selected flow and other 
				#  observed flows
			dist <- abs(dat.pool-this.flow) 
				#select k nearet neighbors
			neighbors <- order(dist)[1:k] 
		
				#Selects a year to be "nearest neighbor"
			NN <- sample(years.sim[neighbors], 1, replace = TRUE, prob=W) 
			this.year <-  NN + 1
				#Fills sequence matrix with each iteration (provides year) 
			sims[i,n] <- dat[years == this.year]
	
		}

	}
	if(plot){
		
		m <- 0
		for(i in 1:nsim){
			x <- density(sims[,i])
			m <- ifelse(max(x$y)>m,max(x$y),m)
		}
		plot(z <- density(dat),ylim=c(0,m))
		for(i in 1:nsim){
			x <- density(sims[,i])
			lines(x$x,x$y,col='grey')
		}
		lines(z)
		
	}
	if(!quiet) cat('Done.\n')
	return(sims)
}