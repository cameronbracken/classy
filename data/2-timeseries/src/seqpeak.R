seqpeak.r <- function(q,r=0.7*mean(q)){
	#   r is used as input so that for simulations that may have different mean
	#  the same release may be used.  r may be a vector to impose a release pattern
	# net change in each time period.  Also this takes care of vector wrap around
	rmq <- r-q   
	# if r and q are of different lengths
	n <- length(q)
	k <- rep(0,n)
	k[1] <- max(rmq[1],0)    #  first time k[0] is 0
	for (j in 2:n){
		k[j] <- max(k[j-1]+rmq[j],0)
	}
	s=max(k)
	return(list(s=s,k=k))
}
