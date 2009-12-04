select.neighbor <- function(prev, x, n, k){
	
	#w <- numeric(k)
	#for(i in 1:k)
	#	w[i] <- (1/i)/(sum(1/(1:i)))
	#w <- cumsum(w/sum(w))
	
	#r <- runif(1)
	#x.dist <- as.matrix(dist(c(prev,x)))[1,]
	#neighbors <- order(x.dist)[2:(k+1)] - 1
	#this.neighbor <- which(order(c(r,w))==1)
	#return(x[neighbors][this.neighbor])
	return(sample(x,1))
	
	
}