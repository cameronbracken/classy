last.12.sum <- function(x){
	v <- as.vector(t(x))
	n <- length(v)
	mat <- matrix(NA,nrow(x)-1,ncol(x))
	i <- j <- 1
	for(t in (ncol(x)+1):n){
		if(j > ncol(mat)){
			j <- 1
			i <- i + 1
		}
		mat[i,j] <- sum(v[(t-12):(t-1)])
		j <- j + 1
	}
	return(mat)
}
