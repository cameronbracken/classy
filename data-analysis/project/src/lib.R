mpbin <- function(bin,m,p){
	n <- length(bin)

	x <- matrix(NA, n-m-1, m)
	y <- matrix(NA, n-m-1, p)

	for(i in 1:(n-m-1)){
		x[i,] <- bin[i:(i+m-1)]
		y[i,] <- bin[(i+m):(i+m+p-1)]
	}
	return(list(x=x,y=y))
}