solve.yw <- 
function(x,p){
	
	r <- my.acf(x, lag.max = p, plot=FALSE)
	M <- ac.mat(x,p)
	
	phi <- solve(M) %*% cbind(r)
	colnames(phi) <- ''
	sigsq <- var(x)*(1-sum(r * phi))
	aic <- length(x)*log(sigsq)+2*(p+1)
	
	return(list(phi=phi,sigsq=sigsq,aic=aic))
	
}

ac.mat <- 
function(x,p){
	
	rho <- c(1,my.acf(x, lag.max = p - 1, plot=FALSE))
	n <- p
	M <- matrix(NA,n,n)
	for(i in 1:n){
		if(i==1){
			v <- 1:n
		}else if(i==n){
			v <- rev(1:n)
		}else{
			v <- c(i:1,2:(n-i+1))
		}
		M[i,] <- rho[v]
	}
	M
	
}