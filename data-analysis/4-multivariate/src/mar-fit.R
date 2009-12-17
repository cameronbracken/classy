mar.fit <- function(Z, ny = length(Z[,1,1])){
	
	## A and B for each month
	A <- B <- array(list(),12)

	for( month in 1:12 ){

		last.month <- ifelse(month == 1, 12, month - 1)

		if(month == 1){
			Zt <- Z[ 2:ny, month, ]
			Zt1 <- Z[ 1:(ny-1), last.month, ] 
		}else{
			Zt <- Z[ 1:ny, month, ] 
			Zt1 <- Z[ 1:ny, last.month, ] 
		}

		#Covariance matricies
		M0 <- var(Zt)
		M01 <- var(Zt1)
		M1 <- var(Zt,Zt1)
		
		#A coefficient matrix
		A[[month]] <- M1  %*% solve(M01)

		#B coefficient matrix
		D <- M0 - M1 %*% solve(M01) %*% t(M1)
		B[[month]] <- svd(D)$u %*% sqrt(diag(svd(D)$d))
	}
	
	return(list(A=A,B=B))
	
}
