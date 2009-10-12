predict.point <- function(obj, at, dist = 'global'){

		# create the "design" matrix, from taylor expansion
	x <- obj$x - at
	X <- if(obj$p == 1){
			cbind(rep(1,obj$n), x)
		}else if(obj$p == 2){
			cbind(rep(1,obj$n), x, x^2)
		}else
			cbind(rep(1,obj$n), x, x^2, x^3)
		
			
		# Distance from estimate to other points
	d <- as.matrix( dist( scale( c(at, obj$x) ) ) )[1,2:(obj$n+1)]
		
		#scale distances to [0,1]
	dk <- d[order(d)[obj$nn]]
	d[d > dk] <- dk
	d <- d/dk
	
		#calculate weights
	W <- diag(obj$W(d))
	
		# get influence vector
	e1 <- c(1,rep(0,obj$p))
	li <- e1 %*% ( solve(t(X) %*% W %*% X) ) %*% ( t(X) %*% W )

	yhat <- sum(li * obj$y)
	
	o <- list(at = at, yhat = yhat, li = li)
	return(o)
	
}