blocfit <- 
function(x, y, a = 0.7, p = 1, kern = 'bisq'){
	
	obj <- list(x = x, 
				y = y, 
				a = a, 
				n = length( y ), 
				nn = round( a*length( y ) ), 
				p = p, 
				W = chooseWeightFunction( kern )
				)
	
	class( obj ) <- "blocfit"
	return( obj )

}

predict.point <- function(obj, at, dist = 'global'){

		# create the "design" matrix, from taylor expansion
	x <- obj$x - at
	X <- if(obj$p == 1)
			cbind(rep(1,obj$n), x)
		else if(obj$p == 2)
			cbind(rep(1,obj$n), x, x^2)
		else
			cbind(rep(1,obj$n), x, x^2, x^3)
			
		# Distance from estimate to other points
	d <- as.matrix( dist( scale( c(at, obj$x) ) ) )[1,2:(obj$n+1)]
		
		#scale distances to [0,1]
	dk <- d[order(d)[obj$nn]]
	d[d > dk] = dk
	d <- d/dk
	
		#calculate weights
	W <- diag(obj$W(d))
	
		#solve for local betas
	betas <- solve(t(X) %*% W %*% X) %*% (t(X) %*% W %*% obj$y)
	
		# make local design vector
	ld <-if(obj$p == 1)
			c(1,at)
		else if(obj$p == 2)
			c(1,at,at^2)
		else
			c(1,at,at^2,at^3)
			
		# get estimate at new point
	yhat <- sum(betai * ld)
}

predict.blocfit <- function(fit, at = fit$x){
	
	#calculate distance
	d <- dist[i,]
	d <- d/max(d)
	nearest <- order(d)[1:nn]
			
	#fit the local model
	lfit <- lm( y[nearest] ~ x[nearest], weights = W(d[nearest]) )
	values[i] <- lfit$fitted.values[1]
	
}

plot.blocfit <- function(obj){
	
	
}

chooseWeightFunction <-
function(kern){
	
	if(kern == 'bisq')
		return( function(x) 15/16*(1-x^2)^2 )
	if(kern == 'none')
		return( function(x) rep(1,length(x)) )
	
}