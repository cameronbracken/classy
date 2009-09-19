blocfit <- 
function(x, y, a = 0.7, p = 1, kern = 'bisq'){
	
	obj <- list(x = x, 
				y = y, 
				a = a, 
				n = length( y ), 
				nn = round( a*n ), 
				p = p, 
				W = chooseWeightFunction( kern ),
				d = as.matrix(dist(scale(x)))
				)
	
	class( obj ) <- "blocfit"
	return( obj )

}

predict.point <- function(obj, at, dist = 'global'){

	
	X <- if(obj$p == 1)
			cbind(rep(1,n), obj$x)
		else if(obj$p == 2)
			cbind(rep(1,n), obj$x, obj$x^2))
		else
			cbind(rep(1,n), obj$x, obj$x^2, obj$x^3))
			
	d <- as.matrix(dist(scale(c(at,obj$x))))[1,]
	d <- d/max(d)
	
	

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