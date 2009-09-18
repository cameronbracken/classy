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

}

predict.point <- function(obj, at, dist = 'global'){

	X <- ifelse(obj$p == 1, 
			cbind(rep(1,n),obj$x), 
			cbind(rep(1,n),obj$x,obj$x^2))

}

predict.blocfit <- function(fit, dist = 'global', at = fit$x){
	
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