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
 
	pred <- predict.blocfit( obj, Lmat = TRUE)
	
		# Hat matrix
	obj$hat <- pred$L
	
		# Degrees of freedom 
	obj$v1 <- sum(diag(pred$L))
	obj$v2 <- sum(diag(t(pred$L) %*% pred$L))
 
	obj$fittedValues <- pred$fittedValues
	obj$resid <- x - pred$fittedValues
	obj$sigma <- sd( obj$resid )
	obj$c <- qnorm(.95)
	obj$gcv <- obj$n * sum(obj$resid^2) / ( obj$n - obj$v1 )^2
 
	class( obj ) <- "blocfit"
	return( obj )
 
}