predict.blocfit <- function(fit, at = fit$x, Lmat = FALSE){
	
	np <- length(at)
	fittedValues <- numeric(np)
	L <- if(Lmat) matrix(NA, nrow = np, ncol = fit$n) else NULL
	
		#for all the points to predict get the fitted values
	for(i in 1:np){
		
		pred <- predict.point(fit, at[i])
		fittedValues[i] <- pred$yhat
		if(Lmat) L[i,] <- pred$li
		
	}
	
	return( list(fittedValues = fittedValues, L = L) )
	
}