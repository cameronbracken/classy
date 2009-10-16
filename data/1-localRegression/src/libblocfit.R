source('blocfit.R')
source('predict.blocfit.R')
source('predict.point.R')
source('plot.blocfit.R')


ci.blocfit <-
function( fit, at = fit$x ){
	
	#Calculate confidence intervals at arbitrary points
	
	ci <- matrix(NA, nrow = 2, ncol = length(at))
	
	for( i in 1:length(at)){
		
		pred <- predict.point(fit, at[i])
		
		ci[1,i] <- pred$yhat - fit$c * fit$sigma * sqrt(sum(pred$li^2))
		ci[2,i] <- pred$yhat + fit$c * fit$sigma * sqrt(sum(pred$li^2))
	}
	
	return(ci)
	
}


summary.blocfit <- 
function( fit ){
	
	cat("\tGCV:",round(fit$gcv,3),'\n')
	cat("\tRSS:",round(fit$sigma,3),'\n')
	cat("\t v1:",round(fit$v1,3),'\n')
	cat("\t v2:",round(fit$v2,3),'\n')
	
}

chooseWeightFunction <-
function(kern){
	
	if(kern == 'bisq')
		return( function(x) 15/16*(1-x^2)^2 )
	if(kern == 'none')
		return( function(x) rep(1,length(x)) )
	
}

anova.locfit <- function(fit,y, sep='\n'){
	rem <- !is.na(y)
	cat('SST =',sum((y[rem] - mean(y[rem]))^2),sep)
	yhat <- predict(fit)
	SSR <- sum((yhat - mean(y[rem]))^2)
	cat('SSR =', SSR, sep)
	SSE <- sum(residuals(fit)^2) 
	cat('SSE =', SSE,sep)
	cat('MSR =', SSR / length(fit$vnames),sep)
	cat('MSE =', SSE/(length(y) - fit$dp["df2"]))
}

	#Compute a nice set of face colors for the persp plot
colf <- function(z){
		nrz <- nrow(z)
		ncz <- ncol(z)
		# Generate the desired number of colors from this palette
		# Compute the z-value at the facet centres
		zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]
		# Recode facet z-values into color indices
		facetcol <- cut(zfacet, nbcol)
		facetcol
	}