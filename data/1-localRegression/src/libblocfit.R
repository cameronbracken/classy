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