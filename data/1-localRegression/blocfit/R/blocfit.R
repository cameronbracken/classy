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
	
		# degrees of freedom 
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

predict.point <- 
function(obj, at, dist = 'global'){

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

predict.blocfit <- 
function(fit, at = fit$x, Lmat = FALSE){
	
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


plot.blocfit <- 
function(obj, get.data = TRUE, abline = TRUE, get.ci = TRUE, legend = TRUE, percent = "%"){
	
	if(class(obj) != "blocfit") stop('object is not of class blocfit')
	
	xnew <- seq(range(obj$x)[1],range(obj$x)[2],length.out=100)
	pred <- predict.blocfit( obj, at = xnew )
	if(get.ci) ci <- ci.blocfit( obj, xnew )
	
		# plot the fit 
	plot(obj$x,obj$y,type='n', xlab = "X", ylab = "Y")
	
		#upper and lower confidence intervals
	if(get.ci) lines(xnew,ci[1,],lty='dashed')
	if(get.ci) lines(xnew,ci[2,],lty='dashed')
	
		#plot the data points
	if(get.data) points(obj$x,obj$y)
	
		#plot the linear regression line
	if(abline) abline(lm(obj$y~obj$x),col='red')
	
	lines(xnew,pred$fittedValues)
	
		#plot a legend
	if(legend){
		ltext <- c("Fit")
		if(get.data) ltext <- c(ltext, "Data")
		if(get.ci) ltext <- c(ltext, paste("95",percent,"CI"))
		if(abline) ltext <- c(ltext, "Linear Reg")
		
		legend("topleft", 
				ltext, 
				lty = c(1,0,2,1),
				pch = c(-1,1,-1,-1),
				col = c(1,1,1,2))
	}
	
}


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