plot.blocfit <- 
function(obj, get.data = TRUE, abline = TRUE, get.ci = TRUE, legend = TRUE, 
	percent = "%"){
 
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