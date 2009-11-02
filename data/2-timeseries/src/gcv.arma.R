gcv.arma <- 
function(m){
	
	n <- length(m$residuals)
	n*m$sigma2/(n-length(m$coef))^2
	
}
