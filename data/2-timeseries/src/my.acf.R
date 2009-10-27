my.acf <- 
function(x, lag.max=2*frequency(x), plot=TRUE){
	
	acf <- numeric(lag.max)
	for(i in 1:lag.max){
		acf[i] <- mylag(x,i,docor=T)
	}
	ci <- 2/sqrt(length(x))
	if(plot){
		plot(acf,type='h',xlab='Lag',ylab='ACF')
		abline(h=c(ci,-ci),col='blue',lty=2)
		abline(h=0)
	}
	invisible(acf)
}