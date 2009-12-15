my.pacf <- function(x, lag.max=2*frequency(x), plot=TRUE){
	pacf <- numeric(lag.max)
	acf <- my.acf(x, lag.max = lag.max, plot=FALSE)
	if(lag.max==1) return(acf)
	pacf[1] <- acf[1]
		# pacf for each lag
	for(q in 2:lag.max){	
		r <- acf[1:q]
			#get the top and bottom matricies for pacf 
		top <- matrix(NA,q,q)
		top[1:(q-1),1:(q-1)] <- ac.mat(x,q-1)
		top[,q] <- r
		top[q,1:(q-1)] <- rev(r[1:(q-1)])
		bot <- ac.mat(x,q)	
		pacf[q] <- det(top)/det(bot)
	}
	ci <- 2/sqrt(length(x))
	if(plot){
		plot(pacf,type='h',xlab='Lag',ylab='PACF')
		abline(h=c(ci,-ci),col='blue',lty=2)
		abline(h=0)
	}
	invisible(pacf)
}
