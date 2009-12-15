sspectrum <- function(y){
	N <- length(y)

	# compute the auto covariance..
	nlag  <- N - 1
	c <- acf(y,type="covariance",lag.max=nlag,plot=F)$acf

	#lag window width
	M  <- round(N/10)		# M should be even
	if(M %% 2 > 0)M <- M+1
	# try different Window widths and different window types....

	#Parzen Window..
	lambdas <- 1:M
	lambdas[1:(M/2)] <- 
		1-(6*((lambdas[1:(M/2)]/M)^2))+ (6*((lambdas[1:(M/2)]/M)^3))
	lambdas[((M/2)+1):M] <- 2*((1-(lambdas[((M/2)+1):M]/M))^3)
	lambda0 <- 1
	degf <- 3.709*N/M

	# number of frequencies possible..
	freqs <- 2*pi*(1:round((N/2)))/N
	plotfreqs <- freqs/(2*pi*(1/12))
	nfreq <- length(freqs)

	tordf <- 1:round((N/2))
	tord <- 1:N

	# compute the smoothed periodgram/spectrum.. - Equation 12.3.15 Wei

	pgrams <- pgrams.ar <- 1:nfreq

	for(i in 1:nfreq)
		pgrams[i] <- 
			((lambda0*c[1]) + (2*sum(lambdas*c[2:(M+1)]*cos(freqs[i]*(1:M)))))/pi


	#confidence intervals..

	lowfac <- degf/qchisq(0.975,degf)
	upfac <- degf/qchisq(0.025,degf)
	spfft.u <- pgrams*upfac
	spfft.l <- pgrams*lowfac

	#plot(plotfreqs,pgrams,xlab="freq. cy/yr",ylab="Spectrum", type="l")

	#conflines(plotfreqs,spfft.u,spfft.l)


	### To get the significance of a peak - you can do it couple of ways
	## as mentioned in the class

	#1. Fit an AR-1 model to the data
	armodel = ar((y-mean(y)),order.max=1)
	#2. Simulate from  the fitted model of the same length

	nsim = 5000
	specsim = matrix(0,ncol=nsim,nrow=nfreq)

	for(isim in 1:nsim){

		ysim  <- arima.sim(n=N, list(ar=c(armodel$ar)), 
			sd=sqrt(armodel$var.pred)) + mean(y)

		#3. Compute the spectrum  in the same manner as you did for the data
		# above..

		c=acf(ysim,type="covariance",lag.max=nlag,plot=F)$acf
		for(i in 1:nfreq)
			pgrams.ar[i] <- 
				((lambda0*c[1]) + (2*sum(lambdas*c[2:(M+1)]*cos(freqs[i]*(1:M)))))/pi
	
		specsim[,isim] <- pgrams.ar
	}

	#4. Repeat - obtain the 95th percentile of the spectrum at each
	#frequency. plot it.
	yconflev = 1:nfreq
	for(i in 1:nfreq)
		yconflev[i] <- quantile(specsim[i,],c(0.95))

	## plot yconflev on the spectrum figure and see which peaks come above
	# this..
	x <- data.frame(freq=plotfreqs,spec=pgrams,ucl=spfft.u,lcl=spfft.l,sig=yconflev)
	return(x)
}
