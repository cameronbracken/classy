fc.svd <- function(flow,swe,nsim){
	n <- nrow(flow)
	n.sites <- ncol(flow)
	nsim <- 100
	ypred <- array(NA,c(n.sites,nsim,n))
	yy <- numeric(n.sites)

	for(i in 1:n){
		#drop a point..
		swe.cv <- swe[-i,]
		flow.cv <- flow[-i,]

		#perform SVD
		C <- var(flow.cv,swe.cv)  
		S <- svd(C)

		#Get the tc's
		flow.cv.tc <- flow.cv %*% S$u
		swe.cv.tc  <- swe.cv %*% S$v
		cv.model <- lsfit(swe.cv.tc[,1],flow.cv.tc[,1])
		stderr <- sum((cv.model$resid)^2) / (nrow(flow.cv) - 2)

		#prediction error..
		Sxx <- sum((swe.cv.tc[,1] - mean(swe.cv.tc[,1]))^2)

		#the TCs of the dropped point..
		xp <- swe[i,] %*% S$v

		#generate an ensemble..
		for(isim in 1:nsim){
			this.stderr <- sqrt(stderr * 
				(1 + (1/(n-1)) + 
					((xp[1] - mean(swe.cv.tc[,1]))^2)/Sxx))

			yy[1] <- cv.model$coef[1] + 
				cv.model$coef[2]*xp[1] + rnorm(1,0,this.stderr)

			#grab random TCs for the remaining TCs
			for(site in 2:n.sites){

				r.tc <- round(runif(1,1,(n-1)))
				yy[site] <- flow.cv.tc[r.tc,site]
			}
			ypred[1:n.sites,isim,i]=yy %*% t(S$u)
		}
	}
	ypred[ypred < 0] <- 0
	return(ypred)
}
