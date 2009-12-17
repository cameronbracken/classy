fc.stats <- function(flow,ypred,nsim){

	n.sites <- ncol(flow)
	site.rpss <- site.mc <- numeric(n.sites)
	n <- nrow(flow)
	
	##Calculate rpss
	for(i in 1:n.sites){

		thresh <- quantile(flow[,i], c(0.33, 0.66))

		# climatological forecast..
		climo=cumsum(c(1/3, 1/3, 1/3))

		rpss = numeric(n)
		for(jj in 1:n){

			# forecast categorical probabilities
			fcastprob=1:3
			yypred=ypred[i,,jj]
			fcastprob[1] <- length(yypred[yypred <= thresh[1]]) / nsim
			fcastprob[2] <- 
				length(yypred[yypred > thresh[1] & 
					yypred < thresh[2]]) / nsim
			fcastprob[3] <- length(yypred[yypred >= thresh[2]]) / nsim

			fcastprob=cumsum(fcastprob)

			# actual..
			actual=rep(0,3)
			if(flow[jj,i] <= thresh[1])actual[1]=1
			if(flow[jj,i] > thresh[1] & flow[jj,i] < thresh[2])actual[2]=1
			if(flow[jj,i] >= thresh[2])actual[3]=1


			rpsclimo = sum((climo-actual)^2)
			rpsfcast = sum((fcastprob - actual)^2)
			rpss[jj] = 1 - (rpsfcast/rpsclimo)

		}
		#print out the median RPSS
		site.rpss[i] <- median(rpss)
		site.mc[i] <- cor(flow[,i],apply(ypred[i,,],2,median))
	}
	return(list(rpss=site.rpss,mc=site.mc))
}
