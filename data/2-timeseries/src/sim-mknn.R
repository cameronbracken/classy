mknn <- function(x,nsims,nyears=nrow(x),plot=FALSE,
					resample=c('resid','norm','none')){
		#fit locfit models for modified knn
	models <- list()
	for( i in 1:12 ){		
	
		models[[i]] <- list()
		if(i == 12){
			this.x <- x[,i]
			this.y <- x[,1]
		}else{
			this.x <- x[,i]
			this.y <- x[,i+1]
		}
		best <- best.par(this.x,this.y,f=gcvplot)
		models[[i]]$lf <- locfit(this.y~this.x, deg=best$p, alpha=best$a, kern='bisq')
		models[[i]]$pred <- predict(models[[i]]$lf,cbind(x[,i]))
		models[[i]]$res <- this.y - models[[i]]$pred
	}

	sim.mknn.v <- numeric(nsims*12*nyears)
	for(i in 1:(nsims*12*nyears-1)){
		
			#Loop through and simulate
		val <- if(i==1) st else sim.mknn.v[i]
		m <- ifelse((i %% 12) == 0,12,i %% 12)
		mp1 <- (i %% 12) + 1
		this.pred <- predict(models[[m]]$lf,cbind(val))
		this.dist <- as.matrix(dist(c(val,x[,m])))[,1]
		neighbors <- order(this.dist)[2:(k+1)] - 1
		
			#either resample the residuals or a normal random number
		if(match.arg(resample) == 'resid'){
			r <- runif(1)
			this.neighbor <- which(order(c(r,w))==1)
			this.res <- models[[m]]$res[neighbors][this.neighbor]
		}else if(match.arg(resample) == 'norm'){
			sd.res <- sd(models[[m]]$res[neighbors])
			this.res <- rnorm(1,sd=sd.res)
		}else{
			this.res <- 0
		}
		sim.mknn.v[i+1] <- this.pred + this.res
	}
	return(sim.mknn.v)
}

