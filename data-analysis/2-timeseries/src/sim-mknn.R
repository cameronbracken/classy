mknn <- function(x,nsims,nyears=nrow(x)){
	
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

	st <- mean(x[,1])
	k <- round(sqrt(length(x[,1])))

	w <- numeric(k)
	for(i in 1:k)
		w[i] <- (1/i)/(sum(1/(1:i)))
	w <- cumsum(w/sum(w))

	sim.mknn.v <- numeric(nsims*12*nyears)
	for(i in 1:(nsims*12*nyears-1)){
		
			#Loop through and simulate
		val <- if(i==1) st else sim.mknn.v[i]
		m <- ifelse((i %% 12) == 0,12,i %% 12)
		mp1 <- (i %% 12) + 1
		this.pred <- predict(models[[m]]$lf,cbind(val))
		this.dist <- as.matrix(dist(c(val,x[,m])))[,1]
		neighbors <- order(this.dist)[2:(k+1)] - 1
		
			#resample the residuals
		r <- runif(1)
		this.neighbor <- which(order(c(r,w))==1)
		this.res <- models[[m]]$res[neighbors][this.neighbor]
	
		sim.mknn.v[i+1] <- this.pred + this.res
	}
	return(sim.mknn.v)
}

