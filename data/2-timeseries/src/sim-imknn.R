imknn <- function(x,nsims,nyears=nrow(x)){
	
		#fit locfit models for interannual modified knn
	x.ann <- apply(x,1,sum)
	models <- list()
	for( i in 1:12 ){		
			
			#get the sum of the last 12 mponths of flow for every point
		last.12 <- last.12.sum(x)
		models[[i]] <- list()
		if(i == 12) this.y <- x[,1][-1] else this.y <- x[,i+1][-1]
		this.x <- cbind(x[,i][-1],last.12[,i])
		
		best <- best.par(this.x,this.y,f=gcvplot)
		models[[i]]$lf <- locfit(this.y~this.x, deg=best$p, alpha=best$a, kern='bisq')
		models[[i]]$pred <- predict(models[[i]]$lf,cbind(x[,i][-1],last.12[,i]))
		models[[i]]$res <- this.y - models[[i]]$pred
	}
	st <- mean(x[,1])
	k <- round(sqrt(length(x[,1])))
	w <- numeric(k)
	for(i in 1:k)
		w[i] <- (1/i)/(sum(1/(1:i)))
	w <- cumsum(w/sum(w))
	
	sim.imknn.v <- numeric(nsims*12*nyears)
	for(i in 1:(nsims*12*nyears-1)){
		
			#Loop through and simulate
		val <- if(i==1) st else sim.imknn.v[i]
		last.ann <- if(i <= 12) mean(x.ann) else sum(sim.imknn.v[(i-12):(i-1)])
		
		m <- ifelse((i %% 12) == 0,12,i %% 12)
		mp1 <- (i %% 12) + 1
		newp <- c(val,last.ann)
		this.pred <- predict(models[[m]]$lf,rbind(newp))
		dat <- cbind(x[,m],x.ann)[1:(nyears-1),]
		this.dist <- as.matrix(dist(rbind(newp,dat)))[,1]
		neighbors <- order(this.dist)[2:(k+1)] - 1
		
		#resample the residuals
		r <- runif(1)
		this.neighbor <- which(order(c(r,w))==1)
		this.res <- models[[m]]$res[neighbors][this.neighbor]

		sim.imknn.v[i+1] <- this.pred + this.res
	}
	return(sim.imknn.v)
}

