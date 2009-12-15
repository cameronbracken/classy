#!/usr/bin/env Rscript

suppressPackageStartupMessages(require(locfit))
suppressPackageStartupMessages(require(fields))
suppressPackageStartupMessages(require(leaps))

	#January data
data.jan <- read.table('data/colo_monthly_precip_01.dat',header=TRUE)
data.jan <- data.jan[!(data.jan$precip == -999.999),]
y.jan <- data.jan$precip
xdata.jan <- as.data.frame(data.jan[c('lat','lon','elev')])
x.jan <- as.matrix(xdata.jan)

	#July Data
data.jul <- read.table('data/colo_monthly_precip_07.dat',header=TRUE)
data.jul <- data.jul[!(data.jul$precip == -999.999),]
y.jul <- data.jul$precip
xdata.jul <- as.data.frame(data.jul[c('lat','lon','elev')])
x.jul <- as.matrix(xdata.jul)

	#DEM data
dem <- as.matrix(read.table('data/colo_dem.dat',header=T))
dem.x <- dem[,1]
dem.y <- dem[,2]
dem.z <- dem[,3]

	# prepare the data for plotting
nx <- length(unique(dem.x))
ny <- length(unique(dem.y))
dem.x <- sort(unique(dem.x))
dem.y <- sort(unique(dem.y))

	#returns the best alpha and degree
best.par <- 
function(x,y, a = seq(0.2,1.0,by=0.05), n = length(a), f=c(gcvplot,aicplot)){
	
		# get the gcv values for all combinations of deg and alpha
	d1 <- f(y~x, deg=1, alpha=a, kern='bisq', scale=T)
	d2 <- f(y~x, deg=2, alpha=a, kern='bisq', scale=T)
	
	gcvs <- c(d1$values,d2$values)
	best <- order(gcvs)[1]
		#get the best alpha and degree
	bestd <- c(rep(1,n),rep(2,n))[best]
	bestalpha <- c(a,a)[best]
	
	return(list(p=bestd,a=bestalpha,gcv=gcvs[best]))
}

best.pred <- function(x,y,f){
	
	combo <- leaps(x,y)$which
	nc <- nrow(combo)
	best <- data.frame(a=numeric(nc),p=numeric(nc),gcv=numeric(nc))
	for(i in 1:nc){
		
		this.best <- best.par(x[,combo[i,]],y,f=f)
		best$a[i] <- this.best$a
		best$p[i] <- this.best$p
		best$gcv[i] <- this.best$gcv
		
	}
	return(list(par=best,which=combo))
}

	#get the best degree and alpha for each month
best.jan <- best.par(x.jan,y.jan,f=gcvplot)
best.jul <- best.par(x.jul,y.jul,f=gcvplot)
best.jul.aic <- best.par(x.jul,y.jul,f=aicplot)
best.models.jul <- best.pred(x.jul,y.jul,f=gcvplot)
best.models.jul.aic <- best.pred(x.jul,y.jul,f=aicplot)

	#Fit models for january
	# At the grid points
datfit.jan <- locfit(y.jan~x.jan, deg=best.jan$p, alpha = best.jan$a, 
		kern='bisq', scale=T, ev=dat())
	# At the data density based locations
fit.jan <- locfit(y.jan~x.jan, deg=best.jan$p, alpha = best.jan$a, 	
		kern='bisq', scale=T)
	# And the cross validated estimates
fitcv.jan <- locfit(y.jan~x.jan, deg=best.jan$p, alpha=best.jan$a, 
		kern="bisq", ev = dat(cv=TRUE), scale=TRUE)

	#Fit models for July
datfit.jul <- locfit(y.jul~x.jul, deg=best.jul$p, alpha = best.jul$a, 
		kern='bisq', scale=T, ev=dat())
fit.jul <- locfit(y.jul~x.jul, deg=best.jul$p, alpha = best.jul$a, 
		kern='bisq', scale=T)
fitcv.jul <- locfit(y.jul~x.jul, deg=best.jul$p, alpha=best.jul$a, 
		kern="bisq", ev = dat(cv=TRUE), scale=TRUE)

	#get estimates at dem points
pred.jan <- predict( fit.jan, newdata = dem, se.fit=T )
pred.jul <- predict( fit.jul, newdata = dem, se.fit=T )

	#fit linear models
	# y must be a vector, xdata a data.frame
	# January
lmfit.jan <- lm(y.jan~., data = xdata.jan)
lmpred.jan <- predict.lm( lmfit.jan, as.data.frame(dem), se.fit=T)
	# July 
lmfit.jul <- lm(y.jul~., data = xdata.jul)
lmpred.jul <- predict.lm( lmfit.jul, as.data.frame(dem), se.fit=T)

	#January grid
locfitgrid.jan <- matrix(pred.jan$fit, nrow = ny, byrow=T)
lmgrid.jan <- matrix(lmpred.jan$fit, nrow = ny, byrow=T)
	#july grid
locfitgrid.jul <- matrix(pred.jul$fit, nrow = ny, byrow=T)
lmgrid.jul <- matrix(lmpred.jul$fit, nrow = ny, byrow=T)

	#January error grid
locfitgrid.se.jan <- matrix(pred.jan$se.fit, nrow = ny, byrow=T)
lmgrid.se.jan <- matrix(lmpred.jan$se.fit, nrow = ny, byrow=T)
	#july error grid
locfitgrid.se.jul <- matrix(pred.jul$se.fit, nrow = ny, byrow=T)
lmgrid.se.jul <- matrix(lmpred.jul$se.fit, nrow = ny, byrow=T)

	# lm cv predictions
lmpred.cv.jan <- numeric(length(y.jan))
lmpred.cv.jul <- numeric(length(y.jul))
	#January
for(i in 1:length(y.jan)){
	tmp <- lm(y.jan[-i]~., data = xdata.jan[-i,])
	lmpred.cv.jan[i] <- predict.lm(tmp, as.data.frame(xdata.jan[i,]))
}
		# July 
for(i in 1:length(y.jul)){
	tmp <- lm(y.jul[-i]~., data = xdata.jul[-i,])
	lmpred.cv.jul[i] <- predict.lm(tmp, as.data.frame(xdata.jul[i,]))
}

save(list=ls(),file='output/2.Rdata')