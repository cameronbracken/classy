#!/usr/bin/env Rscript

suppressPackageStartupMessages(require(locfit))
suppressPackageStartupMessages(require(fields))

data <- read.table('data/colo_precip.dat',header=TRUE)
y <- data$precip
xdata <- as.data.frame(data[c('lat','lon','elev')])
x <- as.matrix(xdata)

dem <- as.matrix(read.table('data/colo_dem.dat',header=T))
dem.x <- dem[,1]
dem.y <- dem[,2]
dem.z <- dem[,3]


n <- length(y)
a <- seq(0.2,1.0,by=0.05)

	# get the gcv values for all combinations of deg and alpha
d1 <- gcvplot(y~x, deg=1, alpha=a, kern='bisq', scale=T, ev=dat())
d2 <- gcvplot(y~x, deg=2, alpha=a, kern='bisq', scale=T, ev=dat())
	
gcvs <- c(d1$values,d2$values)
best <- order(gcvs)[1]
	#get the best alpha and degree
bestd <- c(rep(1,n),rep(2,n))[best]
bestalpha <- c(a,a)[best]

datfit <- locfit(y~x, deg=bestd, alpha = bestalpha, kern='bisq', 
		scale=T, ev=dat())
fit <- locfit(y~x, deg=bestd, alpha = bestalpha, kern='bisq', scale=T)
	# the cross validated estimates
fitcv <- locfit(y ~ x, alpha=bestalpha, deg=bestd, kern="bisq", 
		ev = dat(cv=TRUE), scale=TRUE)

pred <- predict( fit, newdata = dem, se.fit=T )

	# y must be a vector, xdata a data.frame
lmfit <- lm(y~., data = xdata)
lmpred <- predict.lm( lmfit, as.data.frame(dem), se.fit=T)

	# prepare the data for plotting
nbcol <- 20
nx <- length(unique(dem.x))
ny <- length(unique(dem.y))
locfitgrid <- matrix(pred$fit, nrow = ny, byrow=T)
lmgrid <- matrix(lmpred$fit, nrow = ny, byrow=T)
dem.x <- sort(unique(dem.x))
dem.y <- sort(unique(dem.y))

colf <- function(z){
		nrz <- nrow(z)
		ncz <- ncol(z)
		# Generate the desired number of colors from this palette
		# Compute the z-value at the facet centres
		zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]
		# Recode facet z-values into color indices
		facetcol <- cut(zfacet, nbcol)
		facetcol
	}
color <- topo.colors(nbcol)


save(list=ls(),file='output/2.Rdata')