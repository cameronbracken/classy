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

fit <- locfit(y~x, deg=bestd, alpha = bestalpha, kern='bisq', scale=T)
pred <- predict( fit, newdata = dem, se.fit=T )

# y must be a vector, xdata a data.frame
lmfit <- lm(y~., data = xdata)
lmpred <- predict( lmfit, as.data.frame(dem))

#print(lmfit)
#print(lmpred)
