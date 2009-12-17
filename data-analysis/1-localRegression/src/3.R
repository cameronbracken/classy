#!/usr/bin/env Rscript

suppressPackageStartupMessages(require(locfit))
suppressPackageStartupMessages(require(leaps))

	#11 data
data.11 <- read.table('data/colo_pcp_daily_1997_01_11.dat',header=TRUE)
data.11 <- data.11[!(data.11$precip == -999.999),]
y.11 <- data.11$precip
y.11[y.11 > 0] = 1
xdata.11 <- as.data.frame(data.11[c('lat','lon','elev')])
x.11 <- as.matrix(xdata.11)

	#12 Data
data.12 <- read.table('data/colo_pcp_daily_1997_01_12.dat',header=TRUE)
data.12 <- data.12[!(data.12$precip == -999.999),]
y.12 <- data.12$precip
y.12[y.12 > 0] = 1
xdata.12 <- as.data.frame(data.12[c('lat','lon','elev')])
x.12 <- as.matrix(xdata.12)

	#13 Data
data.13 <- read.table('data/colo_pcp_daily_1997_01_13.dat',header=TRUE)
data.13 <- data.13[!(data.13$precip == -999.999),]
y.13 <- data.13$precip
y.13[y.13 > 0] = 1
xdata.13 <- as.data.frame(data.13[c('lat','lon','elev')])
x.13 <- as.matrix(xdata.13)

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

#global glm
gfit.11 <- glm(y.11 ~., data=xdata.11, family=binomial(link="logit"))
gfit.12 <- glm(y.12 ~., data=xdata.12, family=binomial(link="logit"))
gfit.13 <- glm(y.13 ~., data=xdata.13, family=binomial(link="logit"))

# predict at the points of interest..
gpred.11 = predict.glm(gfit.11, as.data.frame(dem), se.fit=T, type="response")
gpred.12 = predict.glm(gfit.12, as.data.frame(dem), se.fit=T, type="response")
gpred.13 = predict.glm(gfit.13, as.data.frame(dem), se.fit=T, type="response")

	#returns the best alpha and degree
best.par.glm <- 
function(x,y, n = length(y), f=c(gcvplot,aicplot)){
	
	a1 <- seq(0.75,1.0,by=0.05); n1 <- length(a1)
	a2 <- seq(0.99,1.0,by=0.05); n2 <- length(a2)
		# get the gcv values for all combinations of deg and alpha
	d1 <- f(y~x, deg=1, alpha=a1, kern='bisq', scale=T,family="binomial",ev=dat())
	d2 <- f(y~x, deg=2, alpha=a2, kern='bisq', scale=T,family="binomial",ev=dat())
	
	gcvs <- c(d1$values,d2$values)
	best <- order(gcvs)[1]
		#get the best alpha and degree
	bestd <- c(rep(1,n1),rep(2,n2))[best]
	bestalpha <- c(a1,a2)[best]
	
	return(list(p=bestd,a=bestalpha,gcv=gcvs[best]))
}

best.pred.glm <- function(x,y,f){
	
	combo <- leaps(x,y)$which
	nc <- nrow(combo)
	best <- data.frame(a=numeric(nc),p=numeric(nc),gcv=numeric(nc))
	for(i in 1:nc){
		
		this.best <- best.par.glm(x[,combo[i,]],y,f=f)
		best$a[i] <- this.best$a
		best$p[i] <- this.best$p
		best$gcv[i] <- this.best$gcv
		
	}
	return(list(par=best,which=combo))
}


	#get the best degree and alpha for each day
best.11 <- best.par.glm(x.11,y.11,f=gcvplot)
best.12 <- best.par.glm(x.12,y.12,f=gcvplot)
best.13 <- best.par.glm(x.13,y.13,f=gcvplot)
best.models.11 <- best.pred.glm(x.11,y.11,f=gcvplot)
best.models.11.aic <- best.pred.glm(x.11,y.11,f=aicplot)

lgfit.11 <- locfit(y.11 ~ ., data=xdata.11, alpha=best.11$a, maxk = 10000, 
	deg=best.11$p, kern="bisq", scale = T, family="binomial")
lgfit.12 <- locfit(y.12 ~ ., data=xdata.12, alpha=best.11$a, maxk = 10000, 
	deg=best.12$p, kern="bisq", scale = T, family="binomial")
lgfit.13 <- locfit(y.13 ~ ., data=xdata.13, alpha=best.11$a, maxk = 10000, 
	deg=best.13$p, kern="bisq", scale = T, family="binomial")
	
lgpred.11 = predict.locfit(lgfit.11, as.data.frame(dem), se.fit=T )
lgpred.12 = predict.locfit(lgfit.12, as.data.frame(dem), se.fit=T )
lgpred.13 = predict.locfit(lgfit.13, as.data.frame(dem), se.fit=T )


	#grids for plotting
ggrid.11 <- matrix(gpred.11$fit, nrow = ny, byrow=T)
ggrid.12 <- matrix(gpred.12$fit, nrow = ny, byrow=T)
ggrid.13 <- matrix(gpred.13$fit, nrow = ny, byrow=T)
lggrid.11 <- matrix(lgpred.11$fit, nrow = ny, byrow=T)
lggrid.12 <- matrix(lgpred.12$fit, nrow = ny, byrow=T)
lggrid.13 <- matrix(lgpred.13$fit, nrow = ny, byrow=T)

save(list=ls(),file='output/3.Rdata')