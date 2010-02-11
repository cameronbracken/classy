require(fields)
require(matlab)

	# load the precip data
janp <- read.table('colo_monthly_precip_01.dat')
names(janp) <- c('y', 'x', 'z', 'p')
janp <- janp[!(janp$p == -999.999),]

	#load the dem
dem <- read.table('colo_dem.dat')
names(dem) <- c('y', 'x', 'z')


lmfit.jan <- lm(p~x+y+z,data=janp)
summary(lmfit.jan)

lmpred.jan <- predict.lm( lmfit.jan, dem)
lmpredp.jan <- predict.lm( lmfit.jan, janp)

	## Plot the surface
nx <- length(unique(dem$x))
ny <- length(unique(dem$y))
#quilt.plot(dem$x,dem$y,dem$z, nrow=nx,ncol=ny)
#mtext('Precip [mm]',side=4)

	## Another Way	
xp <- sort(unique(dem$x))
yp <- sort(unique(dem$y))
zp <- matrix(lmpred.jan,nrow=ny)

#plot.surface(list(x=xp,y=yp,z=zp),zlab='precip')
plot.surface(list(x=xp,y=yp,z=t(flipud(zp))),type='p',
	main='Annual Precip Predictions',zlab='Precip [mm]')

	#Observed versus predicted
plot(janp$p,lmpredp.jan,xlab='Observed',ylab='Predicted')
abline(0,1)

	#Residual plots
res <- residuals(lmfit.jan)
layout(rbind(c(1,2),c(3,4)))
plot(janp$x,res,xlab='Longitude',ylab='Residuals',cex=.5)
plot(janp$y,res,xlab='Latitude',ylab='Residuals',cex=.5)
plot(janp$z,res,xlab='Elevation',ylab='Residuals',cex=.5)
plot(janp$p,res,xlab='Precipitation',ylab='Residuals',cex=.5)
