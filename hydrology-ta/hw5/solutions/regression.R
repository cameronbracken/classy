require(fields)
require(matlab)

	# load the precip data for january
janp <- read.table('colo_monthly_precip_01.dat')
names(janp) <- c('y', 'x', 'z', 'p')
janp <- janp[!(janp$p == -999.999),]

	# load the precip data for july
julp <- read.table('colo_monthly_precip_07.dat')
names(julp) <- c('y', 'x', 'z', 'p')
julp <- julp[!(julp$p == -999.999),]

	#load the dem
dem <- read.table('colo_dem.dat')
names(dem) <- c('y', 'x', 'z')

	#fit the model for january
lmfit.jan <- lm(p~x+y+z,data=janp)
summary(lmfit.jan)

	# fit the model for july
lmfit.jul <- lm(p~x+y+z,data=julp)
summary(lmfit.jul)

	# predict at the dem points and the model points for january
lmpred.jan <- predict.lm( lmfit.jan, dem)
lmpredp.jan <- predict.lm( lmfit.jan, janp)

	# predict at the dem points and the model points for january
lmpred.jul <- predict.lm( lmfit.jul, dem)
lmpredp.jul <- predict.lm( lmfit.jul, julp)

	## Set up the data for plotting
nx <- length(unique(dem$x))
ny <- length(unique(dem$y))
xp <- sort(unique(dem$x))
yp <- sort(unique(dem$y))
zp.jan <- matrix(lmpred.jan,nrow=ny)
zp.jul <- matrix(lmpred.jul,nrow=ny)

	#surface and image plots for january
pdf('surf-jan.pdf')
	plot.surface(list(x=xp,y=yp,z=t(flipud(zp.jan))), 
		main='Annual Precip Predictions [mm]', 
		xlab='Longitude', ylab='Latitude')
dev.off()
pdf('image-jan.pdf')
	plot.surface(list(x=xp,y=yp,z=t(flipud(zp.jan))), type='p', 
		main='Annual Precip Predictions [mm]', zlab='Precip [mm]', 
		xlab='Longitude',ylab='Latitude')
dev.off()

	#surface and image plots for july
pdf('surf-jul.pdf')
	plot.surface(list(x=xp,y=yp,z=t(flipud(zp.jul))), 
		main='Annual Precip Predictions [mm]', 
		xlab='Longitude', ylab='Latitude')
dev.off()
pdf('image-jul.pdf')
	plot.surface(list(x=xp,y=yp,z=t(flipud(zp.jul))), type='p', 
		main='Annual Precip Predictions [mm]', zlab='Precip [mm]', 
		xlab='Longitude',ylab='Latitude')
dev.off()

	#Observed versus predicted for january
pdf('ovp-jan.pdf')
	plot(janp$p,lmpredp.jan,xlab='Observed',ylab='Predicted')
	abline(0,1)
dev.off()

	#Observed versus predicted for july
pdf('ovp-jul.pdf')
	plot(julp$p,lmpredp.jul,xlab='Observed',ylab='Predicted')
	abline(0,1)
dev.off()

	#Residual plots january
pdf('res-jan.pdf')
	res <- residuals(lmfit.jan)
	layout(rbind(c(1,2),c(3,4)))
	plot(janp$x,res,xlab='Longitude',ylab='Residuals',cex=.5)
	plot(janp$y,res,xlab='Latitude',ylab='Residuals',cex=.5)
	plot(janp$z,res,xlab='Elevation',ylab='Residuals',cex=.5)
	plot(janp$p,res,xlab='Precipitation',ylab='Residuals',cex=.5)
dev.off()

	#Residual plots July
pdf('ovp-jul.pdf')
	res <- residuals(lmfit.jul)
	layout(rbind(c(1,2),c(3,4)))
	plot(julp$x,res,xlab='Longitude',ylab='Residuals',cex=.5)
	plot(julp$y,res,xlab='Latitude',ylab='Residuals',cex=.5)
	plot(julp$z,res,xlab='Elevation',ylab='Residuals',cex=.5)
	plot(julp$p,res,xlab='Precipitation',ylab='Residuals',cex=.5)
dev.off()
