require(fields)
require(matlab)

	# load the precip data
rain <- read.table('colo_monthly_precip_01.dat')
names(rain) <- c('y', 'x', 'z', 'r')
rain <- rain[!(rain$r == -999.999),]

	#load the dem
dem <- read.table('colo_dem.dat')
names(dem) <- c('y', 'x', 'z')


lmfit <- lm(r~x+y+z,data=rain)
lmpred <- predict.lm( lmfit, as.data.frame(dem), se.fit=T)

	## Plot the surface
	## Easier way
nx <- length(unique(dem$x))
ny <- length(unique(dem$y))
quilt.plot(dem$x,dem$y,dem$z,
	nrow=nx,ncol=ny)

	## Another Way	
xp <- sort(unique(dem$x))
yp <- sort(unique(dem$y))
zp <- fliplr(matrix(dem$z,nrow=length(unique(dem$x)),byrow=T))
plot.surface(list(x=xp,y=yp,z=zp),zlab='precip')
mtext('Precip [mm]',side=4)
plot.surface(list(x=xp,y=yp,z=zp),type='p',zlab='precip')

	