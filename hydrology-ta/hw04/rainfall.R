#!/usr/bin/env Rscript
##################################
# Rainfall visualization
##################################

# load add on packages (install if necessary)
# install.packages('tripack')
library(tripack)
# install.packages('maps')
library(maps)
# install.packages('fields')
library(fields)

	# if the precip data file is not downloaded, download it
if(!file.exists('colo_precip.dat')) 
	download.file('http://cires.colorado.edu/~aslater/CVEN_6833/colo_precip.dat',
	'colo_precip.dat')
	
if(!file.exists('colo_dem.dat')) 
	download.file('http://cires.colorado.edu/~aslater/CVEN_6833/colo_dem.dat',
	'colo_dem.dat')

	# load the precip data
rain <- read.table('colo_precip.dat')
names(rain) <- c('y', 'x', 'z', 'r')
	# fit a spline surface
Z <- Tps(cbind(rain$x,rain$y),rain$r)

	#plot rainfall surface
pdf('rf.pdf')
	surface(Z,xlab='Longitude', ylab = 'Latitude')
	map('state',add=T,lwd=2)
dev.off()

	#load the dem
dem <- read.table('colo_dem.dat')
names(dem) <- c('y', 'x', 'z')
	# fit a spline surface
Zdem <- Tps(cbind(dem$x,dem$y),dem$z)

	#plot dem surface
pdf('dem.pdf')
	surface(Zdem,xlab='Longitude', ylab = 'Latitude')
	map('state',add=T,lwd=2)
dev.off()


##################################
# Rainfall analysis
##################################
	#remove duplicate values
dupe <- duplicated(rain[,1:2])
rain <- rain[!dupe,]

	#compute the voronoi polygons
v <- voronoi.mosaic(x = rain$x, y = rain$y,'strip')

pdf('voronoi.pdf')
		#plot the gauges and the polygons on a map
	plot(rain$x,rain$y,pch=16,cex=.5,xlab='Longitude', ylab = 'Latitude')
	plot(v, do.points=F, add=T)
	map('state',add=T,lwd=2)
dev.off()

	#area of each polygon
area <- voronoi.area(v)

	# percent of total area of each polygon
parea <- area/sum(area,na.rm=T)
thies.est <- sum(parea * rain$r,na.rm=T)
arith.est <- mean(rain$r)



##################################
# Intesity Duration Data 
# data from http://hdsc.nws.noaa.gov/hdsc/pfds/pfds_series.html
##################################

	# Read in the data and convert to intenstiy from depth
id1 <- read.table('independence-1hr.txt')[,2]
id6 <- read.table('independence-6hr.txt')[,2] / 6
id24 <- read.table('independence-24hr.txt')[,2] / 24


	# number of points
n1 <- length(id1)
n6 <- length(id6)
n24 <- length(id24)

	#emperical quantiles
q1 <- n1:1/n1
q6 <- n6:1/n6
q24 <- n24:1/n24

pdf('cdf.pdf')
	plot(q1,sort(id1),type='n',ylim=c(0,max(id1)), 
		ylab='Rainfall Intensity [in/hr]', xlab = 'Excedance Probability')
	grid()
	lines(q1,sort(id1),type='s',col=1)
	lines(q6,sort(id6),type='s',col=2)
	lines(q24,sort(id24),type='s',col=3)

	legend('topright',c('1 hr', '6 hr', '24 hr'),lty=1,col=1:3)
dev.off()

##################################
# IDF Curves
##################################

	# return probablilities and idf values
rp <- 1-1/c(2,10,25,100)
idf <- cbind(quantile(id1,rp),quantile(id6,rp),quantile(id24,rp))

pdf('idf.pdf')
	plot(c(1,6,24),idf[1,],ylim=range(idf),xlim=c(1,24),
		type='n',log='xy',xlab='Duration [hr]', ylab = 'Intensity [in/hr]',
		main = "IDF curves for Independence, CA")
	grid(10)
	lines(c(1,6,24),idf[1,],col=1, lty=1,lwd=2)
	lines(c(1,6,24),idf[2,],col=2, lty=2,lwd=2)
	lines(c(1,6,24),idf[3,],col=3, lty=3,lwd=2)
	lines(c(1,6,24),idf[4,],col=4, lty=4,lwd=2)
	legend('bottomleft',c('2 yr', '10 yr', '25 yr', '100 yr'),lty=1:4,lwd=2,col=1:4)
dev.off()
