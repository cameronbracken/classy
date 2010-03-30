#!/usr/bin/env Rscript

# channel shear particle tracking

name <- 'channel_shear'
nd <- 2        # dimensions
np <- 5000    # particles
nv <- 0        # vortexes
nt <- 200      # timesteps
dt <- 1       # timestep 
U0 <- 1        # Bulk velocity
b <- .5        # half channel width
Uy <- 2/3*U0   #mean vertical velocity

D <- c(.1,0)  # Diffusion coefficient
nleak <- 0
movieFrameSkip <- 100
maxcnt <- np*.1

movie <- TRUE
plotType <- "hex" # or "hex" 
                  # or "ss" (smooth scatter) 
                  # or "h2d" (2d histogram) 
                  # or "ssi" (smooth scatter image)
                  # or "dc" (plot w/ density colors)

	# plotting boundaries
bnd.max.x <- ceiling(nt*sqrt(2*(D[1]+D[2])*dt)+U0*dt*nt)
bnd.max.y <- b
xlim <- c(-bnd.max.x,.4*bnd.max.x)
ylim <- c(-bnd.max.y,bnd.max.y)
times <- seq(dt,length.out=nt,by=dt)
var <- skew <- numeric(length(times))

	#required packages for plot types
suppressPackageStartupMessages(require(particleTracking))
source('my.hexbin.s4.R')
options(warn=-1)


positions <- matrix(0,np+nv,nd+1)
positions[,2] <- seq(-b,b,length.out=nrow(positions))

colorRamps()

if(movie){ 
	pdf(paste(name,'pdf',sep='.'))
}

pb <- txtProgressBar(dt,nt*dt,style=3)
for(i in times){
		
	setTxtProgressBar(pb, i)
	
	x <- .Fortran(as.character(name),
		positions = as.double(positions[,1:2]),
		np = as.integer(np),
		nt = as.integer(5),
		dt = as.double(dt),
		Dx = as.double(D[1]),
		Dy = as.double(D[2]),
		U0 = as.double(U0),
		b = as.double(b))

	positions[,1:2] <- matrix(x$positions,ncol=2)
	
	var[which(times == i)] <- var(positions[,1])
	skew[which(times == i)] <- skewness(positions[,1])
	
	if(movie)
		plotFun(plotType)
}
close(pb)
if(movie) dev.off()

	# Variance growth plot
pdf(paste(name,'_variance.pdf',sep=''))
	layout(rbind(1,2))
	plot(times,var,xlab='Time',ylab='Streamwise Variace',cex=.5,type='l')
	plot(times,skew,xlab='Time',ylab='Streamwise Skewness',cex=.5,type='l')
dev.off()

if(movie){
	makePdf2SwfMovie(name)
}