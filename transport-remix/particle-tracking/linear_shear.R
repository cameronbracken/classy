#!/usr/bin/env Rscript

# linear shear particle tracking

name <- 'linear_shear'
nd <- 2        # dimensions
np <- 50000    # particles
nv <- 0        # vortexes
nt <- 200     # timesteps
dt <- .1       # timestep 
Us <- 10

D <- c(.1,.1)  # Diffusion coefficient
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
bnd.max.x <- ceiling(nt*sqrt(2*D[1]*dt)+Us*dt*nt*2)
bnd.max.y <- ceiling(nt*sqrt(2*D[2]*dt))
xlim <- c(-bnd.max.x,bnd.max.x)
ylim <- c(-bnd.max.y,bnd.max.y)
times <- seq(dt,length.out=nt,by=dt)
var <- numeric(length(times))

	#required packages for plot types
suppressPackageStartupMessages(require(particleTracking))
source('my.hexbin.s4.R')
options(warn=-1)


positions <- matrix(0,np+nv,nd+1)

colorRamps()

if(movie){ 
	pdf(paste(name,'pdf',sep='.'))
	plotFun(plotType)
}

pb <- txtProgressBar(1,nt*dt,style=3)
for(i in times){
		
	setTxtProgressBar(pb, i)
	
	x <- .Fortran(as.character(name),
		positions = as.double(positions[,1:2]),
		np = as.integer(np),
		nt = as.integer(5),
		dt = as.double(dt),
		Dx = as.double(D[1]),
		Dy = as.double(D[2]),
		Us = as.double(Us))

	positions[,1:2] <- matrix(x$positions,ncol=2)
	
	var[which(times == i)] <- var(positions[,1])
	
	if(movie)
		plotFun(plotType)
}
close(pb)
if(movie) dev.off()

	# Variance growth plot
pdf(paste(name,'_variance.pdf',sep=''),height=4)
	plot(times,var,xlab='Time',ylab='Streamwise Variace',cex=.5)
	lines(times,2/3*Us^2*D[1]*times^3,col=2)
	legend('topleft',c('Simulated','Analytical'),pch=c(1,-1),lty=c(0,1),col=c(1,2))
dev.off()

if(movie){
	makePdf2SwfMovie(name)
}