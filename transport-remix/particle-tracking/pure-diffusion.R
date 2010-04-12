#!/usr/bin/env Rscript

# Pure diffusion particle tracking 

name <- 'pure_diffusion'
nd <- 2        # dimensions
np <- 500000    # particles
nv <- 0        # vortexes
nt <- 200     # timesteps
dt <- .01       # timestep 
nb <- 100

D <- c(.01,.01)  # Diffusion coefficient
nleak <- 0
movieFrameSkip <- 100
maxcnt <- np*.01

movie <- FALSE
slices <- TRUE
plotType <- "hex" # or "hex" 
                  # or "ss" (smooth scatter) 
                  # or "h2d" (2d histogram) 
                  # or "ssi" (smooth scatter image)
                  # or "dc" (plot w/ density colors)

	# plotting boundaries
bnd.max.x <- ceiling(nt*sqrt(2*D[1]*dt)/15)
bnd.max.y <- ceiling(nt*sqrt(2*D[2]*dt)/15)
xlim <- c(-bnd.max.x,bnd.max.x)
ylim <- c(-bnd.max.y,bnd.max.y)
times <- seq(dt,length.out=nt,by=dt)

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

pb <- txtProgressBar(dt,nt*dt,style=3)
for(i in times){
		
	setTxtProgressBar(pb, i)
	
	x <- .Fortran('pure_diffusion',
		positions = as.double(positions[,1:2]),
		np = as.integer(np),
		nt = as.integer(1),
		dt = as.double(dt),
		Dx = as.double(D[1]),
		Dy = as.double(D[2]))

	positions[,1:2] <- matrix(x$positions,ncol=2)
	
	if(movie)
		plotFun(plotType)
	
	
	if(i == times[nt/2]){
		
		if(slices){
			nb <- 35
			pdf(paste(name,'_slices.pdf',sep=''))
				plotSlices(positions,nb)
			dev.off()
		}
	
	}
}
close(pb)

if(movie){
	dev.off()
	#makePdf2SwfMovie(name)
}
