#!/usr/bin/env Rscript

# Pure diffusion particle tracking 

nd <- 2        # dimensions
np <- 500000    # particles
nv <- 0        # vortexes
nt <- 20     # timesteps
dt <- .1       # timestep 

D <- c(.1,.1)  # Diffusion coefficient
nleak <- 0
movieFrameSkip <- 100
maxcnt <- np*.01

movie <- TRUE
slices <- TRUE
swfDevice <- FALSE
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

if(swfDevice && movie){
	swf('pure-diffusion.swf',frameRate=5)
}else if(movie){ 
	pdf('pure-diffusion.pdf')
}

pb <- txtProgressBar(1,nt*dt,style=3)
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
	
	if(movie){
		plotFun(plotType)
		if(swfDevice) swfAddPlayerControls(x=xlim[2],y=ylim[1])
	}
	
	if(i == times[nt/2]){
		
		if(slices){
			nb <- 35
			pdf('pure-diffusion-slices.pdf')
				plotSlices(positions,nb)
			dev.off()
		}
	
	}
}
close(pb)
if(movie) dev.off()

if(!swfDevice & movie){
silence <- 
	system('pdf2swf -l -B alternate_simple_viewer.swf pure-diffusion.pdf',
		intern=T)
silence <- 
	system('swfcombine --dummy -r 7 pure-diffusion.swf -o pure-diffusion.swf')
}