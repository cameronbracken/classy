#!/usr/bin/env Rscript

# channel shear particle tracking

name <- 'single_vortex'
nd <- 2        # dimensions
np <- 2000    # particles
nv <- 0        # vortexes
nt <- 200      # timesteps
dt <- .1       # timestep 
A <- 1       #Vortex scale
B <- 1       # Vortex shape
vtype <- 3   #1=forced,2=ideal,3=oseen 
ic <- 'line' #or point or line or blob

D <- c(0.001,0.001)  #c(0.001,0.001) Diffusion coefficient
movieFrameSkip <- 100
maxcnt <- np*.1

movie <- TRUE
plotType <- "dc" # or "hex" 
                  # or "ss" (smooth scatter) 
                  # or "h2d" (2d histogram) 
                  # or "ssi" (smooth scatter image)
                  # or "dc" (plot w/ density colors)

	# plotting boundaries
bnd.max.x <- 2*A
bnd.max.y <- 2*A
xlim <- c(-bnd.max.x,bnd.max.x)
ylim <- c(-bnd.max.y,bnd.max.y)
times <- seq(dt,length.out=nt,by=dt)

	#required packages for plot types
suppressPackageStartupMessages(require(particleTracking))
source('my.hexbin.s4.R')
options(warn=-1)


positions_rt <- positions <- matrix(0,np+nv,nd+1)
positions_rt[,1] <- initVortex(ic)
if(ic == 'blob') positions_rt[,2] <- initVortex(ic)
	
positions[,1] <- positions_rt[,1]*cos(positions_rt[,2])
positions[,2] <- positions_rt[,1]*sin(positions_rt[,2])

colorRamps()

if(movie){ 
	pdf(paste(name,'pdf',sep='.'))
	plotFun(plotType)
}

pb <- txtProgressBar(dt,nt*dt,style=3)
for(i in times){
		
	setTxtProgressBar(pb, i)
	
	x <- .Fortran(as.character(name),
		positions = as.double(positions_rt[,1:2]),
		np = as.integer(np),
		nt = as.integer(1),
		dt = as.double(dt),
		Dx = as.double(D[1]),
		Dy = as.double(D[2]),
		A = as.double(A),
		B = as.double(B),
		type = as.integer(vtype))

	positions_rt[,1:2] <- matrix(x$positions,ncol=2)
	
	positions[,1] <- positions_rt[,1]*cos(positions_rt[,2])
	positions[,2] <- positions_rt[,1]*sin(positions_rt[,2])
	
	if(movie)
		plotFun(plotType)
}
close(pb)

if(movie) {
	dev.off()
	#makePdf2SwfMovie(name)
}