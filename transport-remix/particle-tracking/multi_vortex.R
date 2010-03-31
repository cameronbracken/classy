#!/usr/bin/env Rscript

# multi vortex particle tracking

name <- 'multi_vortex'
nd <- 2        # dimensions
np <- 2000    # particles
nv <- 5        # vortexes
nt <- 200      # timesteps
dt <- .01       # timestep 
A <- 1       #Vortex scale
B <- 1       # Vortex shape
vtype <- 3   #1=forced,2=ideal,3=oseen 
ic <- 'line' #or point or line
track <- TRUE
ntrack <- 6

D <- c(0.00,0.00)  #c(0.001,0.001) Diffusion coefficient
movieFrameSkip <- 100
maxcnt <- np*.1

movie <- TRUE
plotType <- "dc" # or "hex" 
                  # or "ss" (smooth scatter) 
                  # or "h2d" (2d histogram) 
                  # or "ssi" (smooth scatter image)
                  # or "dc" (plot w/ density colors)

if(track) nt <- 20000
	# plotting boundaries
bnd.max.x <- 4*A
bnd.max.y <- 4*A
xlim <- c(-bnd.max.x,bnd.max.x)
ylim <- c(-bnd.max.y,bnd.max.y)
times <- seq(dt,length.out=nt,by=dt)
if(track){
	paths <- array(NA, c(ntrack,2,length(times)))
	vorts <- array(NA, c(nv,2,length(times)))
	movie <- F
	np <- ntrack
}

	#required packages for plot types
suppressPackageStartupMessages(require(particleTracking))
source('my.hexbin.s4.R')
options(warn=-1)


positions <- matrix(0,np+nv,nd+1)
positions[,1] <- initVortex(ic)
positions[,2] <- initVortex(ic)
positions[1:np,2] <- 0


colorRamps()

if(movie){ 
	pdf(paste(name,'pdf',sep='.'))
	plotFun(plotType)
	points(positions[(np+1):(np+nv),1:2],pch=19)
}

pb <- txtProgressBar(dt,nt*dt,style=3)
for(i in times){
		
	setTxtProgressBar(pb, i)
	
	x <- .Fortran(as.character(name),
		positions = as.double(positions[,1:2]),
		np = as.integer(np),
		nt = as.integer(25),
		nv = as.integer(nv),
		dt = as.double(dt),
		Dx = as.double(D[1]),
		Dy = as.double(D[2]),
		A = as.double(A),
		B = as.double(B),
		type = as.integer(vtype))

	positions[,1:2] <- matrix(x$positions,ncol=2)
	
	if(track){
		paths[,,which(i==times)] <- positions[1:ntrack,1:2]
		vorts[,,which(i==times)] <- positions[(ntrack+1):(ntrack+nv),1:2]
	}
		
	
	
	if(movie){
		plotFun(plotType)
		points(positions[(np+1):(np+nv),1:2],pch=19)
	}
}
close(pb)

umag <- ux <- uy <- matrix(NA,length(times),ntrack)
for(i in 1:ntrack){
	ux[,i] <- (paths[i,1,] - mylag(paths[i,1,],1))/dt
	uy[,i] <- (paths[i,2,] - mylag(paths[i,2,],1))/dt
	umag[,i] <- sqrt(ux[,i]^2+uy[,i]^2)
}

pdf('chaotic-ts.pdf',height=5,width=8)
	plot(times,paths[1,1,],type='l',xlab='X')
	lines(times,paths[2,1,],col=2)
	lines(times,paths[3,1,],col=3)
dev.off()


if(movie) {
	dev.off()
	#makePdf2SwfMovie(name)
}