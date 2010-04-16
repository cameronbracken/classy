#!/usr/bin/env Rscript

# channel shear particle tracking

name <- 'channel_shear'
nd <- 2        # dimensions
np <- 100000    # particles
nv <- 0        # vortexes
nt <- 5000      # timesteps
dt <- 1       # timestep 
U0 <- 10        # Bulk velocity
b <- .5        # half channel width
Uy <- 2/3*U0   #mean vertical velocity
nb <- 200
tol <- .01

D <- c(0.001,0.001)  # Diffusion coefficient
nleak <- 0
movieFrameSkip <- 100
maxcnt <- np*.01
zlim <- NA

movie <- FALSE
imgtype <- 'jpg'
imgdir <- 'img'
plotType <- "h2d"  # or "hex" 
                  # or "ss" (smooth scatter) 
                  # or "h2d" (2d histogram) 
                  # or "ssi" (smooth scatter image)
                  # or "dc" (plot w/ density colors)
				  # or "line"
				  # or 'points'

if(file.exists(imgdir) & movie)
	unlink(imgdir,T)
dir.create(imgdir)

	# plotting boundaries
bnd.max.x <- U0*dt*nt/2+nt*sqrt(2*(D[1]+D[2])*dt)
bnd.max.y <- b
xlim <- c(-bnd.max.x*2,.5*bnd.max.x)
ylim <- c(-bnd.max.y,bnd.max.y)
times <- seq(dt,length.out=nt,by=dt)
var <- skew <- numeric(length(times))

	#required packages for plot types
suppressPackageStartupMessages(require(particleTracking))
source('my.hexbin.s4.R')
options(warn=-1)


positions <- matrix(0,np,nd)
positions[,2] <- seq(-b,b,length.out=nrow(positions))


colorRamps()

if(movie){ 
	dev.movie <-  if(imgtype =='pdf') pdf(paste(name,'pdf',sep='.'))
	else CairoJPEG(file.path(imgdir,'%03d.jpg'),quality=100,height = 240,width=720)
	plotFun(plotType)
}

pb <- txtProgressBar(dt,nt*dt,style=3)
for(i in times){
		
	setTxtProgressBar(pb, i)
	
	x <- .Fortran(as.character(name),
		positions = as.double(positions),
		np = as.integer(np),
		dt = as.double(dt),
		Dx = as.double(D[1]),
		Dy = as.double(D[2]),
		U0 = as.double(U0),
		b = as.double(b))

	positions <- matrix(x$positions,ncol=2)
	
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
	plot(c(0,times),c(0,var),xlab='Time',ylab='Streamwise Variace',cex=.5,type='l')
	plot(c(0,times),c(0,-skew),xlab='Time',ylab='Streamwise Skewness',cex=.5,type='l')
dev.off()

if(movie){
	mname <- paste(name,'mp4',sep='.')
	system(paste('ffmpeg -r 20 -sameq -y -i img/%03d.jpg',mname))
	system(paste('open ',mname,sep=''))
	#makePdf2SwfMovie(name)
}