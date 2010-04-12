#!/usr/bin/env Rscript

# channel shear particle tracking

name <- 'single_vortex'
nd <- 2        # dimensions
np <- 50000    # particles
nv <- 1        # vortexes
nt <- 200      # timesteps
nb <- 150      #number of bins
dt <- 1       # timestep 
nc <- 2      # number of constituent
A <- 1.5       #Vortex rate
B <- 1       # Vortex shape
r <- .1      # Reaction rate
zlim <- 6/10 
tol <- .01
D <- c(0.001,0.001)  #c(0.001,0.001) Diffusion coefficient

vtype <- 'oseen'   #1=forced,2=ideal,3=oseen 
ic <- 'line' #or point or line or blob
track <- FALSE
reaction <- TRUE
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

if(file.exists(imgdir))
	unlink(imgdir,T)
dir.create(imgdir)
if(nc<2)reaction <- FALSE
vtype_f <- switch(vtype, forced = 1, ideal = 2, oseen = 3)
difftype <- ifelse(any(D > 0),'diff','nodiff')

maxcnt <- np*.1


if(nc > 1) plotType <- "twoc"
	# plotting boundaries
bnd.max.x <- 3*B
bnd.max.y <- 3*B
xlim <- c(-bnd.max.x,bnd.max.x)
ylim <- c(-bnd.max.y,bnd.max.y)
times <- seq(dt,length.out=nt,by=dt)

	#required packages for plot types
suppressPackageStartupMessages(require(particleTracking))
source('my.hexbin.s4.R')
options(warn=-1)

masses <- rep(1/np,np)

positions_rt <- positions <- matrix(0,np+nv,nd)
positions <- initVortex(ic,nc=nc)
	
positions_rt[,1] <- sqrt(positions[,1]^2+positions[,2]^2)
	# add 180 degrees if x is negative
#flip <- sign(positions[,1])
#flip[flip > 0] <- 0
#flip[flip < 0] <- pi
positions_rt[,2] <- atan2(positions[,2],positions[,1]) 


colorRamps()

if(movie){ 
	dev.movie <-  if(imgtype =='pdf') pdf(paste(name,'pdf',sep='.'))
	else CairoJPEG(file.path(imgdir,'%03d.jpg'),quality=100)
	plotFun(plotType)
}
if(reaction){
	
	dev.reaction <- if(imgtype =='pdf') pdf(paste(name,'_reaction.pdf',sep=''))
	else CairoJPEG(file.path(imgdir,'%03d_reaction.jpg'),quality=100)
	
}

pb <- txtProgressBar(dt,nt*dt,style=3)
for(i in times){
		
	setTxtProgressBar(pb, i)
	
	z <- .Fortran(as.character(name),
		positions = as.double(positions_rt[,1:2]),
		np = as.integer(np),
		nt = as.integer(1),
		dt = as.double(dt),
		Dx = as.double(D[1]),
		Dy = as.double(D[2]),
		A = as.double(A),
		B = as.double(B),
		type = as.integer(vtype_f))

	#browser()
	#print(matrix(z$positions,ncol=2))
	positions_rt[,1:2] <- matrix(z$positions,ncol=2)	
	
	positions[,1] <- positions_rt[,1]*cos(positions_rt[,2])
	positions[,2] <- positions_rt[,1]*sin(positions_rt[,2])
	
	if(reaction){
		  dev.set(dev.reaction)
			# bin both 
			R <- ash2(bin2(positions,nbin=c(nb,nb)))
			Ra_a <- ash2(bin2(head(positions,np/2),nbin=c(nb,nb)))
			Rb_a <- ash2(bin2(tail(positions,np/2),nbin=c(nb,nb)))
			#inbin <- which.bin(positions,z)
			#for(i in 1:nb){
			#	for(j in 1:nb){
			#		those <- which(inbin[,1]==i & inbin[,2] == j) 
			#		masses[those] <- masses[those] - r*Ra$nc[those]/(np/2)*Rb$nc[those]/(np/2)
			#	}
			#}			
			CaCb <- r * Ra_a$z * Rb_a$z
			image(R$x, R$y, CaCb, col=crr(20),
				xlim=xlim, ylim=ylim, axes=F, xlab='',ylab='')
		
	}
	
	if(movie){
		dev.set(dev.movie)
		plotFun(plotType)
	}
}
close(pb)

if(movie) {
	dev.off()
	mname <- paste(name,vtype,difftype,ic,'.mp4',sep='_')
	system(paste('ffmpeg -r 20 -sameq -y -i img/%03d.jpg',mname))
	system(paste('open ',mname,sep=''))
	#makePdf2SwfMovie(name)
}
if(reaction){
	dev.off()
	mname <- paste(name,vtype,difftype,ic,'reaction.mp4',sep='_')
	system(paste('ffmpeg -r 20 -sameq -y -i img/%03d_reaction.jpg',mname))
	system(paste('open ',mname,sep=''))
}