#!/usr/bin/env Rscript

# Pure diffusion particle tracking 

nd <- 2        # dimensions
np <- 10000    # particles
nv <- 0        # vortexes
nt <- 2000     # timesteps
dt <- 1        # timestep
D <- .1        # Diffusion coefficient
nleak <- 0
movieFrameSkip <- 100
maxcnt <- np/movieFrameSkip
movie <- FALSE
swfDevice <- FALSE
plotType <- "hex" # or "hex" 
                  # or "ss" (smooth scatter) 
                  # or "h2d" (2d histogram) 
                  # or "ssi" (smooth scatter image)
                  # or "dc" (plot w/ density colors)

	# plotting boundaries
bnd.max <- ceiling(nt*sqrt(2*D*dt)/15)
xlim <- ylim <- c(-bnd.max,bnd.max)

	#required packages for plot types
s <- suppressPackageStartupMessages
s(require(swfDevice))
s(require(RColorBrewer))
s(require(gplots))
source('my.hist2d.R')
source('twoDC.R')

if(plotType == "hex"){
	s(require(hexbin))
	source('my.hexbin.s4.R')
}
if(plotType == "ssi"){
	s(require(KernSmooth))
	s(require(fields))
}
source('plot-fun.R')
options(warn=-1)


positions <- matrix(0,np+nv,nd+1)
positions <- as.data.frame(positions)
names(positions) <- c('x','y','M')

	# color ramp functions 
cr <- colorRampPalette(c(gray(.9),rgb(.7,.7,.7),rgb(0,0,1),rep(rgb(0,1,0),2)))
crw <- colorRampPalette(c('white',gray(.9),rgb(.7,.7,.7),rgb(0,0,1),rep(rgb(0,1,0),2)))

if(swfDevice && movie) swf('pure-diffusion.swf',frameRate=5) else pdf('pure-diffusion.pdf')

pb <- txtProgressBar(1,nt,style=3)
for(i in 1:nt){
	for(j in 1:nd)
		positions[1:(np-nleak),j] <- positions[1:(np-nleak),j] + rnorm(np-nleak)*sqrt(2*D*dt)
	
	if((i %% movieFrameSkip) == 0 || i == 1 ){
		
		out <- 
			(positions[1:(np-nleak),1] < xlim[1] | positions[1:(np-nleak),1] > xlim[2]) |
			(positions[1:(np-nleak),2] < ylim[1] | positions[1:(np-nleak),2] > ylim[2])
		nleak <- nleak + length(which(out))
		
		positions <- positions[1:(np-nleak),][!out,]
		
		if(movie){
			plotFun('hex')
			if(swfDevice) swfAddPlayerControls(x=xlim[2],y=ylim[1])
		}
	}
	if(i == nt/2){
		
		nb <- 50
		x <- seq(xlim[1],xlim[2],,nb)
		y <- seq(ylim[1],ylim[2],,nb)

		C <- twoDC(x,y,i*dt,D)
		
		pdf('pure-diffusion-slices.pdf')
			z <- hist2d(positions[,1:2],nbins=nb,show=FALSE)
			at <- barplot(z$counts[c(25,15,35),]/np,beside=T,col=2:4,border='transparent')
			lines(at[1,],C[,25],col=2,lwd=3)
			lines(at[2,],C[,15],col=3,lwd=3)
			lines(at[3,],C[,35],col=4,lwd=3)
		dev.off()
		
		
	}
	setTxtProgressBar(pb, i)
}
close(pb)
if(movie) dev.off()

if(!swfDevice & movie){
	silence <- system('pdf2swf -l -B alternate_simple_viewer.swf pure-diffusion.pdf',intern=T)
	silence <- system('swfcombine --dummy -r 7 pure-diffusion.swf -o pure-diffusion.swf')
}