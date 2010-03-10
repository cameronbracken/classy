#!/usr/bin/env Rscript

# Pure diffusion particle tracking 

nd <- 2        # dimensions
np <- 10000    # particles
nv <- 0        # vortexes
nt <- 2000     # timesteps
dt <- 1        # timestep
D <- .1        # Diffusion coefficient
nleak <- 0
movieFrameSkip <- 50
maxcnt <- np/movieFrameSkip
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
if(plotType == "h2d") {
	s(require(gplots))
	source('my.hist2d.R')
}
if(plotType == "hex"){
	s(require(hexbin))
	source('my.hexbin.s4.R')
}
if(plotType == "ssi"){
	s(require(KernSmooth))
	s(require(fields))
}
options(warn=-1)


positions <- matrix(0,np+nv,nd+1)
positions <- as.data.frame(positions)
names(positions) <- c('x','y','M')

	# color ramp functions 
cr <- colorRampPalette(c(gray(.9),rgb(.7,.7,.7),rgb(0,0,1),rep(rgb(0,1,0),2)))
crw <- colorRampPalette(c('white',gray(.9),rgb(.7,.7,.7),rgb(0,0,1),rep(rgb(0,1,0),2)))

if(swfDevice) swf('pure-diffusion.swf',frameRate=5) else pdf('pure-diffusion.pdf')

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
		
		if(plotType == "hex"){
			
			hb <- hexbin(positions,xbnds=xlim,ybnds=ylim)
			if( !any(hb@count < maxcnt ) ) next
			trans <- function(cnt) sqrt(4*cnt+2)
		    inv   <- function(y) (y^2-2)/4
			#trans <- log2
			#inv <- function(x)2^x
		
			P <- plot(hb,colramp=cr,border=gray(.7),
				maxcnt=maxcnt,trans=trans,inv=inv,mincnt = 2)# not the single ones
			#plot(hb,pen=gray(.7),border=gray(.2),
			#	maxcnt=np/2,trans=trans,inv=inv,style='lattice',maxarea=2)
			grid.text(paste('t = ',round(i*dt,2)),unit(.9,'npc'),unit(.9,'npc'))
			grid.text(paste(round(nleak/np,3),'% mass leakage\n',sep=''),
				unit(.2,'npc'),unit(.9,'npc'))
			
			pushHexport(P$plot.vp)
			
				xy <- hcell2xy(hb)
				if(any(hb@count == 1)) 
					grid.hexagons(hb, style= "centroids", maxcnt = 1, 
						maxarea=0.02, border=gray(.7),pen=gray(.7))
				#grid.text(as.character(hb@count[hb@count != 1]), 
				#	xy$x[hb@count != 1], xy$y[hb@count != 1], 
				#	gp=gpar(cex=0.3, col="red"), default.units="native")
				
			popViewport()
			
		}else if(plotType == "ssi"){
			
			est <- bkde2D(positions[apply(!is.na(positions),1,any),],
				c(diff(xlim)/100,diff(ylim)/100),c(100,100))
			image.plot(est$x1, est$x2, est$fhat,col=crw(64),zlim=c(0,1/(np/50)),xlim=xlim,ylim=ylim)
			
		}else if(plotType == "ss"){
			
			smoothScatter(positions,nrpoints=0,xlim=xlim,ylim=ylim,colramp=crw)
			
		}else if(plotType == "dc"){
			
			col <- densCols(positions,colramp=cr)
			plot(positions$x,positions$y,xlim=xlim,ylim=ylim,col=col,pch=20)
			
		}else if(plotType == "h2d"){
			
			col <- c("white", blues9[-(1:3)])
			rx <- range(positions$x,na.rm = T)
			ry <- range(positions$y,na.rm = T)
			nbins <- round(c(diff(rx)/diff(xlim)*50,diff(ry)/diff(ylim)*50))
			hist2d(positions$x,positions$y, nbins=nbins, 
				col = cr(10), xlim = xlim, #, same.scale=T
				ylim = ylim, zlim=c(0,maxcnt))
			
		}
	
		
		if(swfDevice) swfAddPlayerControls(x=xlim[2],y=ylim[1])
	}
	setTxtProgressBar(pb, i)
}
close(pb)
dev.off()

if(!swfDevice){
	silence <- system('pdf2swf -l -B alternate_simple_viewer.swf pure-diffusion.pdf',intern=T)
	silence <- system('swfcombine --dummy -r 7 pure-diffusion.swf -o pure-diffusion.swf')
}