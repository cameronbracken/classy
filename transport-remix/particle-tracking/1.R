#!/usr/bin/env Rscript

nd <- 2
np <- 10000
nv <- 0
nt <- 1000
dt <- 1
D <- .1
bnd.max <- ceiling(nt*sqrt(2*D*dt)/5)
xbnd <- ybnd <- c(-bnd.max,bnd.max)

require(hexbin)
require(swfDevice)
options(warn=-1)

positions <- matrix(0,np+nv,nd+1)
positions <- as.data.frame(positions)
names(positions) <- c('x','y','M')

#swf('pure-diffusion.swf',frameRate=5)
pdf('pure-diffusion.pdf')
pb <- txtProgressBar(1,nt,style=3)
for(i in 1:nt){
	for(j in 1:nd)
		positions[1:np,j] <- positions[1:np,j] + rnorm(np)*sqrt(2*D*dt)
	if((i %% 20) == 0 || i == 1){
		hb <- hexbin(positions,xbnds=xbnd,ybnds=ybnd)
		cr <- colorRampPalette(c('gray','blue','green'))

		trans <- function(cnt) sqrt(4*cnt+2)
	    inv   <- function(y) (y^2-2)/4
		#plot(hb,colramp=topo.colors,border=gray(.7),
		#	maxcnt=np/3,trans=log,inv=exp)
		plot(hb,pen='steelblue',border=gray(.2),
			maxcnt=np/2,trans=trans,inv=inv,style='lattice',maxarea=2)
		#print(hexbinplot(y~x,positions,xbnds=xbnd,ybnds=ybnd,xlim=xbnd,
		#	ylim=ybnd,colramp=cr,border=gray(.7),maxcnt=np/5,
		#	trans=log,inv = exp))
		
		#hvp <- hexViewport(hb,xbnds=xbnd,ybnds=ybnd)
		#pushHexport(hvp)
		#	grid.hexagons(hb,colramp=cr,border=grey(.7),style= "lattice")
		#	grid.hexlegend(ysize=3,legend=1,lcex=1,maxcnt=np/3,
		#		colorcut=seq(0, 1, length = min(17, np/3)))
		#	grid.rect()
		#	grid.xaxis()
		#	grid.yaxis()
			grid.text(paste('t = ',round(i*dt,2)),unit(.9,'npc'),unit(.9,'npc'))
		#popViewport()
		#grid.newpage()
	}
	setTxtProgressBar(pb, i)
}
close(pb)
dev.off()
