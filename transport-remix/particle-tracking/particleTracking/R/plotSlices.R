plotSlices <- function(positions,nb,br=as.integer(round(nb/c(3,2,4.5/3)))){
	
	z <- hist2d(positions[,1:2],nbins=nb,show=FALSE,same.scale=TRUE)
	dx <- diff(z$x)[1]
	dy <- diff(z$y)[1]
	x <- z$x + dx/2
	y <- z$y + dy/2
	xc <- seq(range(x)[1],range(x)[2],,1000)
	yc <- seq(range(y)[1],range(y)[2],,1000)
	px <- py <- integer(0)
	for(j in 1:length(br)){
		px[j] <- which.min(abs(xc - x[br[j]]))
		py[j] <- which.min(abs(yc - y[br[j]]))
	}
	C <- twoDC(xc,yc,i,D[1],D[2])
	
	
		
	plot(x,z$counts[br[2],]/np/(dx*dy),col=2,ylim=c(0,max(C)),
		ylab='Percent of Initial Mass')
	points(x,z$counts[br[1],]/np/(dx*dy),col=3)
	points(x,z$counts[br[3],]/np/(dx*dy),col=4)
	
	lines(xc,C[px[2],],col=2,lwd=3)
	lines(xc,C[px[1],],col=3,lwd=3)
	lines(xc,C[px[3],],col=4,lwd=3)
	
}