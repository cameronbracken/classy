plotFun <- function(type){
	
	if(plotType == "hex"){
		
		out <- 
			(positions[,1] < xlim[1] | 
				positions[,1] > xlim[2]) |
			(positions[,2] < ylim[1] | 
				positions[,2] > ylim[2])
		nleak <- length(which(out))
		
		hb <- hexbin(positions[!out,],xbnds=xlim,ybnds=ylim)
		if( !any(hb@count < maxcnt ) ) return()
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
		image.plot(est$x1, est$x2, est$fhat,col=crw(12),zlim=c(0,1/(np/50)),xlim=xlim,ylim=ylim)
		
	}else if(plotType == "ss"){
		
		smoothScatter(positions,nrpoints=0,xlim=xlim,ylim=ylim,colramp=crw)
		
	}else if(plotType == "dc"){
		
		col <- densCols(positions,colramp=cr)
		plot(positions[,1],positions[,2],xlim=xlim,ylim=ylim,col=col,pch=20)
		
	}else if(plotType == "h2d"){
		
		col <- c("white", blues9[-(1:3)])
		rx <- range(positions[,1],na.rm = T)
		ry <- range(positions[,2],na.rm = T)
		nbins <- round(c(diff(rx)/diff(xlim)*50,diff(ry)/diff(ylim)*50))
		hist2d(positions[,1],positions[,1], nbins=nbins, 
			col = cr(10), xlim = xlim, #, same.scale=T
			ylim = ylim, zlim=c(0,maxcnt))
		
	}
	
	
	
}