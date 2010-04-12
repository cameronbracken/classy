plotFun <- function(type){
	
	if(plotType == "hex"){
		
		out <- 
			(positions[,1] < xlim[1] | 
				positions[,1] > xlim[2]) |
			(positions[,2] < ylim[1] | 
				positions[,2] > ylim[2])
		nleak <- length(which(out))
		
		hb <- hexbin(positions[!out,],xbins = nb,xbnds=xlim,ybnds=ylim)
		if( !any(hb@count < maxcnt ) ) return()
		trans <- function(cnt) sqrt(4*cnt+2)
	    inv   <- function(y) (y^2-2)/4
		#trans <- log2
		#inv <- function(x)2^x
	
		P <- plot(hb,colramp=crw,border=gray(.7),xlab='X',ylab='Y',
			maxcnt=maxcnt,trans=trans,inv=inv,mincnt = 1)# not the single ones
		#plot(hb,pen=gray(.7),border=gray(.2),
		#	maxcnt=np/2,trans=trans,inv=inv,style='lattice',maxarea=2)
		grid.text(paste('t = ',round(i*dt,3)),unit(.9,'npc'),unit(.9,'npc'))
		grid.text(paste(round(nleak/np,3),'% mass leakage\n',sep=''),
			unit(.2,'npc'),unit(.9,'npc'))
		
		pushHexport(P$plot.vp)
		
			#Single points
			#xy <- hcell2xy(hb)
			#if(any(hb@count == 1)) 
			#	grid.hexagons(hb, style= "centroids", maxcnt = 1, 
			#		maxarea=0.02, border=gray(.7),pen=gray(.7))
			#counts as text
			#grid.text(as.character(hb@count[hb@count != 1]), 
			#	xy$x[hb@count != 1], xy$y[hb@count != 1], 
			#	gp=gpar(cex=0.3, col="red"), default.units="native")
			
		popViewport()
		
	}else if(plotType == "ssi"){
		
		est <- bkde2D(positions[apply(!is.na(positions),1,any),],
			c(diff(xlim)/50,diff(ylim)/50),c(50,50))
		image.plot(est$x1, est$x2, est$fhat,col=crw(12),
			zlim=c(0,1/(np/200)),xlim=xlim,ylim=ylim)
		
	}else if(plotType == "ss"){
		
		smoothScatter(positions,nrpoints=0,xlim=xlim,ylim=ylim,colramp=crw)
		
	}else if(plotType == "dc"){
		
		col <- densCols(positions,colramp=cr)
		par(mar=rep(1,4))
		plot(positions,xlim=xlim,ylim=ylim,col=col,pch=20,
				xlab='',ylab='',axes=F)
		
	}else if(plotType == "points"){

		col <- 'black'
		par(mar=rep(1,4))
		plot(positions,xlim=xlim,ylim=ylim,col=col,pch=20,
				xlab='',ylab='',axes=F, cex=.6)

	}else if(plotType == "h2d"){
		
		R <- ash2(bin2(positions[1:np,],ab=rbind(xlim,ylim),nbin=c(nb,nb)))
		if(is.na(zlim))
			image(R, col=crr(20), xlim=xlim, ylim=ylim, axes=F, xlab='', 
				ylab='')
		else
			image.plot(R, col=crr(20), xlim=xlim, ylim=ylim, axes=F, xlab='', 
				ylab='', zlim=c(0,zlim))
		
					
	}else if(plotType == "twoc"){
		
		par(mar=rep(1,4))
		plot(head(positions,np/2),xlim=xlim,ylim=ylim,col='red',pch=20,
				xlab='',ylab='',axes=F)
		points(tail(positions,np/2+nv)[1:(np/2),],col='blue',pch=20)
		points(tail(positions,nv),pch=20)
		
	}else if(type == 'line'){
		
		pp <- sparsify(positions)
		plot(pp,axes=F,type='n',xlim=xlim,ylim=ylim,xlab='',ylab='')
		lines(pp)
		if(nv > 1)points(positions[(np+1):(np+nv),1:2],pch=19)
		
	}else if(type == 'poly'){
		
		#pp <- sparsify(positions)
		pp <- positions
		#d <- sqrt(diff(positions[,1])^2 + diff(positions[,2])^2)
        #
		#pp[d>tol,1] <- NA
		#wna <- which(is.na(pp[,1]))
		#if(length(wna)>0){
		#	new.pp <- rbind(pp[1,])
		#	for(p in wna){
		#		yy <- 1
		#		pnext <- 1
		#		while(is.na(pp[p+yy,1])){
		#			yy <- yy + 1
		#			pnext <- yy
		#		}
		#		yy <- 1
		#		plast <- 1 
		#		while(is.na(pp[p-yy,1])){
		#			yy <- yy + 1
		#			plast <- yy
		#		}
		#		new.pp <- rbind(new.pp,pp[1:(p-plast),],
		#			cbind(seq(pp[p-plast,1],pp[p+pnext,1],,10),
		#				seq(pp[p-plast,2],pp[p+pnext,2],,10)),
		#			pp[1:(p+pnext),])
		#	}
		#	pp <- new.pp[!is.na(new.pp[,1]),]
		#}
		
		
		par(mar=c(0,0,0,0))
		plot(pp,axes=F,type='n',xlim=xlim,ylim=ylim,xlab='',ylab='')
		polygon(pp[,1],pp[,2],col='blue',border='black')
		points(pp[(np+1):(np+nv),1:2],pch=19)
		
	}else if(type == 'poly2'){
		
		par(mar=rep(0,4))
		one <- head(positions,na)
		two <- head(tail(positions,nb+nv),nb)
		vor <- tail(positions,nv)
		plot(positions,axes=F,type='n',xlim=xlim,ylim=ylim,xlab='',ylab='')
		polygon(one[,1],one[,2],col='blue',border=NA,lwd=1)
		polygon(two[,1],two[,2],col='red',border=NA,lwd=1)
		points(vor,pch=20)
		
		
	}
	
	
	
}