#!/usr/bin/env Rscript

# multi vortex particle tracking

name <- 'multi_vortex'
nd <- 2        # dimensions
np <- 10*8    # particles
nv <- 8        # vortexes
nt <- 300      # timesteps
nc <- 2        # number of constituents
nb <- 100
r <- .015
dt <- .1       # timestep 
A <- 1       #Vortex scale
B <- 1       # Vortex shape
tol <- .01   # tolerance for plotting points
vtype <- "oseen"   #1=forced,2=ideal,3=oseen 
ic <- 'polys' #or point or line or blob or polys or polyc
track <- FALSE
ntrack <- 6
nadd <- 3500 # max number of points to add in a single iteration
reaction <- FALSE
movie <- TRUE
imgtype <- 'pdf'
imgdir <- 'img'

D <- c(0.00,0.00)  #c(0.001,0.001) Diffusion coefficient
movieFrameSkip <- 100
maxcnt <- np*.1

plotType <- "poly2" # or "hex" 
                  # or "ss" (smooth scatter) 
                  # or "h2d" (2d histogram) 
                  # or "ssi" (smooth scatter image)
                  # or "dc" (plot w/ density colors)
				  # or 'twoc'

if(file.exists(imgdir) & movie)
	unlink(imgdir,T)
dir.create(imgdir)
if(nc<2)reaction <- FALSE
na <- nb <- np/2
n1 <- n2 <- n3 <- n4 <- np/4
vtype_f <- switch(vtype, forced = 1, ideal = 2, oseen = 3)
difftype <- ifelse(any(D > 0),'diff','nodiff')

if(track) nt <- 20000
#if(nc > 1) plotType <- "twoc"
	# plotting boundaries
bnd.max.x <- 5*B
bnd.max.y <- 5*B
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
positions <- initVortex(ic,nc)
#positions[np+1,] <- c(0,0)
masses <- rep(1/(np/2),np)
ilen <- numeric(length(times))
vs <- -1*rbinom(nv,1,.5)
vs[vs == 0] <- 1
vs <- rep(1,length(vs))

colorRamps()
if(movie){ 
	dev.movie <-  if(imgtype =='pdf') pdf(paste(name,'pdf',sep='.'),onefile=T)
	else CairoJPEG(file.path(imgdir,'%03d.jpg'),quality=100)
	plotFun(plotType)
}
if(reaction){
	
	dev.reaction <- if(imgtype =='pdf') pdf(paste(name,'_reaction.pdf',sep=''))
	else CairoJPEG(file.path(imgdir,'%03d_reaction.jpg'),quality=100)
	
}

#pb <- txtProgressBar(dt,nt*dt,style=3)
for(i in times){
		
	#setTxtProgressBar(pb, i)
	
	x <- .Fortran(as.character(name),
		positions = as.double(positions[,1:2]),
		np = as.integer(np),
		nt = as.integer(1),
		nv = as.integer(nv),
		vs = as.double(vs),
		dt = as.double(dt),
		Dx = as.double(D[1]),
		Dy = as.double(D[2]),
		A = as.double(A),
		B = as.double(B),
		type = as.integer(vtype_f))
	
		
	positions[,1:2] <- matrix(x$positions,ncol=2)
	ilen[which(i==times)] <- sum(sqrt(diff(positions[,1])^2 + diff(positions[,2])^2))
	
	oldnp <- np
	if(nc > 1){
		
		ti <- system.time({
			if(nc == 2){
				
				one <- sparsify(head(positions,na))
				na <- nrow(one)
				two <- sparsify(head(tail(positions,nb+nv),nb))
				nb <- nrow(two)
				
				vor <- tail(positions,nv)
				#browser()
				positions <- rbind(one,two,vor)
				
				np <- na + nb
				
			}else if(nc == 4){
				
				one <- sparsify(head(positions,n1))
				two <- sparsify(positions[(n1+1):(n1+n2),])
				three <- sparsify(positions[(n1+n2+1):(n1+n2+n3),])
				four <- sparsify(positions[(n1+n2+n3+1):(n1+n2+n3+n4),])
				
				n1 <- nrow(one)
				n2 <- nrow(two)
				n3 <- nrow(three)
				n4 <- nrow(four)
				
				vor <- tail(positions,nv)
				#browser()
				positions <- rbind(one,two,three,four,vor)
				
				np <- n1 + n2 + n3 + n4
			}
		})
		
	}else{

		positions <- rbind(sparsify(head(positions,np)),tail(positions,nv))
		np <- nrow(positions) - nv	
	}
	
	if(track){
		paths[,,which(i==times)] <- positions[1:ntrack,1:2]
		vorts[,,which(i==times)] <- positions[(ntrack+1):(ntrack+nv),1:2]
	}
		
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
		if(imgtype != 'pdf')dev.set(dev.movie)
		plotFun(plotType)
	}
	cat('Using',sprintf('%6d',np),'points took',sprintf('%4.1f',ti[['elapsed']]),
		'seconds,',round(i/(dt*nt)*100,0),'\b% done\n')
	if((np-oldnp) >nadd) break
	if(ti[['elapsed']] > 300) break
}
#close(pb)

if(track){
	umag <- ux <- uy <- matrix(NA,length(times),ntrack)
	for(i in 1:ntrack){
		ux[,i] <- (paths[i,1,] - mylag(paths[i,1,],1))/dt
		uy[,i] <- (paths[i,2,] - mylag(paths[i,2,],1))/dt
		umag[,i] <- sqrt(ux[,i]^2+uy[,i]^2)
	}

	pdf('chaotic-ts.pdf',height=5,width=8)
		plot(times,paths[1,1,],type='l',ylab='X', xlab='Time')
		lines(times,paths[2,1,],col=2)
		#lines(times,paths[3,1,],col=3)
	dev.off()
}

pdf('interface_length.pdf',height=5,width=8)
	plot(times,ilen,type='l',xlab='time',ylab='Interface Length')
dev.off()

if(movie) {
	dev.off()
	mname <- paste(name,vtype,difftype,ic,'.mp4',sep='_')
	system(paste('ffmpeg -r 20 -sameq -y -i img/%03d.jpg',mname))
	print(np)
	#system(paste('open ',mname,sep=''))
	#makePdf2SwfMovie(name)
}
if(reaction){
	dev.off()
	mname <- paste(name,vtype,difftype,ic,'reaction.mp4',sep='_')
	system(paste('ffmpeg -r 20 -sameq -y -i img/%03d_reaction.jpg',mname))
	#system(paste('open ',mname,sep=''))
}