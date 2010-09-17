constriction <- function(x,delta0,d0,sub=TRUE,crit=FALSE){
	d <- d_star <- B <- d_c <- E_star <- q <- numeric(length(x))
	k <- ifelse(sub,0,4*pi/3)
	E <- d0 + Q^2/(B_0^2*2*g*d0^2)
	print(E)

	for( i in seq_along(x) ){
	
		B[i] <- B_0 - 2*delta(x[i],delta0)
		last.d <- ifelse(i==1,d0,d[i-1])
		q[i] <- Q/B[i]
	
		d_c[i] <- (q[i]^2/g)^(1/3)
	
		E_star[i] <- E/d_c[i]
		nextd <- d_star(E_star[i],k)*d_c[i]
		if(crit)
			if(sub){
				nextd <- d_star(E_star[i],k)*d_c[i]
				if(nextd - last.d > 0)
					k <- 4*pi/3
			}else{
				if(nextd - last.d < 0 & x[i] >=.5)
					k <- 0
			}	
		
		d_star[i] <- d_star(E_star[i],k)
		d[i] <- d_star[i]*d_c[i]
	
	}
	return(list(x=x,d=d,dc=d_c,Es=E_star,ds=d_star))
}

d_star <- function(Es,k){
	
	gamma <- acos(1-27/4*Es^(-3))
	Es/3*(1+2*cos(gamma/3+k))
	
}

delta <- function(x,delta0,L=1)
	delta0/2*(1-cos(2*pi*x/L))
	
	
# Dimensional specific energy 
SEcurves <- function(xp,d,delta0){

	E <- matrix(NA,nrow=length(xp),ncol=length(d))
	for(i in seq_along(xp)){
		B <- B_0 - 2*delta(xp[i],delta0)
		q <- Q/B
		E[i,] <- d + q^2/(2*g*d^2)
	}
	E
}	

delta_crit <- function(d_0){
	E1 <- d_0+Q^2/(B_0^2*2*g*d_0^2)
	Bc <- (3/2)^(3/2)*Q/sqrt(g*E1^3)
	delta_0_crit <- (B_0-Bc)/(1-cos(pi))
	delta_0_crit
}


AwesomePlots <- function(obj,delta0,xp,p,E, main, np=1000){
	
	d_plot <- seq(0,1,,np)
	E_plot <- SEcurves(xp,d_plot,delta0)

	quartz(height=4)
	layout(matrix(c(1,2),1))
	
	###  Flow Depth 
	plot(obj$x,obj$d,ylim=c(0,.3),type='l',col='blue', xlab='$x$', ylab= '$d$',
		main=main)
	abline(h=0,lty='dashed')
	
	# dimensional Specific energy 
	for(i in seq_along(xp)){
		if(i == 1){
			r <- c(0,.3)
			plot(E_plot[i,],d_plot,ylim=r,xlim=c(0,.3),type='l',xlab='E',
				ylab='d', main = main)
			abline(b=1,a=0)
			abline(v=E,lty='dashed')
		}else{
			lines(E_plot[i,],d_plot)
		}
	}
	points(obj$Es[p]*obj$dc[p],obj$d[p])
	
	# Non-dimensional specific Energy
	quartz(width=3.5,height=4)
	d_plot <- seq(0,4,,np)
	E_plot <- d_plot + 1/(2*d_plot^2)
	plot(E_plot,d_plot,ylim=c(0,3),xlim=c(0,4),type='l',xlab='$E^*$',
		ylab='$d^*$', main = main)
	abline(b=1,a=0)
	points(obj$Es[p],obj$ds[p])
	
	
}