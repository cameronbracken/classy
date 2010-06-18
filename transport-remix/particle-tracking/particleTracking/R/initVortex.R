initVortex <- function(ic,nc=1,dis = 0){
	
	vp <- numeric(nv)
	if(nv > 0)
		vp <- matrix(rnorm(nv*2,sd=B,mean=0),nv,2)
		
	z <- ifelse(track,.001,B)
	if(ic == "line"){
		
		p <- 
		if(nc>1)
				cbind(c(
					seq(-z,-z/2,length.out=np/2),
					seq(z/2,z,length.out=np/2)), 
					c(seq(-z,-z/2,length.out=np/2),
						seq(z/2,z,length.out=np/2)))
			else 
				cbind(seq(-z,z,length.out=np),rep(0,np))
		
	}else if(ic == 'blob'){
		
		p <- 
		if(nc>1)
			cbind(
				c(rnorm(np/2,sd=z/10,mean=z/2),rnorm(np/2,sd=z/10,mean=-z/2)),
				c(rnorm(np/2,sd=z/10,mean=z/2),rnorm(np/2,sd=z/10,mean=-z/2))
			)
		else
			cbind(rnorm(np,sd=z/10,mean=z/2),rnorm(np,sd=z/10,mean=z/2))
		
	}else if(ic == 'point'){
		
		p <- 
		if(nc>1)
			cbind(
				c(rep(z/2,np/2),rep(-z/2,np/2)),
				c(rep(z/2,np/2),rep(-z/2,np/2)))
		else
			cbind(rep(z/2,np),rep(z/2,np))
		
	}else if(ic == 'polys'){
		sep <- 2*B
		p <- 
		if(nc>1)
			rbind(
				cbind(c(
			 		seq(-sep*z,-dis,,np/8),
				 	seq(-dis,-dis,,np/8),
				 	seq(-dis,-sep*z,,np/8),
				 	seq(-sep*z,-sep*z,,np/8)),
				 	c(seq(-z,-z,,np/8),
				 	seq(-z,z,,np/8),
				 	seq(z,z,,np/8),
				 	seq(z,-z,,np/8))),
				cbind(c(
				 	seq(sep*z,dis,,np/8),
				 	seq(dis,dis,,np/8),
				 	seq(dis,sep*z,,np/8),
				 	seq(sep*z,sep*z,,np/8)),
				 	c(seq(-z,-z,,np/8),
				 	seq(-z,z,,np/8),
				 	seq(z,z,,np/8),
				 	seq(z,-z,,np/8))))
		else
			cbind(c(
			 	seq(-z,z,,np/4),
			 	seq(z,z,,np/4),
			 	seq(z,-z,,np/4),
			 	seq(-z,-z,,np/4)),
			 	c(seq(-z,-z,,np/4),
			 	seq(-z,z,,np/4),
			 	seq(z,z,,np/4),
			 	seq(z,-z,,np/4)))

		}else if(ic == 'polys4'){
			sep <- 0
			p <- 
			if(nc>1)
				cbind(
					c(seq(-z,-dis,,np/16),seq(-dis,-dis,,np/16),seq(-dis,-z,,np/16),seq(-z,-z,,np/16),
				 	  seq(-z,-dis,,np/16),seq(-dis,-dis,,np/16),seq(-dis,-z,,np/16),seq(-z,-z,,np/16),
					  seq(dis,z,,np/16),seq(z,z,,np/16),seq(z,dis,,np/16),seq(dis,dis,,np/16),
					  seq(dis,z,,np/16),seq(z,z,,np/16),seq(z,dis,,np/16),seq(dis,dis,,np/16)),
					c(seq(dis,dis,,np/16),seq(dis,z,,np/16),seq(z,z,,np/16),seq(z,dis,,np/16),
					  seq(-dis,-dis,,np/16),seq(-dis,-z,,np/16),seq(-z,-z,,np/16),seq(-z,-dis,,np/16),
					  seq(dis,dis,,np/16),seq(dis,z,,np/16),seq(z,z,,np/16),seq(z,dis,,np/16),
					  seq(-z,-z,,np/16),seq(-z,-dis,,np/16),seq(-dis,-dis,,np/16),seq(-dis,-z,,np/16)))
			else
				cbind(c(
				 	seq(-z,z,,np/4),
				 	seq(z,z,,np/4),
				 	seq(z,-z,,np/4),
				 	seq(-z,-z,,np/4)),
				 	c(seq(-z,-z,,np/4),
				 	seq(-z,z,,np/4),
				 	seq(z,z,,np/4),
				 	seq(z,-z,,np/4)))
				

			}else if(ic == 'polyc'){

			t <- if(nc>1)
				c(seq(0,2*pi,,np/2),seq(0,2*pi,,np/2))
			else 
				seq(0,2*pi,,np)
				
			p <- 
			if(nc>1)
				rbind(
					cbind(z*cos(head(t,np/2))+2*z,z*sin(head(t,np/2))),
					cbind(z*cos(tail(t,np/2))-2*z,z*sin(tail(t,np/2))))
			else
				cbind(z*cos(t),c(z*sin(t)))

		}
	
	return(rbind(p,vp))
}