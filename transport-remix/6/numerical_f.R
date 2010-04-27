numerical <- function(nip,xl,xr,ll,ul,C0,tmax,D,U0,P,dt=NULL){
	
	Bl <- Br <- 0
	dx <- (xr - xl)/(nip + 1)
	dt <- min(c(dx^2*.25/(2*(D*2)),1))
	skip <- 1/dt
	
	u <- function(t) U0 * sin(2*pi*t/P)
	
		#number of interior points
	nip <- (xr - xl)/dx - 1
	
	alpha <- 1/6/dt
	beta <- function(t) u(t)/4/dx
	gamma <- D/2/dx^2
	
	p <- function(t) alpha - beta(t) - gamma
	q <- function(t) 4*alpha + 2*beta(t)
	r <- function(t) alpha + beta(t) + gamma
	L <- function(t) alpha + beta(t) - gamma
	M <- function(t) 4*alpha - 2*beta(t)
	N <- function(t) alpha - beta(t) + gamma
	
	t <- seq(0,tmax,dt)
	x <- seq(dx,(xr-xl)-dx,dx)
	nt <- length(t)
	
		#Initial conditions
	C <- matrix(0,nrow=nt/skip+2,ncol=nip)
	#ic1 <- which.min(abs(x-ll))
	#ic2 <- which.min(abs(x-ul))
	#C[1,ic1:ic2] <- C0
	C[1,] <- Cold <- Cnew <- analytical(x,20,ll,ul,D,C0,U0,P)
	
	A <- B <- matrix(0,nip,nip)
	f <- numeric(nip)
	g <- 2:nip
	h <- 1:(nip-1)
	
	s <- 0
	for(n in 1:(nt-1)){
		if(n%%skip==0)plot(Cnew,type='l')
		
		A[,] <- B[,] <- f[] <- 0
		
		diag(A) <- q(t[n])
		diag(B) <- M(t[n])
		
		pn <- p(t[n])
		rn <- r(t[n])
		Ln <- L(t[n])
		Nn <- N(t[n])
		
		for(i in 1:(nip-1)){
			A[i+1,i] <- pn
			A[i,i+1] <- rn
			B[i+1,i] <- Ln
			B[i,i+1] <- Nn
		}
			#lower diagonal
		#A[g,h] <- p(t[n])
		#B[g,h] <- L(t[n])
			#diagonal
			#upper diagonal
		#A[h,g] <- r(t[n])
		#B[h,g] <- N(t[n])
		
			#junk
		f[1] <- Bl*(L(t[n])-p(t[n]))
		f[nip] <- Br*(N(t[n])-r(t[n]))
		
		#browser()
		Cold <- as.vector(Cnew)
		Cnew <- solve(A) %*% t((as.vector(Cold) %*% B) + f)
		if(n%%skip==0){
			s <- s + 1
			C[s+1,] <- Cnew
		}
	}
	
	
}