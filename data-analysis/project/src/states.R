source('lib.R')
load('data/ts.Rdata')

m <- 2 #years back
p <- 2 #years ahead, 2 or greater
nsims <- 250
paleo <- meko
paleo.b <- meko.b
hist <- lees
hist.b <- lees.b

n <- length(paleo)
nh <- length(hist)

hist.bmp <- mpbin(hist.b,m,p)
hist.mp <- mpbin(hist,m,p)
paleo.bmp <- mpbin(paleo.b,m,p)

sim <- matrix(NA,nh-m,nsims)

	
for(i in (m+1):nh){
	
	state <- paleo.bmp$x[i,]
	
	is.in <- !logical(nh-m-1)
	for(j in 1:m)
		is.in <- is.in & (hist.bmp$x[,j] == state[j])
	
	hist.prev <- hist.mp$x[is.in,]
	hist.next <- hist.mp$y[is.in,]
	
	K  <- nrow(hist.next)
	W <- 1/(1:K)
	W <-W/sum(W)
	browser()
	
	for(ns in 1:nsims){
		
		
		
	}
	
	
}