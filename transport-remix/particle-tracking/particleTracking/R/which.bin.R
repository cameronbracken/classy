which.bin <- function(x,bins){
	a <- ash2(bins)
	xbr <- a$x - diff(a$x)[1]/2
	ybr <- a$y - diff(a$y)[1]/2
	xb <- floor(((length(xbr) - 1) * (x[,1] - min(xbr)))/diff(range(xbr))+1)
	yb <- floor(((length(ybr) - 1) * (x[,2] - min(ybr)))/diff(range(ybr))+1)
	return(cbind(yb,xb))
}