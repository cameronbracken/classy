twoDC <- function(x,y,ti,D,M=1,x0=0,y0=0){
    
    p <- expand.grid(x=x,y=y)
    C <- M/(4*pi*D*ti)*(exp(-(p$x-x0)^2/(4*D*ti)-(p$y-y0)^2/(4*D*ti)))

	C <- matrix(C,ncol=length(x))
	C
}