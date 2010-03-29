twoDC <- function(x,y,ti,Dx,Dy,M=1,x0=0,y0=0){
    
    p <- expand.grid(x=x,y=y)
    C <- M/(4*pi*sqrt(Dx*Dy)*ti)*(exp(-(p$x-x0)^2/(4*Dx*ti)-(p$y-y0)^2/(4*Dy*ti)))

	C <- matrix(C,ncol=length(x),byrow=T)
	C
}