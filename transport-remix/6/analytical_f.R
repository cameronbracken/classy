analytical <- function(x,t,ll,ul,D,C0,U0,P){
	n <- length(t)
	xt <- expand.grid(x,t)
	x <- xt[,1]
	t <- xt[,2]
    C <- -C0/2*(erf(((x-ul)-U0*sin(2*pi*t/P))/sqrt(4*D*t))-
				erf(((x-ll)+U0*sin(2*pi*t/P))/sqrt(4*D*t)))
    C <- matrix(C,n,byrow=T)
    C
}
