ts.annual.mean <- 
function(x){

    options(warn=-1)
    syear <- start(x)[1]
	sper <- start(x)[2]
	eyear <- end(x)[1]
	eper <- end(x)[2]
    nst <- numeric(nyears)
	
	#discard incomplete years at beginning or end of series
	if(sper != 1) {
		syear <- syear + 1
		sper <- 1
	}
	if(eper != frequency(x)){
		eyear <- eyear - 1
		eper <- frequency(x)
	}
	nyears <- length(unique(floor(time(x))))
	mat <- matrix(window(x,syear,c(eyear,eper)),nyears,frequency(x))
   
    nst <- ts(apply(mat,1,mean), start = c(syear,1), frequency = 1) 
    return(nst)

}
