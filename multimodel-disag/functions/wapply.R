wapply.single <- 
function(x, fun, win.len = length(x), ...){
    
    r <- (length(x) %% win.len)
    if(r > 0) x <- x[1:(length(x)-r)]
    stack <- matrix(x,nrow = win.len)
    return(apply(stack,2,fun,...))
    
}

wapply <- function(x, fun, win.length = length(x), ...){
	
	if(is.null(ncol(x))){
		return(wapply.single(x, fun, win.length, ...))
	}else{
		for(i in 1:ncol(x)){
			if(i == 1){
				z <- wapply.single(x[,1], fun, win.length, ...)
			}else{
				z <- cbind(z,wapply.single(x[,i], fun, win.length, ...))
			}
			
		}
		attributes(z)$dimnames[[2]] <- attributes(x)$dimnames[[2]]
		return(z)
	}
		
	
}