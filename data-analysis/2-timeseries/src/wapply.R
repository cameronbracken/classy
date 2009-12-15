wapply <- 
function(x, fun, win.len = length(x), ...){
    
    r <- (length(x) %% win.len)
    if(r > 0) x <- x[1:(length(x)-r)]
    stack <- matrix(x,nrow = win.len)
    return(apply(stack,2,fun,...))
    
}
