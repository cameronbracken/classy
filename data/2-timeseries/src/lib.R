source('my.acf.R')
source('my.pacf.R')
source('solve.yw.R')

skew <- 
function(x){
	n <- length(x)
	nfact <- n/((n - 1) * (n - 2))
	xm <- mean(x)
	xv <- sqrt(var(x))
	skw <- sum((x - xm)^3)
	skew <- (nfact * skw)/xv^3
	return(skew)
}

mylag <- 
function(x,lag,docor=FALSE){

    if(lag>length(x)) warning("Lag is larger than input vector length, returning NA's") 

    if(lag<0)  lagn = c(rep(NA,abs(lag)),x[-(length(x):(length(x)+lag+1))])
    if(lag==0) lagn = x    
    if(lag>0)  lagn = c(x[-(1:lag)],rep(NA,lag))

    remove = !is.na(lagn) & !is.na(x)
    if(docor) return(cor(x[remove],lagn[remove])) 
    else return(lagn)

}

ts.annual.mean <- 
function(x){

    options(warn=-1)
    nyears <- length(unique(floor(time(x))))
    syear <- start(x)[1]
    nst = vector('numeric',nyears)

    for(i in 1:nyears)  nst[i] <- mean(window(x,syear - 1 + i,
                                        c(syear - 1 + i, frequency(x))))
   
    nst <- ts(nst, start = c(syear,1), frequency = 1) 
    return(nst)

}

wapply <- 
function(x, fun, win.len = length(x), ...){
    
    r <- (length(x) %% win.len)
    if(r > 0) x <- x[1:(length(x)-r)]
    stack <- matrix(x,nrow = win.len)
    return(apply(stack,2,fun,...))
    
}


ts.next.time <- 
function(x){
    en <- end(x)
    en <- if(en[2] == frequency(x))
            c(en[1] + 1, 1)
        else
            c(en[1], en[2] + 1)
    return(en)
}

ts.order.start <- 
function(x,start.per){

        f = frequency(x)
        ord <- if(start.per == 1) 
                start.per:f
           else if(start.per == f) 
                c(f, 1:(f - 1))
           else
                c(start.per:f, 1:(start.per - 1))
        return(ord)
}

ts.sim.and.calc <- 
function(x, order, annual.mean = TRUE, meanvec=NULL, nsims = 10000){

    x.fit <- arima(x,order=order)
    x.sim <- arima.sim(list(ar = coef(x.fit)), nsims, n.start = 100,
                                sd = sqrt(x.fit$sigma2))
    
    x.sim <- ts(x.sim, start = ts.next.time(x), frequency = frequency(x))
    if(annual.mean) x.sim <- ts.annual.mean(x.sim + meanvec)
    
    return(x.sim)
}
