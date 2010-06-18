
laglead <- 
function(bin,q,r){
    # gives back a list of two matricies, lag and lead, 
    # essentially the lag q matrix and the subsequent 
    # r values. 
    #
    # Ex.
    # 
    #  laglead(1:10,3,2)
    ## $x
    ##      [,1] [,2] [,3]
    ## [1,]    1    2    3
    ## [2,]    2    3    4
    ## [3,]    3    4    5
    ## [4,]    4    5    6
    ## [5,]    5    6    7
    ## [6,]    6    7    8
    ## 
    ## $y
    ##      [,1] [,2]
    ## [1,]    4    5
    ## [2,]    5    6
    ## [3,]    6    7
    ## [4,]    7    8
    ## [5,]    8    9
    ## [6,]    9   10
    
    
	n <- length(bin)

	lag <- matrix(NA, n-q-1, q)
	lead <- matrix(NA, n-q-1, r)

	for(i in 1:(n-q-1)){
		lag[i,] <- bin[i:(i+q-1)]
		lead[i,] <- bin[(i+q):(i+q+r-1)]
	}
	return(list(lag=lag,lead=lead))
}


wapply <- 
function(x, fun, win.len = length(x), ...){
    # applies a function to each succesive window of a time series;
    # good for things like annual means from monthly values.
    # note: does not return a ts object
    #
    # Ex.
    #
    # wapply(ts(rnorm(12*10),frequency = 12),mean,12)
    ## [1] -0.10910749  0.20212615  0.89948987 -0.27514597  0.18479893  0.55860823
    ## [7]  0.38323747  0.08271312  0.26844408  0.03115793
    ##
    
    r <- (length(x) %% win.len)
    if(r > 0) x <- x[1:(length(x)-r)]
    stack <- matrix(x,nrow = win.len)
    return(apply(stack,2,fun,...))
    
}

binary.ts <- 
function(x, limit = median(x), tie = 1 ){
    # returns a binary representation of x, 1 above limit, 0 below
    #
    # Ex. 
    #
    # binary.ts(1:10)
    ## [1] 0 0 0 0 0 1 1 1 1 1
    # binary.ts(1:10,8)
    ## [1] 0 0 0 0 0 0 0 1 1 1
    # binary.ts(1:10,8,tie=0)
    ## [1] 0 0 0 0 0 0 0 0 1 1
    
    b <- integer(length(x))
    
    filter <- if(tie == 1) x >= limit else x > limit
    
    #only need to set the 1's because b is already 0's
    b[filter] <- 1L
    
    if(class(x) == 'ts') 
        return(ts(b,start=start(x),end=end(x))) 
    else 
        return(b)
}

ntile.ts <- 
function(x, n, limit = seq(min(x),max(x),by=diff(range(x))/n), tie = 1 ){
    # returns a binary representation of x, 1 above limit, 0 below
    #
    
    b <- integer(length(x))
    
	for(i in 1:(n+1)){
    	filter <- 
		if(tie == 1) 
			x >= limit[i] & x <= limit[i+1]
		else 
			x > limit[i] & x <= limit[i+1]
    
	    #only need to set the 1's because b is already 0's
	    b[filter] <- as.integer(i-1)
	}
    
    if(class(x) == 'ts') 
        return(ts(b,start=start(x),end=end(x))) 
    else 
        return(b)
}

binarys <- function(i,align=F){
    # returns the binary representation of integer vector i (coerced) 
    # as a character vector
    #
    # Ex.
    #
    # binarys(1:10)
    ## [1] "1"    "10"   "11"   "100"  "101"  "110"  "111"  "1000" "1001" "1010"
    # binarys(1:10,align=T)
    ## [1] "0001" "0010" "0011" "0100" "0101" "0110" "0111" "1000" "1001" "1010"
    #
    
    bb <- function(i) if (i) paste(bb(i %/% 2), i %% 2, sep="") else ""
        
        #number of binary digits of largest number
    width <- ceiling(log(max(i)+1,base=2))
    
        #get the character vector
    i <- sapply(as.integer(i),bb)
    
    if(align)
        return(sprintf(paste('%0',width,'d',sep=''),as.integer(i)))
    else
        return(i)
}

rows.equal <- function(x, y){
	
	if(is.null(dim(x))) x <- rbind(x)
	which( as.logical( 
			apply(x, 1, all.equal, y)))
}

trprob <- function(lag,lead){
	
	states.lag <- unique(lag)
	states.lead <- unique(lead)
	nc <- 2^(ncol(states.lag)+ncol(states.lead))
	tpm <- list(
		from = matrix(NA,nrow=nc,ncol=ncol(states.lag)),
		to = matrix(NA,nrow=nc,ncol=ncol(states.lead)), 
		p = numeric(nc),
		pools = list()
		)
		
	n <- 0
	tpm$pools$to <- tpm$pools$from <- list()
	for(i in 1:nrow(states.lag)){
		
			#all rows starting in a particular state
		pool <- rows.equal(lag,states.lag[i,])
		tpm$pools$from[[i]] <- pool
		
		for(j in 1:nrow(states.lead)){
			
			n <- n + 1
			tpm$from[n,] <- states.lag[i,]
			tpm$to[n,] <- states.lead[j,]
			matches <- rows.equal(lead[pool,], states.lead[j,])
			possible <- nrow(cbind(lead[pool,]))
			tpm$p[n] <- length( matches ) / possible
				
				#save the indexes of the states from and to
			tpm$pools$to[[n]] <- matches
		
		}
	}
	return(tpm)
	
}

binary.combos <- function(n){
	as.matrix( expand.grid( rep( list(c(T,F)), n ) ) )
}

sim.pqr <- function(tr.paleo, conditional.pool = FALSE){
	
	if(conditional.pool){
			#simulate transition
		rand <- runif(1)
		which.to <- rank(c(rand,cumsum(this.p)))[1]
		state.to <- tr.paleo$to[which.to,]


			#conditional pool, quantiles given state.to
		pool.from <- tr.paleo$pools$from[[ which.from[which.to] ]]
		pool.to <- tr.paleo$pools$to[[ which.from[which.to] ]]
		pool.from <- matrix(pool.from,,q)
		pool.to <- matrix(pool.to,,r)
	}
	
}

mylag <- 
function(x,lag,docor=FALSE){

    if(lag>length(x)) 
		warning("Lag is larger than input vector length, returning NA's") 

    if(lag<0)  lagn = c(rep(NA,abs(lag)),x[-(length(x):(length(x)+lag+1))])
    if(lag==0) lagn = x    
    if(lag>0)  lagn = c(x[-(1:lag)],rep(NA,lag))

    remove = !is.na(lagn) & !is.na(x)
    if(docor){
		return(cor(x[remove],lagn[remove]))
    }else{
		return(lagn)
	}
}
