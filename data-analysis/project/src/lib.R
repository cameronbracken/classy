
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

	for(i in 1:(n-m-1)){
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
    # getBinaryVector(1:10)
    ## [1] 0 0 0 0 0 1 1 1 1 1
    # getBinaryVector(1:10,8)
    ## [1] 0 0 0 0 0 0 0 1 1 1
    # getBinaryVector(1:10,8,tie=0)
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