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
