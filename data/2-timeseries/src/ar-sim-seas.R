ar.sim.seas <- function(model, peacf, n, n.start = NA){

    # simulate from a peariodic AR model 

    nobs <- length(model$residuals)
    p <- model$model.orders
     
    x.means <- peacf$means
    freq <- frequency(model$residuals)

        #find the starting period of the prediction
    end <- end(model$residuals)
    end <-  if(end[2] == freq)
                c(end[1] + 1, 1)
            else
                c(end[1], end[2] + 1)
    
    if(is.na(n.start))
        n.start <- 10*freq

    pred <- ts(rep(NA, n), start = end, frequency = freq)
    start.per <- end[2] 
    per <- if(start.per == 1)
                rep(start.per:freq, length.out = n)
           else if(start.per == frequency(model$residuals))
                rep(c(freq, 1:(freq-1)), length.out = n)
           else
                rep(c(start.per:freq,1:(start.per-1)), length.out = n)

    per <- ts(per, start = end, frequency = freq)
    len <- 10
    modvec <- rep(1:freq,len)

    
    for(t in 1:n){
        which.per <- modvec[((len-1)*freq + per[t] - p[per[t]]):((len-1)*freq - 1 + per[t])]
        m <- x.means[which.per]

        lastp <- if(t <= p[per[t]]) 
                    c(m[(length(m) - p[per[t]] + t):length(m)], pred[1:(t - 1)])[1:p[per[t]]]
                 else
                    pred[(t - p[per[t]]):(t - 1)]

        e <- rnorm(1,sd = sqrt(model$resvar[per[t]]))

        these.phis <- model$phi[per[t],1:p[per[t]]]
        
        pred[t] <- sum(these.phis * (lastp - m)) + x.means[per[t]] + e
                
    }
    
    pred <- pred[(n.start + 1):n]
    pred <- ts(pred, start = end, frequency = freq)
    
    return(pred)
    
    
}
