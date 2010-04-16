mylag <- 
function(x,lag,docor=FALSE){

    if(lag>length(x)) warning("Lag is larger than input vector length, returning NA's") 

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
