aminf <- function(data,nlags){

	require(sm)
	n=length(data)
	tobs=1:nlags
	for(i in 1:nlags){
		n1=n-i
		i1=i+1
		x=data[1:n1]
		y=data[i1:n]
		air3  <- cbind(x,y)
		result.12 <- sm.density(air3,   eval.points = air3,   display = "none")
		result.1  <- sm.density(x,  eval.points = x,  display = "none")
		result.2  <- sm.density(y, eval.points = y, display = "none")
		tobs[i]      <- mean(log(result.12$estimate / 
					(result.1$estimate * result.2$estimate)))
		#Mutual Information
	}

	tobs

}
