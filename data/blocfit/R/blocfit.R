blocfit <- 
function(x, y, a = 0.7, deg = 1, kern = 'bisq'){
	
	# Setup Constants
	n <- length( y )
	nn <- floor( a*n )
	print(nn)
	
	# Construct the L Matrix
	L <- matrix( NA, nrow = n, ncol = n )
	
	W <- chooseWeightFunction( kern )
	values <- numeric( n )
	
	for(i in 1:n){
		
		#calculate distance
		d <- abs(x - x[i])
		d <- d/max(d)
		nearest <- order(d)[1:nn]
		
		#fit the local model
		lfit <- lm( y[nearest] ~ x[nearest], weights = W(d[nearest]) )
		values[i] <- lfit$fitted.values[1]

	}
	
	plot(x,y)
	lines(values)
	abline(lm(y~x),col='red')
	values
	
}

chooseWeightFunction <-
function(kern){
	
	if(kern == 'bisq')
		return( function(x) 15/16*(1-x^2)^2 )
	if(kern == 'none')
		return( function(x) rep(1,length(x)) )
	
}