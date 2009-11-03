	#returns the best alpha and degree
best.par <- 
function(x,y, a = seq(0.2,1.0,by=0.05), n = length(a), f=c(gcvplot,aicplot)){
	
		# get the gcv values for all combinations of deg and alpha
	d1 <- f(y~x, deg=1, alpha=a, kern='bisq', scale=T)
	d2 <- f(y~x, deg=2, alpha=a, kern='bisq', scale=T)
	
	gcvs <- c(d1$values,d2$values)
	best <- order(gcvs)[1]
		#get the best alpha and degree
	bestd <- c(rep(1,n),rep(2,n))[best]
	bestalpha <- c(a,a)[best]
	
	return(list(p=bestd,a=bestalpha,gcv=gcvs[best]))
}
