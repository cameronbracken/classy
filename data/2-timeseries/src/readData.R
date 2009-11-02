	#read support functions
source('lib.R')
	#read data (ac-ft)
x <- as.matrix(read.table('data/Leesferry-mon-data.txt')[,-1])
	# creat timeseries and convert to cms 
x <- x*0.000469050
x.mean <- apply(x,2,mean)
x.sd <- apply(x,2,sd)
x.skew <- apply(x,2,skew)
x.lag1 <- apply(x,2,mylag,1,docor=TRUE)
	#remove mean
x <- t((t(x) - x.mean) / x.sd)
x.may <- x[,5]
x.ts <- ts(array(t(x)),start=c(1906,1),frequency=12)

save(x,x.ts,x.may,x.mean,x.sd,x.skew,x.lag1,file='output/lf.Rdata')
