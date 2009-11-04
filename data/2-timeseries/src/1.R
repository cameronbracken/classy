	#read support functions
source('lib.R')
require(pear)

nsims <- 100
nyears <- 95

	#read data (ac-ft)
x <- as.matrix(read.table('data/Leesferry-mon-data.txt')[,-1])
	# creat timeseries and convert to cms 
x <- x*0.000469050
x.mean <- apply(x,2,mean)
x.sd <- apply(x,2,sd)
x.skew <- apply(x,2,skew)

	#remove mean
x.raw <- array(t(x))
x.scale <- t((t(x) - x.mean) / x.sd)
x.may.scale <- x.scale[,5]
x.may <- x[,5]
x.ts <- ts(array(t(x.scale)),start=c(1906,1),frequency=12)
x.ts.raw <- ts(x.raw,start=c(1906,1),frequency=12)


x.lag1 <- peacf(x.ts.raw,plot=FALSE,lag.max=1)$acf

save(x,x.raw,x.ts,x.ts.raw,x.may,x.may.scale,
	x.mean,x.sd,x.skew,x.lag1,nyears,nsims,file='output/1.Rdata')
