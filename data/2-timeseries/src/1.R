	#read support functions
source('lib.R')

	#read data (ac-ft)
x <- read.table('data/Leesferry-mon-data.txt')

	# creat timeseries and convert to cms 
x <- matrix(as.vector(t(x[,-1])),ncol=12)*0.000469050

	#remove mean
x.mean <- apply(x,2,mean)
x.sd <- apply(x,2,sd)

x <- t((t(x) - x.mean) / x.sd)
x <- ts(array(t(x)),start=c(1906,1),frequency=12)

save(x,file='output/1.Rdata')