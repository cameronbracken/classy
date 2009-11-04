# Get the data
source('lib.R')
if(file.exists('output/1.Rdata')) load('output/1.Rdata') else source('1.R')
require(locfit)
options(warn=-1)

y.may <- x[,5]
x.lag <- list()
gcvs <- ks <- numeric(1)
for(i in 1:4){
	x.lag[[i]] <- cbind(x[,(5-i):4])
	bp <- best.par(x.lag[[i]],y.may,f=gcvplot,maxk=1000)
	gcvs[i] <- bp$gcv
	ks[i] <- bp$a * nrow(x.lag[[i]])
}
#tab <- rbind(gcvs,ks,mi)
#print(xtable(tab))
mi <- aminf(as.vector(t(x)),4)
