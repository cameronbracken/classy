source('lib.R')
if(file.exists('output/1.Rdata')) load('output/1.Rdata') else source('1.R')

arma.mod <- list()
for(i in 0:3)
	for(j in 0:3)
		arma.mod[[paste('ARMA(',i,',',j,')',sep='')]] <- arima(x.ts,c(i,0,j))

aics <- sapply(arma.mod,AIC)
gcvs <- sapply(arma.mod,gcv.arma)

best.arma.aic <- names(which(aics == min(aics)))
best.arma.gcv <- names(which(gcvs == min(gcvs)))

save(best.arma.aic,best.arma.gcv,arma.mod,aics,gcvs,file='output/2.Rdata')
