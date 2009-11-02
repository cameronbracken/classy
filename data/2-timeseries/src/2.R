source('lib.R')
if(file.exists('output/lf.Rdata')){load('output/lf.Rdata')}else{source('readData.R')}

arma.mod <- list()
for(i in 0:3)
	for(j in 0:3)
		arma.mod[[paste('arma',i,j,sep='')]] <- arima(x.ts,c(i,0,j))

aics <- sapply(arma.mod,AIC)
gcvs <- sapply(arma.mod,gcv.arma)

best.arma.aic <- names(which(aics == min(aics)))
best.arma.gcv <- names(which(gcvs == min(gcvs)))

save(best.arma.aic,best.arma.gcv,arma.mod,aics,gcvs,file='output/2.Rdata')
