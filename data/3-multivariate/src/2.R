source('lib.R')
if(file.exists('output/1.Rdata')) load('output/1.Rdata') else source('1.R')
library(locfit)

#usa - x
#pac - y
lfpc <- list()
for(i in 1:2)
	for(j in 1:2){
		b <- best.par(pac$pc[,i],usa$pc[,j])
		lfpc[[paste(i,j,sep='')]] <- locfit(usa$pc[,j]~pac$pc[,i],alpha=b$a,deg=b$p)
	}

#correlation maps
cor.pdsi.sst.pac1 <- as.vector(cor(pac$pc[,1],pdsi.usa))
cor.pdsi.sst.pac2 <- as.vector(cor(pac$pc[,2],pdsi.usa))
cor.pdsi.sst.atl1 <- as.vector(cor(atl$pc[,1],pdsi.usa))
cor.pdsi.sst.atl2 <- as.vector(cor(atl$pc[,2],pdsi.usa))

cor.map <- list()
cor.map[[1]] <- Tps(cbind(lon.usa-360,lat.usa),cor.pdsi.sst.pac1)
cor.map[[2]] <- Tps(cbind(lon.usa-360,lat.usa),cor.pdsi.sst.pac2)
cor.map[[3]] <- Tps(cbind(lon.usa-360,lat.usa),cor.pdsi.sst.atl1)
cor.map[[4]] <- Tps(cbind(lon.usa-360,lat.usa),cor.pdsi.sst.atl2)

save(lfpc,cor.map,file='output/2.Rdata')
