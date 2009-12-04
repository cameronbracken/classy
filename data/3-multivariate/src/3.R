source('lib.R')
if(file.exists('output/1.Rdata')) load('output/1.Rdata') else source('1.R')

C <- var(pdsi.usa,sst)  
S <- svd(C)

#Time coefficients 
pdsi.tc <- pdsi.usa %*% S$u
sst.tc <- sst %*% S$v

cor.map.sst <- cor.map.pdsi <- list()
for(i in 1:3){
	cor.map.sst[[i]] <- 
		Tps(cbind(sst.lon-180,sst.lat),as.vector(cor(sst.tc[,1],sst)))
	cor.map.pdsi[[i]] <- 
		Tps(cbind(lon.usa-360,lat.usa),as.vector(cor(pdsi.tc[,1],pdsi.usa)))
}

save(sst.tc, pdsi.tc, cor.map.sst, cor.map.pdsi, file='output/3.Rdata')
