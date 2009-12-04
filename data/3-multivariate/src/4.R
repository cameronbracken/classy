source('lib.R')
flow <- as.matrix(read.table("data/AMJJFLOW-49-99.txt"))/(10^3)
swe <- as.matrix(read.table("data/March1SWE-1949-99.txt"))
nsim <- 250

#Singular value decomposition
C <- var(flow,swe)  #note that the variable with fewer columns should be first
S <- svd(C)

#Time coefficients 
flow.tc <- flow %*% S$u
swe.tc <- swe %*% S$v

#heterogeneous Correlation maps
cor.map.swe <- cor.map.flow <- list()
for(i in 1:3){
	cor.map.swe[[i]] <- as.vector(cor(swe.tc[,1],swe))
	cor.map.flow[[i]] <- as.vector(cor(flow.tc[,1],flow))
}

#Local polynomial fit of TC
lftc <- list()
for(i in 1:2)
	for(j in 1:2){
		b <- best.par(swe.tc[,i],flow.tc[,j])
		lftc[[paste(i,j,sep='')]] <- 
			locfit(flow.tc[,j]~swe.tc[,i],alpha=b$a,deg=b$p)
	}
	
#Forecasting
ypred.svd <- fc.svd(flow,swe,nsim)

#get the rpss and median correlation stats
stats.svd <- fc.stats(flow,ypred.svd,nsim)
stats.svd$rpss <-

save(swe.tc, flow.tc, cor.map.swe, cor.map.flow, lftc, stats.svd, ypred.svd,
	flow, file='output/4.Rdata')
	