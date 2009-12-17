
if(!file.exists('output/flows.Rdata'))source('readData.R')
load('output/flows.Rdata')
source('lib.R')

knn.sim <- knn.multi.sim(Z)

#Put back seasonal mean and sd
for(i in 1:nsims){
	knn.sim[,,1,i] <- t((t(knn.sim[,,1,i]) * lf.sd) + lf.mean)
	knn.sim[,,2,i] <- t((t(knn.sim[,,2,i]) * gr.sd) + gr.mean)
}

lf.stats.knn <- sim.stats(knn.sim[,,1,],nsims)
gr.stats.knn <- sim.stats(knn.sim[,,2,],nsims)

save(lf.stats.knn,gr.stats.knn,knn.sim,file='output/2.Rdata')
