
if(!file.exists('output/flows.Rdata'))source('readData.R')
load('output/flows.Rdata')
source('lib.R')

Cmat <- mar.fit(Z)

mar.sim <- mar_sim(Z,Cmat$A,Cmat$B)

#Put back seasonal mean and sd
for(i in 1:nsims){
	mar.sim[,,1,i] <- t((t(mar.sim[,,1,i]) * lf.sd) + lf.mean)
	mar.sim[,,2,i] <- t((t(mar.sim[,,2,i]) * gr.sd) + gr.mean)
}

lf.stats.mar <- sim.stats(mar.sim[,,1,],nsims)
gr.stats.mar <- sim.stats(mar.sim[,,2,],nsims)

save(lf.stats.mar,gr.stats.mar,mar.sim,file='output/1.Rdata')
