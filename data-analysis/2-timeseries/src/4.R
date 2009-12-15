# Get the data
source('lib.R')
if(file.exists('output/1.Rdata')) load('output/1.Rdata') else source('1.R')
require(pear)

win <- frequency(x.ts.raw)

peacf <- peacf(x.ts.raw, lag.max=1, plot=FALSE)
pepacf <- pepacf(x.ts.raw, lag.max=1, plot=FALSE)
pearp <- pear(x.ts.raw, m = 1)
sim.seas.ts <- ar.sim.seas(pearp, peacf, (nsims+1)*nyears*win)
sim.seas.agg <- ts.annual.mean(sim.seas.ts)
sim.seas <- array(NA,c(nyears,12,nsims))
for(i in 1:nsims)
	sim.seas[,,i] <- 
	matrix(sim.seas.ts[((i-1)*12*nyears+1):(i*12*nyears)],nyears,12,byrow=T)

stats.seas <- sim.stats(sim.seas,sim.seas.agg,nsims=nsims)

sp.seas <- seqpeak.multi(sim.seas,x.ts.raw,nsims)

save(stats.seas,sim.seas,sim.seas.agg,sp.seas,file='output/4.Rdata')
