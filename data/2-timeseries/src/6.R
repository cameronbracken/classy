# Get the data
source('lib.R')
if(file.exists('output/1.Rdata')) load('output/1.Rdata') else source('1.R')
require(locfit)
require(pear)

	#Interannual Modified KNN
sim.imknn.v <- imknn(x,nsims=nsims)

sim.imknn.ts <- ts(sim.imknn.v,start=c(2001,1),frequency=12)
sim.imknn.agg <- ts.annual.mean(sim.imknn.ts)
sim.imknn <- array(NA,c(nyears,12,nsims))
for(i in 1:nsims)
	sim.imknn[,,i] <- 
	matrix(sim.imknn.ts[((i-1)*12*nyears+1):(i*12*nyears)],nyears,12,byrow=T)

stats.imknn <- sim.stats(sim.imknn, sim.imknn.agg, nsims=nsims,nyears=nyears)
	
sp.imknn <- seqpeak.multi(sim.imknn,x.ts.raw,nsims)

	
save(sim.imknn,sim.imknn.agg,stats.imknn,sp.imknn,file='output/6.Rdata')
