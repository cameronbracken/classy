# Get the data
source('lib.R')
if(file.exists('output/1.Rdata')) load('output/1.Rdata') else source('1.R')
if(file.exists('output/2.Rdata')) load('output/2.Rdata') else source('2.R')
require(pear)

# collect the simulations
sim <- array(NA,c(nyears,12,nsims))
b <- arma.mod[[best.arma.aic]]
p <- b$arma[1]
q <- b$arma[2]

sim.ts <- arima.sim(n=nyears*12*nsims, list(ar=b$coef[1:p],
		ma=b$coef[(p+1):(p+q)]), sd=sqrt(b$sigma2))
sim.ts <- ts(sim.ts,start=c(2001,1),frequency=12)

for(i in 1:nsims){
	sim[,,i] <- 
	matrix(sim.ts[((i-1)*12*nyears+1):(i*12*nyears)],nyears,12,byrow=T)
}

for(i in 1:12){
	sim[,i,] <- sim[,i,] * x.sd[i] + x.mean[i]
	mon <- seq(i,length(sim.ts),by=12)
	sim.ts[mon] <- sim.ts[mon] * x.sd[i] + x.mean[i]
}
		
#Get the statistics
sim.agg <- ts.annual.mean(sim.ts)
stats <- sim.stats(sim,sim.agg,nsims)

x.ts.ann <- ts.annual.mean(x.ts.raw)

sp <- seqpeak.multi(sim,x.ts.raw,nsims)

save(stats,x.ts.ann,sim,sp,file='output/3.Rdata')
