# Get the data
source('lib.R')
if(file.exists('output/1.Rdata')) load('output/1.Rdata') else source('1.R')
require(locfit)
require(pear)

st <- mean(x[,1])
k <- round(sqrt(length(x[,1])))

w <- numeric(k)
for(i in 1:k)
	w[i] <- (1/i)/(sum(1/(1:i)))
w <- cumsum(w/sum(w))

sim.mknn.v <- mknn(x,nsims=nsims)

sim.mknn.ts <- ts(sim.mknn.v,start=c(2001,1),frequency=12)
sim.mknn.agg <- ts.annual.mean(sim.mknn.ts)
sim.mknn <- array(NA,c(nyears,12,nsims))
for(i in 1:nsims)
	sim.mknn[,,i] <- 
	matrix(sim.mknn.ts[((i-1)*12*nyears+1):(i*12*nyears)],nyears,12,byrow=T)

stats.mknn <- sim.stats(sim.mknn, sim.mknn.agg, nsims=nsims,nyears=nyears)
	
sp.mknn <- seqpeak.multi(sim.mknn,x.ts.raw,nsims)

	
save(sim.mknn,sim.mknn.agg,nsims,stats.mknn,
	nyears,sp.mknn,file='output/5.Rdata')
