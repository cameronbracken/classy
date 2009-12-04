source('lib.R')
nyears <- 95
nsim <- nyears * 100
	#read data (ac-ft) and convert to cms
x <- as.matrix(read.table('data/Leesferry-mon-data.txt')[,-1])*0.000469050
x.raw <- array(t(x))
x.ann <- x.bin <- ts.annual.mean(ts(x.raw,start=c(1906,1),frequency=12))
n <- length(x.ann)
	#historical stats
x.mean <- mean(x.ann); x.sd <- sd(x.ann)
x.skew <- skew(x.ann); x.lag1 <- mylag(x.ann,1,docor=T)

	#binary series
x.bin[(x.ann > median(x.ann))] <- 1
x.bin[!(x.ann > median(x.ann))] <- 0

x.wd <- x.ww <- x.dw <- x.dd <- numeric()
p.wd <- p.ww <- p.dw <- p.dd <- n.wd <- n.ww <- n.dw <- n.dd <- 0
for(i in 2:length(x.bin)){
	if(x.bin[i-1] == 0 && x.bin[i] == 0){n.dd <- n.dd + 1; x.dd[n.dd] <- x.ann[i]}
	if(x.bin[i-1] == 0 && x.bin[i] == 1){n.dw <- n.dw + 1; x.dw[n.dw] <- x.ann[i]}
	if(x.bin[i-1] == 1 && x.bin[i] == 0){n.wd <- n.wd + 1; x.wd[n.wd] <- x.ann[i]}
	if(x.bin[i-1] == 1 && x.bin[i] == 1){n.ww <- n.ww + 1; x.ww[n.ww] <- x.ann[i]}	
}
p.wd <- n.wd / (n.wd+n.ww); p.ww <- n.ww / (n.wd+n.ww)
p.dw <- n.dw / (n.dw+n.dd); p.dd <- n.dd / (n.dw+n.dd)

#Simulate
x.sim.state <- x.sim <- numeric(nsim)
x.sim.state[1] <- 1
x.sim[1] <- quantile(x.ann,.75)
for(i in 2:nsim){
	r <- runif(1)
	if(x.sim.state[i-1] == 1){
		x.sim.state[i] <- if(r < p.ww) 1 else 0
		pool <- if(r < p.ww) x.ww else x.wd
	}else{
		 x.sim.state[i] <- if(r < p.dd) 0 else 1 
		pool <- if(r < p.dd) x.dd else x.dw
	}	
	x.sim[i] <- sample(pool,1)
}
#calculate stats
x.sim <- matrix(x.sim,ncol=nyears)
x.sim.stats <- annual.stats(x.sim)

save(x.sim.stats, x.sim, x.ann, x.mean, x.sd, x.skew, x.lag1, nsim, nyears,
	file='output/6.Rdata')
	