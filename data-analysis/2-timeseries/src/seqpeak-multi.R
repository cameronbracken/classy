seqpeak.multi <- function(sim,hist,nsims){
	
	days <- c(31,28,31,30,31,30,31,30,31,31,30,31)
	x.ts.s <- hist * days
	y <- c((0:9)/10,0.95)
	s.sim <- rep(0,length(y))
	sim.sy <- matrix(NA,nsims,length(y))
	for(i in 1:nsims){
		simf <- as.vector(t(sim[,,i]))* days
		for (j in 1:length(y))
			s.sim[j] <- seqpeak.r(simf,y[j]*mean(x.ts.s))$s
		sim.sy[i,] <- s.sim
	}
	sim.sy <- as.data.frame(sim.sy)
	names(sim.sy) <- round(mean(x.ts.s)*y)
	s <- rep(0,length(y))
	for (j in 1:length(y))
		s[j] <- seqpeak.r(x.ts.s,y[j]*mean(x.ts.s))$s
	
	return(list(s=s,y=y,sim.sy=sim.sy))
}
