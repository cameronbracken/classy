# Get the data
source('lib.R')
if(file.exists('output/lf.Rdata')){load('output/lf.Rdata')}else{source('readData.R')}
if(file.exists('output/2.Rdata')){load('output/2.Rdata')}else{source('2.R')}

# collect the simulations
n.sim <- 100
n.years <- 95
sim <- array(NA,c(n.years,12,n.sim))
b <- arma.mod[[best.arma.aic]]
p <- b$arma[1]
q <- b$arma[2]
for(i in 1:n.sim)
	sim[,,i] <- matrix(arima.sim(n=n.years*12, 
		list(ar=b$coef[1:p],
			ma=b$coef[(p+1):(p+q)]), 
			sd=sqrt(b$sigma2)),ncol=12,byrow=T)
		
#Get the statistics
stats <- list()
stats$mean <- stats$var <- stats$skew <- stats$lag1 <- matrix(NA,n.sim,13)

ann <- matrix(NA,n.years,n.sim)

for(i in 1:12){
	this.mon <- sim[,i,] * x.sd[i] + x.mean[i]
	stats$mean[,i] <- apply(this.mon,2,mean)
	stats$var[,i] <- apply(this.mon,2,var)
	stats$skew[,i] <- apply(this.mon,2,skew)
	stats$lag1[,i] <- apply(this.mon,2,function(x) mylag(x,1,docor=TRUE))
}

#Annual stats
for(i in 1:n.years)
	ann[i,] <- apply(sim[i,,],2,mean)
	
stats$mean[,13] <- apply(ann,2,mean)
stats$var[,13] <- apply(ann,2,var)
stats$skew[,13] <- apply(ann,2,skew)
stats$lag1[,13] <- apply(ann,2,function(x) mylag(x,1,docor=TRUE))

#setup for plotting
stats <- lapply(stats,as.data.frame)
mon <- c('Jan','Feb','Mar','Apr','May','Jun',
	'Jul','Aug','Sep','Oct','Nov','Dec','Ann')
for(i in 1:length(stats)) names(stats[[i]]) <- mon

x.ts.ann <- ts.annual.mean(x.ts)

source('3-seqpeak.R')

save(stats,x.ts.ann,sim,n.sim,n.years,y,s,x.ts.s,sim.sy,file='output/3.Rdata')
