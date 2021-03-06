#!/usr/bin/env Rscript

rm(list=ls())

library(fields)
library(topmodel)
data(huagrahuma)
data(huagrahuma.dem)
h <- huagrahuma
Qobs <- h$Qobs

    #dem
image.plot(huagrahuma.dem,axes=F,xlab='Longitude',ylab='Latitude',
	main='Huagrahuma Basin DEM (m)')
contour(huagrahuma.dem,add=T)

    #contributing area
carea <- topidx(huagrahuma.dem, resolution= 25)$area
image.plot(carea,axes=F,main='Contributing Area (m^2)',
    xlab='Longitude',ylab='Latitude')

    #topographic index
ti<- topidx(huagrahuma.dem, resolution= 25)$atb
image.plot(ti,axes=F,main='Topographic Index')

    ## Run once
Qsim <- topmodel(h$parameters,h$topidx,h$delay,h$rain,h$ET0)
times <- seq(0,by=15/1440,length.out=length(Qobs))

    #plot simulated and observed
plot(times, Qobs,cex=.2, xlab='Time (days)', ylab='Flow (mm / 15 min)')
lines(times, Qsim, col='blue')
legend('topleft',c('Observed','Predicted'),pch=c(1,-1),lty=c(0,1),col=c(1,4))

    #plot observed versus predicted
plot(Qobs,Qsim, main = 'Observed v. Predicted Flow (mm / 15 min)', 
	xlab='Observed', ylab= 'Predicted')
abline(0,1)

NSeff(Qobs,Qsim)

#number of monte carlo simulations
runs<-10000

qs0   <- runif(runs)*4e-5
lnTe  <- runif(runs)*3-2
m     <- runif(runs)*0.2
Sr0   <- runif(runs)*0.02
Srmax <- runif(runs)*2
td    <- runif(runs)*3
vch   <- 1000
vr    <- 100+runif(runs)*2400
k0    <- runif(runs)*0.01
CD    <- runif(runs)*5
dt    <- 0.25

    # load the nse from a data file or run the model and save the values
saveFile <- 'nse.RData'
if(!file.exists(saveFile)){
		# returns an array of Nash Sutcliffe efficiencies; 
    	# one for each parameter set:
	parameters<-cbind(qs0,lnTe,m,Sr0,Srmax,td,vch,vr,k0,CD,dt)
	nse <- topmodel(parameters,h$topidx,h$delay,h$rain,h$ET0,Qobs = Qobs)
	save(nse,parameters,file=saveFile)
}else{
	load(saveFile)
}


p.best <- parameters[which(nse==max(nse)),]
p.top <- parameters[which(nse > .8),]
p.worst <- parameters[which(nse==min(nse)),]
Qsim.best <- topmodel(p.best, h$topidx, h$delay, h$rain, h$ET0)
Qsim.top <- topmodel(p.best, h$topidx, h$delay, h$rain, h$ET0)
Qsim.worst <- topmodel(p.worst, h$topidx, h$delay, h$rain, h$ET0)

NSeff(Qobs,Qsim.best)

    # plot observed and simulated discharge for the best and worst models:
plot(times, Qobs,cex=.2,xlab='Time (days)',ylab='Discharge (mm/15min)',
	main='Best Model')
lines(times, Qsim.worst, col=rgb(1,0,0,.5))
lines(times, Qsim.best, col='blue')
legend('topleft',c('Observed','Best','Worst'),lty=c(0,1,1),col=c(1,'blue','red'),pch=c(1,-1,-1))

##Plot histograms of the top sets of parameters
layout(matrix(1:9,ncol=3))
for(i in 1:ncol(p.top)){
	if(!any(colnames(p.top)[i]==c('dt','vch'))){
		hist(p.top[,i],xlab='',main=colnames(p.top)[i])
	}
}

library(MASS)
vr <- p.top[,8]

    #Fit normal
normal.fit <- fitdistr(vr, 'normal')
mean <- normal.fit$estimate[1]
sd <- normal.fit$estimate[2]

    #Fit exponential
exp.fit <- fitdistr(vr, 'exponential')
exp.lam <- exp.fit$estimate[1]

    #Fit Gamma
gamma.fit <- fitdistr(vr, dgamma, list(shape = 5, rate = 0.01), lower = 0.001)
shape <- gamma.fit$estimate[1]
rate <- gamma.fit$estimate[2]

	#plot the results
x <- seq(0,3000,,1000)
hist(vr,freq=F,xlim=c(0,max(vr)),ylim=c(0,max(dgamma(x,shape,rate))))
lines(x,dgamma(x,shape,rate),col='green')
lines(x,dnorm(x,mean,sd),col='orange')
lines(x,dexp(x,exp.lam),col='purple')
legend('topright',c('Gamma','Normal','Exponential'),
    lty=1,col=c('green','purple','orange'),pch=-1)