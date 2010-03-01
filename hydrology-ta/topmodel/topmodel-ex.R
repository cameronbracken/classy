#!/usr/bin/env Rscript

rm(list=ls())
require(topmodel)

data(huagrahuma)
attach(huagrahuma)

## Run once
## returns a list of simulated runoff (Qobs), overland flow (qo), 
## subsurface flow (qs) and storage (S):
Qsim <- topmodel(parameters,topidx,delay,rain,ET0,verbose = TRUE)

#number of monte carlo simulations
runs<-10000

#define parameters 
qs0   <- runif(runs)*4e-5
lnTe  <- runif(runs)*3-2
m     <- runif(runs)*0.2
Sr0   <- runif(runs)*0.02
Srmax <- runif(runs)*2
td    <- runif(runs)*3-3
vch   <- 1000
vr    <- 100+runif(runs)*2400
k0    <- runif(runs)*0.01
CD    <- runif(runs)*5
dt    <- 0.25

parameters<-cbind(qs0,lnTe,m,Sr0,Srmax,td,vch,vr,k0,CD,dt)

## returns an array of Nash Sutcliffe efficiencies; one for each parameter set:
nse <- topmodel(parameters,topidx,delay,rain,ET0,Qobs = Qobs)

p.best <- parameters[which(nse==max(nse)),]
p.top <- parameters[which(nse > .8),]
p.worst <- parameters[which(nse==min(nse)),]
Qsim.best <- topmodel(p.best, topidx, delay, rain, ET0, verbose=T)
Qsim.top <- topmodel(p.best, topidx, delay, rain, ET0, verbose=T)
Qsim.worst <- topmodel(p.worst, topidx, delay, rain, ET0, verbose=T)

## Create timeseris objects of data
## Interpolate observed flow for the plot
n <- length(Qobs)
Qobs.interp <- approx(1:n,Qobs,1:n)$y
Qobs.ts <- ts(Qobs.interp,start=c(0,0),frequency=86400/15)
rain.ts <- ts(rain,start=c(0,0),frequency=86400/15)
Qsim.best.ts <- ts(as.vector(Qsim.best$Q),start=c(0,0),frequency=86400/15)
Qsim.worst.ts <- ts(as.vector(Qsim.worst$Q),start=c(0,0),frequency=86400/15)

## plot observed and simulated discharge for the best and worst models:
layout(cbind(1:2))
plot(Qobs.ts,type='l',xlab='Time (days)',ylab='Discharge (mm/15min)',
	main='Best Model')
lines(Qsim.best.ts, col='blue')

plot(Qobs.ts,type='l',xlab='Time (days)',ylab='Discharge (mm/15min)',
	main='Worst Model')
lines(Qsim.worst.ts, col='blue')

##Plot histograms of the top sets of parameters
layout(matrix(1:9,ncol=3))
for(i in 1:ncol(p.top))
	if(!any(colnames(p.top)[i]==c('dt','vch')))
		hist(p.top[,i],xlab='',main=colnames(p.top)[i])
