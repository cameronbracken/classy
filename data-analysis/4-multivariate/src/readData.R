rm(list=ls())
source('lib.R')

nsims <- 250

lf <- as.matrix(read.table('data/LeesFerry-monflows-1906-2006.txt'))[,-1]
gr <- as.matrix(read.table('data/GreenRiver-UT-monflows-1906-2006.txt'))[,-1]
lf <- lf * 0.0004690502
gr <- gr* 0.0004690502

lf.ann <- apply(lf,1,mean)
gr.ann <- apply(gr,1,mean)

lf.may <- lf[,5]
gr.may <- gr[,5]

lf.mean <- apply(lf,2,mean)
gr.mean <- apply(gr,2,mean)

lf.sd <- apply(lf,2,sd)
gr.sd <- apply(gr,2,sd)

lf.lag1 <- peacf(ts(array(t(lf)),start=1906,frequency=12),
	plot=FALSE,lag.max=1)$acf
gr.lag1 <- peacf(ts(array(t(gr)),start=1906,frequency=12),
	plot=FALSE,lag.max=1)$acf

lf.skew <- apply(lf,2,skew)
gr.skew <- apply(gr,2,skew)

lf.scale <- t( ( t(lf) - lf.mean ) / lf.sd )
gr.scale <- t( ( t(gr) - gr.mean ) / gr.sd )

Z <- array(NA,c(nrow(lf),ncol(lf),2))
Z[,,1] <- lf.scale
Z[,,2] <- gr.scale

ny <- nrow(lf)

if(!file.exists('output')) dir.create('output')
save(list=ls(),file='output/flows.Rdata')
