#!/usr/bin/env Rscript

source('lib.R')
require(gtools)
seasonal.total <- TRUE
mean.len <- 5
running.mean <- FALSE
if(!running.mean)mean.len <- 1

    # NINO3 index (kaplan reconstruction)
enso <- as.matrix(read.table('data/inino3.txt'))[,-1]
sy <- scan('data/inino3.txt',comment.char='#', n=1, quiet=T)
enso[enso == -999.9] <- NA
enso <- ts(array(t(enso[,-1])), start = sy, frequency=12)

	# Meko reconstructions converted to MAF and 
	# 5-year running mean
meko <- as.matrix(read.table('data/Meko.txt'))[,2]
sy.meko <- as.matrix(read.table('data/Meko.txt'))[1,1]
meko.raw <- meko
if(running.mean) meko <- running(meko,width=mean.len)
meko <- ts(meko,start = sy.meko+mean.len-1) * 10^-6

    # Woodhouse reconstructions converted to MAF and 
	# 5-year running mean
wood <- as.matrix(read.table('data/woodhouse.txt'))[,2]
sy.wood <- as.matrix(read.table('data/woodhouse.txt'))[1,1]
wood.raw <- wood
if(running.mean) wood <- running(wood,width=mean.len)
wood <- ts(wood,start = sy.wood +mean.len-1) * 10^-6

    #Lees Ferry Monthly volumes can be seasonal or not depending on 
    # flag at the top of this file
lees <- as.matrix(read.table('data/LeesFerry.txt'))[,-1]
sy <- scan('data/LeesFerry.txt',comment.char='#', n=1, quiet=T)
months <- if(seasonal.total) 4:7 else 1:12
lees <- apply(lees[,months], 1, sum)
#if(running.mean) lees <- running(lees,width=mean.len)
lees <- ts(lees, start = sy) * 10^-6

    #Get binary state series based on median of historical
meko.b <- binary.ts(meko,median(meko))
wood.b <- binary.ts(wood,median(wood)) 
lees.b <- binary.ts(lees,median(lees))

#plot(wood,type='n');grid()
#lines(wood,type='s')
#lines(lees,col='red',type='s')

save(enso, meko, meko.raw, meko.b, wood, wood.raw, wood.b, lees, lees.b, 
	file = 'data/ts.Rdata')
