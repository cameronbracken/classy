#!/usr/bin/env Rscript

source('lib.R')
require(gtools)
require(Hmisc)
seasonal.total <- TRUE
running.mean <- TRUE
if(!running.mean)m <- 1

    # NINO3 index (kaplan reconstruction)
enso <- as.matrix(read.table('data/inino3.txt'))[,-1]
sy <- scan('data/inino3.txt',comment.char='#', n=1, quiet=T)
enso[enso == -999.9] <- NA
enso <- ts(array(t(enso[,-1])), start = sy, frequency=12)

  # nino3 cook reconstructions
cook <- read.table('data/cook.txt',header=T)
cook <- ts(cook[,2],start=cook[1,1])
if(running.mean) cook <- as.vector(running(cook,width=m))
cook <- ts(cook,start = start(cook) + m-1)

  # nino3 observed
nino3 <- read.table('data/nino3.txt',header=T)
nino3 <- ts(nino3[,3],start=c(nino3[1,1],nino3[1,2]),frequency=12)
nino3 <- ts(wapply(nino3,mean,12),start=start(nino3))
  

  

  ######################################################
  #
  # Meko reconstructions converted to MAF
  #
  ######################################################
meko <- as.matrix(read.table('data/Meko.txt'))[,2]
sy.meko <- as.matrix(read.table('data/Meko.txt'))[1,1]
meko.raw <- meko
if(running.mean) meko <- as.vector(running(meko,width=m))
meko <- ts(meko,start = sy.meko+m-1) * 10^-6

  ######################################################
  #
  # Woodhouse reconstructions converted to MAF
  #
  ######################################################
wood <- as.matrix(read.table('data/woodhouse.txt'))[,2]
sy.wood <- as.matrix(read.table('data/woodhouse.txt'))[1,1]
wood.raw <- wood
if(running.mean) wood <- as.vector(running(wood,width=m))
wood <- ts(wood,start = sy.wood +m-1) * 10^-6

  ######################################################
  #
  # Lees Ferry Monthly volumes 
  # (can be seasonal or not depending on flag at 
  # the top of this file
  #
  ######################################################
lees <- as.matrix(read.table('data/LeesFerry.txt'))[,-1]
sy <- scan('data/LeesFerry.txt',comment.char='#', n=1, quiet=T)
months <- if(seasonal.total) 4:7 else 1:12
lees <- apply(lees[,months], 1, sum)
#if(running.mean) lees <- running(lees,width=m)
lees <- ts(lees, start = sy) * 10^-6

    #Get binary state series based on median of historical
meko.b <- ntile.ts(meko,ns)
wood.b <- ntile.ts(wood,ns)
cook.b <- ntile.ts(cook,ns) 
lees.b <- ntile.ts(lees,ns)
nino3.b <- ntile.ts(lees,ns)

#plot(wood,type='n');grid()
#lines(wood,type='s')
#lines(lees,col='red',type='s')

save(enso, meko, meko.raw, meko.b, wood, wood.raw, wood.b, lees, lees.b, 
  cook, cook.b, nino3, nino3.b,
  file = 'data/ts.Rdata')
