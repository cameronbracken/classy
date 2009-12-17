#!/usr/bin/env Rscript
source('setup.R')
seasonal.total <- TRUE

    # NINO3 index (kaplan reconstruction)
enso <- as.matrix(read.table('data/inino3.txt'))[,-1]
sy <- scan('data/inino3.txt',comment.char='#', n=1, quiet=T)
enso[enso == -999.9] <- NA
enso <- ts(array(t(enso[,-1])), start = sy, frequency=12)

	# Meko reconstructions converted to MAF and 
	# 5-year running mean
meko <- as.matrix(read.table('data/Meko.txt'))
meko <- ts(running(meko[,2],width=5),start = meko[1,1]) * 10^-6

    # Woodhouse reconstructions converted to MAF and 
	# 5-year running mean
wood <- as.matrix(read.table('data/woodhouse.txt'))
wood <- ts(running(wood[,2],width=5),start = wood[1,1]) * 10^-6

    #Lees Ferry Monthly volumes can be seasonal or not depending on 
    # flag at the top of this file
lees <- as.matrix(read.table('data/LeesFerry.txt'))[,-1]
sy <- scan('data/LeesFerry.txt',comment.char='#', n=1, quiet=T)
months <- if(seasonal.total) 4:7 else 1:12
lees <- ts( running(apply(lees[,months], 1, sum),width=5), start = sy) * 10^-6

    #Get binary state series based on median of historical
meko.b <- binary.ts(meko,median(lees))
wood.b <- binary.ts(wood,median(lees)) 
lees.b <- binary.ts(lees,median(lees))

#plot(wood,type='n');grid()
#lines(wood,type='s')
#lines(lees,col='red',type='s')

save(enso, meko, meko.b, wood, wood.b, lees, lees.b, file = 'data/ts.Rdata')
