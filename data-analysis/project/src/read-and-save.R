enso <- as.matrix(read.table('data/inino3.txt'))[,-1]
sy <- scan('data/inino3.txt',comment.char='#', n=1, quiet=T)
enso[enso == -999.9] <- NA
enso <- ts(array(t(enso[,-1])), start = sy, frequency=12)

	#All flow data converted to cms
meko <- as.matrix(read.table('data/Meko.txt'))
meko <- ts(meko[,2],start = meko[1,1]) * 0.0004690502

wood <- as.matrix(read.table('data/woodhouse.txt'))
wood <- ts(wood[,2],start = wood[1,1]) * 0.0004690502

lees <- as.matrix(read.table('data/LeesFerry.txt'))[,-1]
sy <- scan('data/LeesFerry.txt',comment.char='#', n=1, quiet=T)
lees <- ts(apply(lees[,-1], 1, mean), start = sy) * 0.0004690502

meko.m <- mean(meko)
meko.b <- integer(length(meko))
meko.b[meko >= meko.m] <- 1
meko.b[meko < meko.m] <- 0

wood.m <- mean(wood)
wood.b <- integer(length(wood))
wood.b[wood >= wood.m] <- 1
wood.b[wood < wood.m] <- 0

lees.m <- mean(lees)
lees.b <- integer(length(lees))
lees.b[lees >= lees.m] <- 1
lees.b[lees < lees.m] <- 0

save(enso, meko, meko.b, wood, wood.b, lees, lees.b, file = 'data/ts.Rdata')
