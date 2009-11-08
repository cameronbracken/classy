source('lib.R')
if(file.exists('output/1.Rdata')) load('output/1.Rdata') else source('1.R')
library(locfit)

#usa - x
#pac - y
b1 <- best.par(pac$pc[,1],usa$pc[,1])
lfpc1 <- locfit(usa$pc[,1]~pac$pc[,1],alpha=b1$a,deg=b1$p)
b2 <- best.par(pac$pc[,2],usa$pc[,2])
lfpc2 <- locfit(usa$pc[,2]~pac$pc[,2],alpha=b1$a,deg=b2$p)

save(lfpc1,lfpc2,file='output/2.Rdata')