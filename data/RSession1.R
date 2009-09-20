#!/usr/bin/env Rscript

system("R CMD INSTALL --no-docs blocfit")
require(blocfit)
require(locfit)
data(ethanol)
data(oneD)

x <- oneD$x; y <- oneD$y
#x <- ethanol$E; y <- ethanol$NOx

values <- blocfit(x, y, a=.7, kern='bisq')
print(values)
#plot(x,y)
#lines(sort(x),values[order(x)])
#abline(lm(y~x),col='red')
