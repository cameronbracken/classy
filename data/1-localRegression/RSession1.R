#!/usr/bin/env Rscript

system("R CMD INSTALL --no-docs blocfit")
require(blocfit)
#require(locfit)
#data(ethanol)
data(oneD)

x <- oneD$x; y <- oneD$y
#x <- ethanol$E; y <- ethanol$NOx

blfit <- blocfit(x, y, a=.7, p=2, kern='bisq')
plot(blfit)
