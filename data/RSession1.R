#!/usr/bin/env Rscript

system("R CMD INSTALL --no-docs blocfit")
require(blocfit)
require(locfit)
data(ethanol)
data(oneD)

x <- oneD$x; y <- oneD$y
#x <- ethanol$E; y <- ethanol$NOx

fit <- blocfit(x, y, a=.7, p=2, kern='bisq')
xnew <- seq(range(x)[1],range(x)[2],length.out=100)
pred <- predict.blocfit(fit, at = xnew)



plot(x,y)
lines(xnew,pred$fittedValues)
abline(lm(y~x),col='red')

#pred <- predict.blocfit(fit)
#lines(x,pred$fittedValues)