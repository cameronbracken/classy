#!/usr/bin/env Rscript

require(locfit)
source('libblocfit.R')

	#Read simple test data
oneD <- read.table('data/oneD.tab',header=T)

x <- oneD$x; y <- oneD$y

blfit <- blocfit(x, y, a=.5, p=1, kern='bisq')	
