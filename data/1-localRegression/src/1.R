#!/usr/bin/env Rscript

rm(list=ls())

suppressPackageStartupMessages(require(locfit))
source('libblocfit.R')
if(!file.exists('output')) dir.create('output')

	#Read simple test data
oneD <- read.table('data/oneD.tab',header=T)

x <- oneD$x; y <- oneD$y

save(list=ls(),file='output/1.Rdata')