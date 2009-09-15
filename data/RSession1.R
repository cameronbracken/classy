#!/usr/bin/env Rscript

system("R CMD INSTALL blocfit")
library(blocfit)
load("/Users/cameron/Desktop/codes/classy/data/blocfit/data/sample-set.Rdata")

blocfit(blocfit.data$x,blocfit.data$y, a=.3, kern='none')