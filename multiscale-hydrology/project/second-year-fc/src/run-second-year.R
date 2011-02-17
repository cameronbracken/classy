nsi <- 3 # states
q <- 2 #years back
r <- 2 #years ahead
m <- 11
nsims <- 250

dataset <- 'meko'
cache <- FALSE

#Paramters to implement
mode <- 'drop-one' # or 'retroactive'
hist.cdf <- 'ecdf' # or 'gamma'
conditional.pools <- FALSE
scaled <- FALSE



for(ns in nsi){
  source('second-year.R')
}
