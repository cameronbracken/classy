source("functions/skew.R")
source("functions/sim_knn.R")
source('functions/wapply.R')
#######get data and format


year.s <- 1906
	# monthly observed inteveneing natural flows at all 
	# natural flow nodes in upper basin
nat.mon <- read.csv('data/natural-flows-intervening-ac-ft.csv',skip=3)
	# Create a time series object
nat.mon.ts <- ts(nat,start=c(year.s,1),frequency=12)
	# number of sites
n.sites <- ncol(nat.mon)
	# number of times (months)
n.months <- frequency(nat.ts)
	# Aggregated annual series
nat.ann <- wapply(nat.ts,mean,12)
	# aggregated inteveneing flows, ac-ft/month
	# (equivalent to total flow at lees ferry)
nat.ann.tot <- ts(apply(nat.ann,1,sum),start=year.s,frequency=1) 
n.years <- length(time(nat.ann.tot))

	#array for obs common era data
data <- array(data=NA, dim=c(n.years, n.months, n.sites))

for(j in 1:n.sites)
	data[,,j] <- matrix(nat.mon.ts[,j], nrow=n.years, ncol=n.months, byrow=T)

nsim=100 # number of simulations	

###at this point, get sunthetic data for disag and assign to variable "seqs"
#synthetic data from KNN is based on ann flow at most DS location
seqs <- sim.knn(nat.ann.tot, 250)

