proportion.disag <- function(x,hist,n.sim=250,l=nrow(x)){
	
	#takes a matrix of simulations
	
	# begin disag
	cat('Disaggregating...')
	
		# length of each simulation (yrs)
	l <- nrow(x)
	n.sites <- dim(x)[3]
	n.times <- ncol(x)

		#matrix for recording scale factors (if need be)
	sf_mat <- matrix(ncol=n.sim, nrow=l) 

		#matrix for disag values - row(yr), col (month), index1 (sim), index2 (site)
	disag <- array(data=NA,dim=c(l,n.times,n.sim,n.sites))
	
		#number of neighbors
	k <- sqrt(l) 
	
	for(j in 1:n.sim){

		# this picks the 1st year for disag based only on 
		# the annual flow b/c there is no prev. Dec
			
			#first annual value in each trace to be disagged
		this.flow <- seqs[1,j]
	
			# distance between observed years and simulated value
		D=abs(an_flw[,2]-this.flow) 
	
			#combines difference and corresponding year into one matrix
		Delta=cbind(an_flw[,1],D)
	
			#reorders the delta matrix based on distances
		Delta_sort=cbind(Delta[,1][order(Delta[,2])], sort(Delta[,2]))
	
			#selects the "k-nearest-neighbors" from Delta_sort 
		kmatrix = Delta_sort[1:k,1:2]
	
			# defines matrix for weights
		weight = matrix(nrow=k, ncol=1) 
 			
			# ranks distances for purpose of generating weights
		rnk=rank(kmatrix[,2])
		
		for(i in 1:k)	
			weight[i,1] = 1/(rnk[i])

			# sums weights 
		z = sum(weight) 

			#divides weights by sum of weights so cumulative probability = 1
		weights = weight/z
		
			#Selects a year to be "nearest neighbor"
		N=sample(kmatrix[,1], 1, replace = TRUE, prob=weights) 
		
			# index for selected yr
		pos=N-(an_flw[1,1]-1) 

			# scaling factor to apply for disag
		SF=Flow/(an_flw[pos,2])
			# records scale factor
		sf_mat[1,j]=SF
		index_mat[1,j]=pos
	
			# "disaggregation"
        disag[1, ,j,]=data[pos,,]*SF 

		# now that one year has been disaggregated, the remaining years 
		# in the trace use annual flow and also december of last yr (CY data)

		for(h in 2:l){

			#number of neighbors
		k=sqrt(len) 
			# annual flow value for disaggregation
		Flow=seqs[h,j]
			#matrix for distances, length is (len-1) 
			# b/c first observed year has no prev. december
		D=2:len
	
		###distance based only on annual flow
	
		D=abs(an_flw[,2]-Flow)
			# combines difference and corresponding year 
			# into one matrix mean flow 
		Delta=cbind(an_flw[,1],D) 

			#reorders the delta matrix based on distances
		Delta_sort=cbind(Delta[,1][order(Delta[,2])], sort(Delta[,2])) 

			#selects the "k-nearest-neighbors" from Delta_sort 
		kmatrix = Delta_sort[1:k,1:2]

			# defines matrix for weights
		weight = matrix(nrow=k, ncol=1) 
	
			#ranks distances for purpose of generating weights
		rnk=rank(kmatrix[,2]) 
			#fills weighting matrix
		for(i in 1:k){
				weight[i,1] = 1/(rnk[i])

			# sums weights 
		z = sum(weight) 
			#divides weights by sum of weights so cumulative probability = 1
		weights = weight/z

		N=sample(kmatrix[,1], 1, replace = TRUE, prob=weights) #Selects a year to be "nearest neighbor"

		pos=N-(an_flw[1,1]-1) # index for selected yr

		SF=Flow/(an_flw[pos,2]) # scaling factor to apply for disag

		sf_mat[h,j]=SF
     

		disag[h, , j,]=data[pos, ,]*SF #disaggregation
     			
		}
	}
		


	## output to "flat" file


	disagmat=matrix(ncol=4, nrow=(l*n.sim*365))


	for(j in 1:4){
	p=1
	P=(l*365)
	for(k in 1:n.sim){

	disagmat[p:P,j]= as.vector(t(disag[,,k,j]))
	p=p+(l*365)
	P=P+(l*365)
	}

	}

	cat('Done.')
}