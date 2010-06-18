####KNN flow sequence generation of length "l" 
nsim=100
l=50


dat = an_flw #matrix(scan("input/caracas_ann.txt"), ncol=2, byrow=TRUE) #read in data		

nyear=length(dat[,1]) #number of years in observed data, which will also be length of each sequence

data_d = dat[1:(nyear-1),1:2] # data to be used to calculate distances 




k=sqrt(nyear) #number of neighbors
	
	

sequencematrix = matrix(nrow=l, ncol=nsim) #defines matrix to hold flow sequences

	for(n in 1:nsim){

	yr=sample(dat[,1], 1, replace = TRUE) #randomly select one of the observed years
	
	sequencematrix[1,n]=yr #Fills sequence matrix with each iteration (provides year) 

		for(i in 2:l){
		 

		Flow=dat[(yr-(dat[1,1]-1)),2] #Flow corresponding to the "current" year
			
		D=abs(data_d[,2]-Flow) #calculates difference between selected flow and other observed flows
		
		Delta=cbind(data_d[,1],D) #combines difference and corresponding year into one matrix
		
		Delta_sort=cbind(Delta[,1][order(Delta[,2])], sort(Delta[,2]))	#reorders the delta matrix based on distances	

		kmatrix = Delta_sort[1:k,1:2] #selects the "k-nearest-neighbors" from Delta_sort 

		weight = matrix(nrow=k, ncol=1) # defines matrix for weights
 		
		rnk=rank(kmatrix[,2]) #ranks distances for purpose of generating weights
		
		for(j in 1:k){
	
		weight[j,1] = 1/(rnk[j]) #fills weighting matrix
	
		}

		z = sum(weight) # sums weights 

		weights = weight/z	#divides weights by sum of weights so cumulative probability = 1
		

		NN=sample(kmatrix[,1], 1, replace = TRUE, prob=weights) 	#Selects a year to be "nearest neighbor"
		
				

		yr= NN+1

		sequencematrix[i,n]=yr #Fills sequence matrix with each iteration (provides year) 
		
		}

	}

#takes sequence of years from sequence_matrix file and retrives corresponding flows


seqf = sequencematrix

nyear= length(seqf[,1])

X = matrix(nrow=nyear, ncol=nsim)


for(j in 1:nsim){

for(i in 1:nyear){

	yr = seqf[i,j]
	yra = yr-(dat[1,1]-1)  #identifies location in matrix 
	X[i,j] = dat[yra, 2] #pairs flow with year
}
}

write(t(X), file="results/KNN_sj.txt", ncol=nsim) #writes a new files containing the new flow sequence