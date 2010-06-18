source("functions/skew.R")
#######get data and format


	#reads in daily data [acre-ft]
d1 <- as.matrix(read.table("data/caracas_daily_af.txt"))
d2 <- as.matrix(read.table("data/pagosa_daily_af.txt"))
d3 <- as.matrix(read.table("data/Nav_daily_af.txt"))
d4 <- as.matrix(read.table("data/L_nav_daily_af.txt"))

obs_ann <- matrix(scan("data/caracas_ann.txt"), ncol=2, byrow=T) #reads in annual data acre-ft caracas

	#picks start year for obs period
Y_s <- max(d1[1,1],d2[1,1],d3[1,1],d4[1,1]) 
	#picks end year from all obs data - i.e. common era identified
Y_e <- min(d1[nrow(d1),1],d2[nrow(d2),1],d3[nrow(d3),1],d4[nrow(d4),1]) 

#subset common era
od1 <- subset(d1, d1[,1]>=Y_s & d1[,1]<=Y_e)
od2 <- subset(d2, d2[,1]>=Y_s & d2[,1]<=Y_e)
od3 <- subset(d3, d3[,1]>=Y_s & d3[,1]<=Y_e)
od4 <- subset(d4, d4[,1]>=Y_s & d4[,1]<=Y_e)


OD=cbind(od1, od2, od3, od4)


L <- Y_e - Y_s + 1 #length of common era



dmat=array(data=NA,dim=c(L,365,4)) #array for obs common era data

y1=Y_s # first year of observed data

#this loop puts observed data in to the observed data matrix
#it removes  Feb. 29th if it is a leap year and then corrects the mean flow accordingly
#this makes for all years with 365 days

P=1

for(j in 1:4){

obs_daily=OD[,P:(P+1)]

P=P+2

for(i in 1:L){


x=subset(obs_daily[,2], obs_daily[,1]==(y1+i-1))

	if(length(x)>365){

		
	x=c(x[1:59],x[61:366])

	 ##****29th flow volume******#
	
	}

dmat[i,,j]=x

}
}


#### this is the "correcting" due to forced 365 day year
#the index gauge (used for disag) is recomputed as the sum of all gauges for all days in a year
#thiws makes for a value that isn't "real" since obs data is not intervening, but fine to condition disag on

IG=matrix(ncol=2, nrow=L)


for(i in 1: L){

IG[i,2]=sum(dmat[i,,])

}

IG[,1]=Y_s:Y_e

######



#re-assign variables to make combined code mesh

data=dmat 

an_flw= IG 


#this loop uses the annual flow (actually index gauge (IG)) at most DS location to do disag
#an_flw=matrix(ncol=2, nrow=L)
#for( i in 1:L){
#an_flw[i,2]=sum(data[i,,1])
#}

an_flw[,1]=Y_s:Y_e


 # observed annual flow for picking disag yr

len=length(data[,1,1]) # how many yrs of observed data



nsim=100 # number of simulations	

###at this point, get sunthetic data for disag and assign to variable "seqs"
#synthetic data from KNN is based on ann flow at most DS location
source("functions/SJ_knn.R")
seqs=X

##use ar1 to model at index gauge
#source("code\\SJ_ar1.txt")
#seqs=traces


l= length(seqs[,1]) # length of each simulation (yrs)

index_mat=matrix(ncol=nsim, nrow=l) # matrix for recording yr index for disag (if need be)

sf_mat= matrix(ncol=nsim, nrow=l) #matrix for recording scale factors (if need be)

disag=array(data=NA,dim=c(l,365,nsim,4)) #matrix for disag values - row(yr), col (month), index1 (sim), index2 (site)


# experimental loop to correct Dec Jan continuity
# instead of looking at 12/31 only, this approach looks at 15 days 

#p2=1:len

#for(i in 1:len){
#p2[i]=sum(data[i,362:365])
#}

#Phi coefficients for distance formula - look only at 12/31

#ph1=1/(var(an_flw[,2]))
#ph2=1/(var(data[,365,1]))
#ph2=1/(var(p2))



# begin disag

for(j in 1:nsim){

#this picks the 1st year for disag based only on the annual flow b/c there is no prev. Dec
	
	k=sqrt(len) #number of neighbors
	#k=20

	Flow=seqs[1,j] #first annual value in each trace to be disagged
	
	D=abs(an_flw[,2]-Flow) # distance between observed years and simulated value
	
	Delta=cbind(an_flw[,1],D) #combines difference and corresponding year into one matrix
	
	Delta_sort=cbind(Delta[,1][order(Delta[,2])], sort(Delta[,2])) #reorders the delta matrix based on distances
	
	kmatrix = Delta_sort[1:k,1:2] #selects the "k-nearest-neighbors" from Delta_sort 
	
	weight = matrix(nrow=k, ncol=1) # defines matrix for weights
 		
	rnk=rank(kmatrix[,2]) #ranks distances for purpose of generating weights
		
		for(i in 1:k){
	
			weight[i,1] = 1/(rnk[i]) #fills weighting matrix
	
		}

	z = sum(weight) # sums weights 

	weights = weight/z	#divides weights by sum of weights so cumulative probability = 1
		
	N=sample(kmatrix[,1], 1, replace = TRUE, prob=weights) #Selects a year to be "nearest neighbor"
		

	pos=N-(an_flw[1,1]-1) # index for selected yr

	SF=Flow/(an_flw[pos,2]) # scaling factor to apply for disag


	sf_mat[1,j]=SF # records scale factor
	index_mat[1,j]=pos
	
        disag[1, ,j,]=data[pos,,]*SF # "disaggregation"

# now that one year has been disaggregated, the remaining years in the trace use annual flow and also december of last yr (CY data)

		for(h in 2:l){


		k=sqrt(len) #number of neighbors
		
		#k=20
		Flow=seqs[h,j] # annual flow value for disaggregation
		
		D=2:len #matrix for distances, length is (len-1) b/c first observed year has no prev. december
	
        		
			###Distances based on the 2 distance method 

			#for(g in 2:len){
 			
			#D[g-1]=sqrt((ph1*(Flow-an_flw[g,2])^2) + (ph2*(disag[(h-1),365,j,1]-data[(g-1),365,1])^2))
			#D[g-1]=sqrt((ph1*(Flow-an_flw[g,2])^2) + (ph2*(sum(disag[(h-1),362:365,j])-p2[(g-1)])^2))
				
			#}
			#Delta=cbind(an_flw[2:len,1],D) #combines difference and corresponding year into one matrix
			

			###distance based only on annual flow
			
			D=abs(an_flw[,2]-Flow)
			Delta=cbind(an_flw[,1],D) #combines difference and corresponding year into one matrix mean flow 


			Delta_sort=cbind(Delta[,1][order(Delta[,2])], sort(Delta[,2])) #reorders the delta matrix based on distances
		
			kmatrix = Delta_sort[1:k,1:2] #selects the "k-nearest-neighbors" from Delta_sort 
	
			weight = matrix(nrow=k, ncol=1) # defines matrix for weights
 		
			rnk=rank(kmatrix[,2]) #ranks distances for purpose of generating weights
		
			for(i in 1:k){
	
			weight[i,1] = 1/(rnk[i]) #fills weighting matrix
	
			}

			z = sum(weight) # sums weights 
	
			weights = weight/z	#divides weights by sum of weights so cumulative probability = 1
		
			N=sample(kmatrix[,1], 1, replace = TRUE, prob=weights) #Selects a year to be "nearest neighbor"
		
			pos=N-(an_flw[1,1]-1) # index for selected yr
	
			SF=Flow/(an_flw[pos,2]) # scaling factor to apply for disag

			sf_mat[h,j]=SF
       

			disag[h, , j,]=data[pos, ,]*SF #disaggregation
       			
			}
			}
			


## output to "flat" file


disagmat=matrix(ncol=4, nrow=(l*nsim*365))


for(j in 1:4){
p=1
P=(l*365)
for(k in 1:nsim){

disagmat[p:P,j]= as.vector(t(disag[,,k,j]))
p=p+(l*365)
P=P+(l*365)
}

}