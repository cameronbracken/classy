#REad in the entire global SST data - annual average at each grid
#location 5deg x 5deg grid

test=matrix(scan("kaplan-sst-wy-1925-2003-revised.txt"),ncol=3,byrow=T)
sst=t(matrix(test[,3],ncol=79,byrow=T))

# Read in the latitude and longitude of the locations
locs=matrix(scan("kaplan-sst-wy-1925-2003-II.txt"),ncol=2,byrow=T)
lats=locs[,1]
longs=locs[,2]
nlocs=length(lats)


longs[longs <0]=longs[longs < 0]+360

#define the region you wish to get the data..
#say 20S and north and the Pacific domain with little Atlantic domain..

xlongs=longs[lats >= -20 & longs >=120 & longs <= 300]
xlats=lats[lats >= -20 & longs >=120 & longs <= 300]

index=1:nlocs
index=index[lats >= -20 & longs >=120 & longs <= 300]

###############
#for Indian Ocean

#xlongs=longs[lats >= -20 & lats <= 30 & longs >=40 & longs <= 120]
#xlats=lats[lats >= -20 & lats <= 30 & longs >=40 & longs <= 120]

#index=1:nlocs
#index=index[lats >= -20 & lats <= 30 & longs >=40 & longs <= 120]

############################

# get the data for the desired domain..
sstdata=sst[,index]

#get variance matrix..
zs=var(sstdata)

#do an Eigen decomposition..
zsvd=svd(zs)

#Principal Components...
pcs=t(t(zsvd$u) %*% t(sstdata))

#Eigen Values.. - fraction variance 
lambdas=(zsvd$d/sum(zsvd$d))

plot(1:40, lambdas[1:40], type="l", xlab="Modes", ylab="Frac. Var. explained")
points(1:40, lambdas[1:40], col="red")


#plots..
#plot the first spatial component or Eigen Vector pattern..
library(maps)
library(akima)

zz=interp(xlongs-360,xlats,zsvd$u[,1])
image(zz)
map('world',add=TRUE)

}
