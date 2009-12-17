{

test=matrix(scan("pdsi-wy-1925-2003.txt"),ncol=3,byrow=T)
pdsi=t(matrix(test[,3],ncol=79,byrow=T))

locs=matrix(scan("pdsi-wy-1925-2003-II.txt"),ncol=2,byrow=T)
lats=locs[,1]
longs=locs[,2]
nlocs=length(lats)


longs[longs <0]=longs[longs < 0]+360

#define the region you wish to get the data..
#get PDSI data for N. America..

xlongs=longs[lats >= 25 & lats <= 47 & longs >=230 & longs <= 300]
xlats=lats[lats >= 25 & lats <= 47 & longs >=230 & longs <= 300]

index=1:nlocs
index=index[lats >= 25 & lats <= 47 & longs >=230 & longs <= 300]

###############

pdsidata=pdsi[,index]

#get variance matrix..
zs=var(sstdata)

#do an Eigen decomposition..
zsvd=svd(zs)

#Principal Components...
pcs=t(t(zsvd$u)%*% t(sstdata))

#Eigen Values..
lambdas=(zsvd$d/sum(zsvd$d))

#plot the PCS and the Eigen Vectors..
#e.g.,
library(akima)
library(maps)

zz=interp(xlongs,xlats,zsvd$u[,1])

contour(zz)
map('world',add=TRUE)

}
