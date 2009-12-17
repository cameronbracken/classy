source('lib.R')
flows <- as.matrix(read.table("data/AMJJFLOW-49-99.txt"))/(10^3)
swe <- as.matrix(read.table("data/March1SWE-1949-99.txt"))

X=swe
Y=flows

swemeans=colMeans(X)
swesds=apply(X,2,sd)
flowmeans=colMeans(flows)
flowsds=apply(flows,2,sd)

#-----  CCA ----
#

X1=scale(X)
Y1=scale(Y)


#### PCA on X and retain the first 6 PCS
Sxx=var(X)
xeof=svd(Sxx)
xpcs = X %*% xeof$u
X=xpcs[,1:6]


######## Perform CCA on the 6 PCs of SWE with the six streamflows.
X1=scale(X)
Y1=scale(Y)

M=dim(X)[2]
J=dim(Y)[2]
J=min(M,J)
N = length(Y[,1])

Qx1 = qr.Q(qr(X1))
Qy1 = qr.Q(qr(Y1))
T11 = qr.R(qr(X1))
T22 = qr.R(qr(Y1))

VV=t(Qx1) %*% Qy1
BB = svd(VV)$v
AA = svd(VV)$u 

BB = solve(T22) %*% svd(VV)$v * sqrt(N-1)
wm1=Y1 %*% BB


AA = solve(T11) %*% svd(VV)$u * sqrt(N-1)
vm1 = X1 %*% AA

cancorln = svd(VV)$d[1:J]   #canonical correlation

Fyy = var(Y1) %*% BB
#--------------------

#U = XA   W = YB; Relate Yhat = U*betahat; betahat = (t(U)U)^-1 t(U) Y


betahat = solve(t(AA) %*% t(X1)%*% X1 %*% AA) %*% t(AA) %*% t(X1) %*% Y1

ypred=X1 %*% AA %*% betahat

## scale back the data
ypred=t(t(ypred)*flowsds + flowmeans)


#---------------------------------

#To predict -  We drop a point, perform the CCA on the rest of the data
# compute the by and ax matrices; compute the wm vector for the dropped point
# Estimate the vector Y for the dropped point by backtransformation EQn (9.54)

# Mean prediction - i.e. a single prediction for each point (or year in our case)


N = length(flows[,1])

ypred=matrix(0,nrow=N,ncol=J)

index=1:N

for(i in 1:N){

	#drop a point..
	index1=index[index != i]

	xp=swe[i,]   #prediction point


	#get the rest of the data..
	X=swe[index1,]
	swemeans=colMeans(X)
	swesds=apply(X,2,sd)

	Y=flows[index1,]
	flowmeans=colMeans(Y)
	flowsds=apply(Y,2,sd)

	## scale the data to be predicted..
	xp = (xp-swemeans)/swesds


	# get the first 6 PCS of SWE
	X=scale(X)
	Sxx=var(X)
	xeof=svd(Sxx)
	xpcs=X %*% xeof$u
	xpcs=xpcs[,1:6]
	X=xpcs

	### Perform CCA on the 6 PCS of SWE and six streamflows..
	X1 = scale(X)
	Y1=scale(Y)
	Qx1 = qr.Q(qr(X1))
	Qy1 = qr.Q(qr(Y1))

	VV=t(Qx1) %*% Qy1

	T11 = qr.R(qr(X1))
	T22 = qr.R(qr(Y1))

	BB = solve(T22) %*% svd(VV)$v * sqrt(length(Y1[,1]-1))
	BB = svd(VV)$v
	wm1=Y1 %*% BB

	Fyy = var(Y1) %*% BB

	AA = solve(T11) %*% svd(VV)$u * sqrt(length(Y1[,1]-1))
	AA = svd(VV)$u
	vm1 = X1 %*% AA
	cancorln = svd(VV)$d[1:J]  

	#### get the regression coefficient..

	betahat = solve(t(AA) %*% t(X1)%*% X1 %*% AA) %*% t(AA) %*% t(X1) %*% Y1

	## get the first six Pc values of xp
	vmp = (xp %*% xeof$u)[,1:6] 
	vmp = vmp %*% AA


	### predict
	yp = vmp %*% betahat

	### scale back

	yp = yp*flowsds + flowmeans

	ypred[i,]=yp

}
ypred.cca <- ypred
ypred.cca[ypred.cca < 0] <- 0
save(ypred.cca,file='output/5.Rdata')