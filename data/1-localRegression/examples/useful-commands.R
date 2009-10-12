### some useful commands to implementing local polynomials..
### also some useful commands from LOCFIT


### xp is the point of estimate coule be a single value or vector
## K = round(alpha*N)
## define L
L = matrix(0,nrow=N, ncol=N)
yest = 1:N

### define yest

xp=x[i,]
xx=as.matrix(dist(scale(x)))      #original data in x
xdist=xx[i,]
distorder = order(xdist)      #order the distaances - gives the place order in the original matrix of the sorted values
distk = xdist[distorder[K]]   # distance to the Kth neighbor
xdist[xdist >= distk]=distk    # make all distances greater than distk = distk
weights = 15*(1-(xdist/distk)2)2
W = diag(weights)      #diagonal matrix

## get the augmented matrix to the Taylor series expansion..
ap = x-xp
xkk = cbind(rep(1,N),ap,ap2)
yk=y

betai = solve(t(xkk)%*%weights%*%xkk) %*% (t(xkk) %*% weights %*% yk)

evect = rep(0,nparam)		#create e_1 vector
evect[1]=1
Lx = evect%*%(solve(t(xk)%*%weights%*%xk) )%*% (t(xk) %*% weights)

yest[i]=sum(Lx*y)
############

zz=locfit(y ~ x, alpha=bestalpha, deg=bestdeg, kern="bisq", ev=dat(), scale=TRUE)

nu1 = zz$dp[6]		# trace(L)
nu2 = zz$dp[7]     #trace(L^T L)

#If you want to get the L matrix..

L=locfit(y ~ x, alpha=bestalpha, deg=bestdeg, geth=1,kern="bisq", ev=dat(), scale=TRUE)

# L is now the hat matrix..
## If you wish to estimate/predict the function using the first
# principles  - i.e., implementing the locfit at each point rather
# using the default interpolation..

#Then use the following..


lx=locfit(y ~ x, alpha=bestalpha, deg=bestdeg, kern="bisq", ev=xp, geth=1,scale=TRUE)
yest = sum(lx*y)

colojan=matrix(scan("colo_monthly_precip_01.txt"), ncol=4, byrow=T)

index=1:length(colojan[,1])
index1=index[colojan[,4] >= 0]
length(index1)
[1] 449

The above steps gets only the rows that have 'non-missing' values.. (missing value is flagged as -999.99)

lat = colojan[index1,1]
long=colojan[index1,2]
elev=colojan[index1,3]

janprecip = colojan[index1,4]

xmat = cbind(lat,long.elev)
xmat = as.data.frame(xmat)
## now the data frame xmat has columns that are named lat, long, elev
## as you can see by just typing the first row..
>xmat[1,]
xmat[1,]
    lat     long     elev
 37.401 -104.655 1950.720

>zzlin = lm(janprecip ~ ., data=xmat)      #note that xmat is already a data frame.

## read in the DEM
colodem=matrix(scan("colo_dem.txt"), ncol=3, byrow=T)

## Now set xmat to colodem
xmat = colodem

DEMestlin=predict.lm(zzlin,as.data.frame(xmat),se.fit=T)

DEMestlin$fit now has 1705 values and so does DEMestlin$se 

