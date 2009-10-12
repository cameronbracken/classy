{
library(locfit)
library(sm)
library(akima)
library(fields)

f2dat=matrix(scan("bangkok-1998-welldata.txt"),ncol=3,byrow=T)

par(mfrow=c(2,3))

#search for best alpha over a range of alpha values between 0,1

X=f2dat[,1:2]

Y=f2dat[,3]

nvar=length(X[1,])	#numer of variables
N=length(Y)	#number ofdata points
porder=1
minalpha=2*(nvar*porder+1)/N
alpha=seq(minalpha,1.0,by=0.05)
n=length(alpha)


porder=2
minalpha=2*(2*nvar*porder+1)/N
alpha1=seq(minalpha,1.0,by=0.05)
alpha2=c(alpha,alpha1)

#get the GCV values for all the alpha values in alpha for order of
# polynomial = 1. kern="bisq" argument is to use the bisquare kernel
# in computing the weights of the neighbors, which are then used in
# the weighted least squares..

zz=gcvplot(Y ~ X, alpha=alpha,deg=1,kern="bisq",ev=dat(),scale=TRUE)
z1=gcvplot(Y ~ X, alpha=alpha1,deg=2,kern="bisq",ev=dat(),scale=TRUE)

# pick the best alpha and the degree of the polynomial that
# gives the least GCV
z2=order(c(zz$values,z1$values))
deg1=1
if(z2[1] > n)deg1=2

bestalpha = alpha2[z2[1]]
bestdeg=deg1

# Now fit the LOCFIT model using the best alpha and degree obtained from
#above..
zz=locfit(Y ~ X, alpha=bestalpha,deg=bestdeg,kern="bisq",scale=TRUE)

#get the residuals of the model..
x11=residuals(zz)

# residuals = Y - Yestimate ==> Yestimate = Y - residuals
yest=f2dat[,3]-x11

#plot the image and contours of the estimates from the model..

 plot(zz,xlab="X",ylab="Y", type="image",col=rainbow(100))
 plot(zz,xlab="X",ylab="Y", type="contour",add=T,nlevel=5,lwd=2)


# if you change type ="persp" you will get a perspective plot - i.e
#surface plot..

# model diagnostics..
# (2(i))
# plot the histogram of the residuals and add the pdf. This is to 
# see how good the residuals fit a normal distribution..
# you can do a KS test on this, or a qqplot..)
hist(x11,probability=T)          
sm.density(x11,add=T) 

#QQplot
qqnorm(x11)
qqline(x11)
               

#plot the residuals against the Yest value
plot(yest,x11,xlab="Y",ylab="residuals") 

#plot the autocorrelation function - to see if the residuals are related
# to each other..
z1=acf(x11)

#plot the residuals against the X value
plot(X[,2],x11,xlab="Y",ylab="residuals") 


#----------------------------------------
# Estimate the function at several points on a grid..
xpred=seq(42.5,117.5,by=5)
ypred=seq(-152.5, -67.5,by=5)


nx=length(xpred)
ny=length(ypred)

predloc=matrix(0,nrow=nx*ny,ncol=2)

n=0
for(i in 1:nx){
  for(j in 1:ny){
n=n+1
predloc[n,1]=xpred[i]
predloc[n,2]=ypred[j]}}

# predict at the prediction points - i.e predloc matrix..
# 2(ii)
Xy=X

z1=predict.locfit(zz, predloc, se.fit=T)

# You might want to plot these two surfaces separately
#
#2(iv)
#plot the surface of the estimates from the model..


xxun=unique(predloc[,1])
yyun=unique(predloc[,2])
zzmat=matrix(z1$fit,nrow=ny,ncol=nx)

#persp(xxun,yyun,t(zzmat),theta=30,phi=30,expand=0.5,shade=0.5,col="cyan",
#ltheta=-30,xlab="Easting", ylab="Northing",zlab="Piezzometric heights")

image.plot(xxun,yyun,t(zzmat),xlab="Easting", ylab="Northing",col=rainbow(100), zlim=range(0,90))
contour(xxun,yyun,t(zzmat), lwd=2, nlevels=5,add=T)

#plot the error estimates  from the model
zzmat=matrix(z1$se,nrow=ny,ncol=nx)

#persp(xxun,yyun,t(zzmat),theta=30,phi=30,expand=0.5,shade=0.5,col="cyan",
#ltheta=-30,xlab="Easting", ylab="Northing",zlab="Standard error")

image.plot(xxun,yyun,t(zzmat),xlab="Easting", ylab="Northing",col=rainbow(100), zlim=c(0,10))
contour(xxun,yyun,t(zzmat), lwd=2, nlevels=5,add=T)


## If you wish to evaluate the function eacth point from first principles..
##
zz=locfit(Y ~ X, alpha=bestalpha,ev=dat(),deg=bestdeg,kern="bisq",scale=TRUE)
RSS1 = sum(residuals(zz)^2)
nu1 = sum(fitted.locfit(zz,what="infl"))     # trace(L)
nu2 = sum(fitted.locfit(zz,what="vari"))     # trace(L^T L)
sigmae = RSS1 / (N - 2*nu1 + nu2)	# error variance

npred = length(predloc[,1])   # number of points to predict..
ypred = 1:npred
sigpred = 1:npred		#standard error
for(i in 1:npred){
lx=locfit(Y ~ X, alpha=bestalpha,deg=bestdeg,kern="bisq",scale=TRUE,ev=predloc[i,],geth=1)
ypred[i]=sum(lx*Y)
sigpred[i]=sqrt(sigmae)*sqrt(sum(lx*lx))		#confidence interval
}

zzmat=matrix(ypred,nrow=ny,ncol=nx)
image.plot(xxun,yyun,t(zzmat),xlab="Easting", ylab="Northing",col=rainbow(100),zlim=range(0,90))
contour(xxun,yyun,t(zzmat), lwd=2, nlevels=5,add=T)

# the standard error..
zzmat=matrix(sigpred,nrow=ny,ncol=nx)
image.plot(xxun,yyun,t(zzmat),xlab="Easting", ylab="Northing",col=rainbow(100), zlim=range(0,10))
contour(xxun,yyun,t(zzmat), lwd=2, nlevels=5,add=T)

# Check with Fields..
fit = Tps(X,Y)
surface(fit)


}
