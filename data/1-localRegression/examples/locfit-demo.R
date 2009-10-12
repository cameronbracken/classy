{

#Open all the relevent libraries..
library(locfit)
library(sm)

f2dat=matrix(scan("f1.txt"),ncol=3,byrow=T)

f2dat=matrix(scan("f2.txt"),ncol=3,byrow=T)
#f2dat=matrix(scan("f3.txt"),ncol=3,byrow=T)

par(mfrow=c(2,3))

#search for best alpha over a range of alpha values between 0,1

X=f2dat[,2]

Y=f2dat[,1]

Ytrue=f2dat[,3]		#the true value of Y

#nvar=length(X[1,])	#numer of variables
nvar=1

N=length(Y)	#number ofdata points
porder=1
minalpha=2*(nvar*porder+1)/N
alpha1=seq(minalpha,1.0,by=0.05)
n=length(alpha1)


porder=2
minalpha=2*(nvar*porder+1)/N
alpha2=seq(minalpha,1.0,by=0.05)
alpha=c(alpha1,alpha2)

#get the GCV values for all the alpha values in alpha for order of
# polynomial = 1. kern="bisq" argument is to use the bisquare kernel
# in computing the weights of the neighbors, which are then used in
# the weighted least squares..

zz=gcvplot(Y ~ X, alpha=alpha1,deg=1,kern="bisq",ev=dat(),scale=TRUE)
z1=gcvplot(Y ~ X, alpha=alpha2,deg=2,kern="bisq",ev=dat(),scale=TRUE)

# pick the best alpha and the degree of the polynomial that
# gives the least GCV
z2=order(c(zz$values,z1$values))
deg1=1
if(z2[1] > n)deg1=2
bestalpha=alpha[z2[1]]
bestdeg=deg1

# Now fit the LOCFIT model using the best alpha and degree obtained from
#above..
zz=locfit(Y ~ X, alpha=bestalpha,deg=bestdeg,kern="bisq",scale=TRUE)

#Estimate the value of the function at the observed locations..
yest=predict.locfit(zz,X)

#get the residuals of the model..
x11=residuals(zz)

#plot the estimates from the model along with the original data
plot(X,Y, xlab="X", ylab="Y")
lines(X[order(X)],yest[order(X)])
#add the true function..
lines(X[order(X)], Ytrue[order(X)], col="red")

# if you just want to plot the locfit model output along with the observations
# then use this command. 
# Note this only works for a single variable regression. You have to use
# type="persp" or "contour" for two variable regression..
#plot(zz,get.data=T,xlab="X",ylab="Y")
 


# model diagnostics..
# 
# plot the histogram of the residuals and add the pdf. This is to 
# see how good the residuals fit a normal distribution..
# you can do a KS test on this, or a qqplot..)

hist(x11,probability=T)          
#fit and plot a normal distribution to the residuals..
lines(x11[order(x11)], dnorm(x11[order(x11)], mean=mean(x11), sd=sd(x11)), col="red")
lines(x11[order(x11)], dnorm(x11[order(x11)], mean=0, sd=sd(x11)), col="red"
sm.density(x11,add=T)         #nonparametric PDF   

#QQ plot..
qqnorm(x11)
qqline(x11)    

#plot the residuals against the X value
plot(X,x11,xlab="X",ylab="residuals") 
abline(h=0)

# plot the Y estimates agains the residuals..
plot(yest,x11,xlab="estimate of Y", ylab="residuals")
abline(h=0)

#plot the autocorrelation function - to see if the residuals are related
# to each other..
z1=acf(x11)

#########################

#do a x-validated prediction - i.e., drop a point and obtain its estimate using the rest.

#you can do it in many ways.. the default way is
# fit a locfit model with cross validation arguement

zcv=locfit(Y~X, alpha=bestalpha, deg=bestdeg, kern="bisq", ev=dat(cv=TRUE), scale=TRUE)

#cross validated estimates..
ycv = predict.locfit(zcv)

plot(Y, yest, xlab="Y", ylab="full data estimates")
plot(Y, ycv, xlab="Y", ylab="cross validated estimates")

## You can also do the leave one value out cross validation by

ycv=1:N
index=1:N
for(i in 1:N){

#get all the data points by dropping the ith point
index1=index[index != i]
xx=X[index1]
yy=Y[index1]

# the dropped point..
xp=X[i]

zl=locfit(yy ~ xx, alpha=bestalpha, deg=bestdeg, kern="bisq", scale=TRUE)
ycv[i]=predict.locfit(zl,xp)
}

plot(Y, ycv, xlab="Y", ylab="cross validated estimates")

}
