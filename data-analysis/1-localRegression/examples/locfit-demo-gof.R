{

#Open all the relevent libraries..
library(locfit)

# The commands below 
# 1. Fits a best 'local polynomial' model 
# 2. Computes the RSS from the local model and also from a linear regression
#    model
# 3. Computes the F statistic from the data and also from theory
# 4. If Fdata > Ftheory - reject null hypothesis (i.e, reject that the linear
#    model is a best fit to the data

# The commands also plots the linear model and the local model. 
#  If you have more than one independent variable in X - then comment the plotting
#  commands..

# Test data
data(trees)


#search for best alpha over a range of alpha values between 0,1

X=trees$Girth

Y=trees$Volume

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

zz=locfit(Y ~ X, alpha=bestalpha,deg=bestdeg,kern="bisq",scale=TRUE)
plot(zz, get.data=T, xlab="Tree Diameter", ylab="Volume of Timber")
abline(lsfit(X,Y), col="red")

# Now fit the LOCFIT model using the best alpha and degree obtained from
#above..
# compute RSS0, RSS1 
## test goodness of fit..

zz=locfit(Y ~ X, alpha=bestalpha,deg=bestdeg,kern="bisq",scale=TRUE, ev=dat())
RSS1 = sum(residuals(zz)^2)
nu1 = sum(fitted.locfit(zz,what="infl"))     # trace(L)
nu2 = sum(fitted.locfit(zz,what="vari"))     # trace(L^T L)
## Or
nu1 = zz$dp[6]
nu2 = zz$dp[7]
nu1 = N-2*nu1 + nu2

## Linear regression..

zz=lsfit(X,Y)
RSS0 = sum(residuals(zz)^2)
nu0 = length(zz$coef)		#number of model coefficients
nu0 = N-nu0

Fdata = (RSS0 - RSS1)/(nu0 - nu1)
Fdata = (Fdata / (RSS1 / nu1))

Ftheor = qf(0.95,(nu0-nu1), nu1)    #95% confidence level..

## Fdata > Ftheor   - reject null - i.e., data fits a linear model

}
