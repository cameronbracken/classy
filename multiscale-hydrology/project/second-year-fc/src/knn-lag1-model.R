### Code By Balaji

rm(list=ls())  #clear variables

test=matrix(scan("data/Leesferry-mon-data.txt"),ncol=13,byrow=T)
test=test[,2:13]/10000
#you have to string it to create a long vector
x=array(t(test))
N=length(x)
N_mo = N/12
nyrs = length(test[,1])
nyrs1 = nyrs-1

#source("skew.s")

May = test[,5]

xeval=seq(min(May)-0.25*sd(May),max(May)+0.25*sd(May),length=100)
nevals=length(xeval)

library(sm)
zz=sm.density(May,eval.points=xeval,display="none")
xdensityorig=zz$estimate

#Fitting Np-AR model

K=round(sqrt(N))
W=1:K
W=1/W
W=W/sum(W)
W=cumsum(W)

nsim=50	#generate 100 simulates each of length N
nsim1 = nsim+1

#initialize the matrices that store the statistics
# the first row contains the statistic of the historical data. 

armean=matrix(0,nsim1,12)	
arstdev=matrix(0,nsim1,12)
arcor=matrix(0,nsim1,12)
arskw=matrix(0,nsim1,12)
#maysim=matrix(0, nyrs, nsim)
maysim=1:nyrs
simpdf=matrix(0,nrow=nsim,ncol=nevals)

#based on MI we identify the lag to be

ilag=1

for (isim in 1:nsim) {

    # pick a random starting year
	i=round(runif(1,2,nyrs))

	zsim = 1:N

	ilag1 = ilag -1
	index = (i-1)*12+1
	zsim[1:ilag] = x[index:(index+ilag1)]
	xp = zsim[1:ilag]

	for (j in (ilag+1):N) {
	
		mo = j%%12 # this is the month we are simulating
	
	
		if (mo == 0) { # if mo = 0 (Dec) 
		  data = test 
		}  # data is the same as test (Jan - Dec)
		if (mo > 0) {
		  data = cbind(test[,(mo+1):12],test[,1:mo])
		}# if mo = 2 (Feb), 1st col of data will be March, last col will be Feb
		
		if (ilag == 1) {
		  xdist = order(abs(xp - data[,11]))
		}
	  if (ilag >1) {
	    xx=rbind(xp, data[,(12-ilag):11]) 
	    xdist = order(as.matrix(dist(xx))[1,2:nyrs+1]) 
	  }

		xx=runif(1,0,1)
		xy=c(xx,W)
		xx=rank(xy)
		i1=xdist[xx[1]]
	
		ydata = data[,12] # last column of data is always month we are simulating
		zsim[j]=ydata[i1]
		xp=zsim[(j-ilag+1):j]
		browser()
	} # end j loop

	simdismon = t(matrix(zsim, nrow=12)) # makes a 12 column matrix with jan thru dec
	maysim = simdismon[,5]
	simpdf[isim,]=sm.density(maysim,eval.points=xeval,display="none")$estimate
	#______________________________________________
	#***calculate basic statistics****************
	#______________________________________________

	k1=isim+1
	for(j in 1:12){

                armean[k1,j]=mean(simdismon[,j])
                arstdev[k1,j]=sd(simdismon[,j])
		arskw[k1,j]=skew(simdismon[,j])
        } # end j in 1:12

	for(j in 2:12) {
		j1=j-1
               	arcor[k1,j]=cor(simdismon[,j],simdismon[,j1])
        }
	arcor[k1,1]=cor(simdismon[1:nyrs1,12],simdismon[2:nyrs,1])

} # end isim loop

#_______________________________________
#Observed Data Stats

obsmean=1:12
obsstdev=1:12
obscor=1:12
obsskw=1:12
obsmax=1:12
obsmin=1:12

for(i in 1:12){

        obsmean[i]=mean(test[,i])
        obsstdev[i]=sd(test[,i])
	obsskw[i]=skew(test[,i])

}
for(i in 2:12){
i1=i-1
        obscor[i]=cor(test[,i], test[,i1])
}
obscor[1]= cor(test[1:nyrs1,12], test[2:nyrs,1])

        armean[1,1:12]=obsmean[1:12]
        arstdev[1,1:12]=obsstdev[1:12]
        arskw[1,1:12]=obsskw[1:12]
        arcor[1,1:12]=obscor[1:12]

#write the stats in separate files..

write(t(arcor),file="arcorrs1",ncol=12)
write(t(armean),file="armeans",ncol=12)
write(t(arstdev),file="arstdevs",ncol=12)
write(t(arskw),file="arskews",ncol=12)

#write(t(maysim), file="Maysims.txt", ncol = nsim)

#boxplots of the PDF....
par(mfrow=c(1,1))

zz=boxplot(split(simpdf,col(simpdf)),plot=F,cex=1.0)
zz$names=rep("",length(zz$names))
z1=bxp(zz,ylim=range(simpdf,xdensityorig),xlab="",ylab="",cex=1.25)

z2=1:6
n1=1:6
z2[1]=z1[1]
z2[2]=z1[20]
z2[3]=z1[40]
z2[4]=z1[60]
z2[5]=z1[80]
z2[6]=z1[100]

n1[1]=xeval[1]
n1[2]=xeval[20]
n1[3]=xeval[40]
n1[4]=xeval[60]
n1[5]=xeval[80]
n1[6]=xeval[100]
n1=round(n1,dig=0)
n1=as.character(n1)

axis(1,at=z2,labels=n1,cex=1.00)
lines(z1,xdensityorig,lty=2,lwd=2,col="red")

