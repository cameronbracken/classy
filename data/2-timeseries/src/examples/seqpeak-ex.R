pdf()
# Reading in the data
x=matrix(scan("boise.txt"),ncol=3,byrow=T)
# Assigning variables
yr=x[,1]
mo=x[,2]
flow=x[,3]
ym=yr+mo/12
plot(ym,flow,type="l",xlab="Year",ylab="cfs")
title("Boise River Streamflow")


#  formating the flows into a matrix
fmat=matrix(flow,ncol=12,byrow=T)


#  computing average across years for each month
fmave=rep(0,12)
for(i in 1:12){ 
fmave[i]=mean(fmat[,i],na.rm=T)
}

plot(1:12,fmave,type="l",xaxt="n",ylab="Mean flow cfs",xlab="Month")
axis(1,at=1:12,labels=c(10:12,1:9))


# computing variance for each month
fmvar=rep(0,12)
for(i in 1:12){ 
fmvar[i]=var(fmat[,i],na.rm=T)
}
plot(1:12,fmvar,type="l",xaxt="n",ylab="Variance flow cfs^2",xlab="Month")
axis(1,at=1:12,labels=c(10:12,1:9))


# Defining a standard deviation function
sdev=function(x){sqrt(var(x,na.rm=T))}

# computing Std deviation for each month
fsdev=rep(0,12)
for(i in 1:12){ 
fsdev[i]=sdev(fmat[,i])
}
plot(1:12,fsdev,type="l",xaxt="n",ylab="Std Dev flow cfs",xlab="Month")
axis(1,at=1:12,labels=c(10:12,1:9))

# Storage yield analysis
# Sequent peak function
seqpeak.r=function(q,r=0.7*mean(q))
{
#   r is used as input so that for simulations that may have different mean
#  the same release may be used.  r may be a vector to impose a release pattern
rmq=r-q   # net change in each time period.  Also this takes care of vector wrap around
# if r and q are of different lengths
n=length(q)
k=rep(0,n)
k[1]=max(rmq[1],0)    #  first time k[0] is 0
for (j in 2:n)
{
k[j]=max(k[j-1]+rmq[j],0)
}
s=max(k)
return(list(s=s,k=k))
}

#  Converting the flows to summable units.
#  Use cfs.days as the unit and ignore leap years
days=c(31,30,31,31,28,31,30,31,30,31,30,31)
#  This is oct, nov, dec, jan … sep for a water year
flowv= as.vector(t(fmat))
flowcfd=flowv*days     # again relying on automatic wrap around


#  Determining storage for release 70% of mean flow
r1=0.7*mean(flowcfd)
spres=seqpeak.r(flowcfd,r=r1)
plot(spres$k,type="l",xlab="Months",ylab="Storage Deficit")


#  Determining storage for release 50% of mean flow
r2=0.5*mean(flowcfd)
spres2=seqpeak.r(flowcfd,r=r2) 
lines(spres2$k,col=2)
legend(0,280000,c("0.7","0.5"),col=c(1,2),lty=1)
title("Storage deficit time series for different levels of demand")


#  Determining storage for a range of yields
y=c((1:9)/10,0.95)
s=rep(0,length(y))
for (j in 1:length(y)){
s[j]=seqpeak.r(flowcfd,y[j]*mean(flowcfd))$s
}
par("mar"=c(5.1,4.1,7.1,4.1))   # increase margin space for extra axes
plot(s,y*mean(flowcfd),type="l",xlab="Storage  cfs.day",ylab="Yield   cfs.day")
labs=c((0:50)/10)
axis(4,at=labs*mean(flowcfd),labels=labs)
mtext("Yield %", side=4,line=2)
axis(3,at=labs*mean(flowcfd)*12,labels=labs)
mtext("% Mean flow",side=3,line=2)
title("Deterministic Storage Yield Plot")


#First stochastic model
# Standardizing the flows
x= (as.vector(t(fmat))-fmave)/sqrt(fmvar)
# truncating the result to remove missing data at the beginning
n=length(x)
rho=cor(x[1:(n-1)],x[2:n])
fmsdev=sqrt(fmvar)

# specify number of realizations to simulate.  
m=20
    
# matrix to hold simulations – the same length as historic series
flowssim=matrix(0,m,n)   #  standardized simulations
flowcfssim=flowssim   # cfs simulations
for (i in 1:m){
flowssim[i,1]=rnorm(1)
for (j in 2:n){
flowssim[i,j]=flowssim[i,j-1]*rho+rnorm(1)*sqrt(1-rho*rho)
}
flowcfssim[i,]=flowssim[i,]*fmsdev+fmave
 # again taking advantage of wrap around for un-standardization
flowcfssim[i,]=ifelse(flowcfssim[i,]>0,flowcfssim[i,],0)  # replacing negatives with 0
}

plot(flowcfssim[1,],type="l")

 

# Stochastic storage yield analysis

#  For a range of yields
y=c((1:9)/10,0.95)
s=rep(0,length(y))
for (j in 1:length(y)){
s[j]=seqpeak.r(flowcfd,y[j]*mean(flowcfd))$s
}
par("mar"=c(5.1,4.1,7.1,4.1))   # increase margin space for extra axes
plot(s,y*mean(flowcfd),type="l",xlab="Storage  cfs.day",ylab="Yield   cfs.day")
labs=c((0:50)/10)
axis(4,at=labs*mean(flowcfd),labels=labs)
mtext("Yield %", side=4,line=2)
axis(3,at=labs*mean(flowcfd)*12,labels=labs)
mtext("% Mean annual runoff",side=3,line=2)
title("Storage Yield Plot")

i=1
simcfd=flowcfssim[i,]*days
s=rep(0,length(y))
for (j in 1:length(y)){
s[j]=seqpeak.r(simcfd,y[j]*mean(flowcfd))$s
}
points(s,y*mean(flowcfd))

for(i in 1:m){
simcfd=flowcfssim[i,]*days
s=rep(0,length(y))
for (j in 1:length(y)){
s[j]=seqpeak.r(simcfd,y[j]*mean(flowcfd))$s
}
points(s,y*mean(flowcfd))
}


#  Reservoir reliability
dstar=0.5* mean(flowcfd)
sreq=rep(0,m)
prel=rep(0,20)
for (j in 1:m){
simcfd=flowcfssim[j,]*days
sreq[j]=seqpeak.r(simcfd,dstar)$s
prel[j]=j/(m+1)
}
sreq=sort(sreq)
plot(prel,sreq)

dev.off()