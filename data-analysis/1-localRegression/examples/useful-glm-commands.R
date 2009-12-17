#

Commands to perform GLM

library(MASS)
library(locfit)

#read the data you have as a matrix. 


test=matrix(scan("gw-data.txt", skip=1), ncol=5, byrow=T)

#You can also read it as a table and convert it into a matrix at the same time.
test = as.matrix(read.table("gw-data.txt",skip=1))

#the first row has the column headers and I wish to skip it..

#help(scan) gives you all the details of scan. It has several features that you can use #by passing appropriate arguements.

#2. The above data is a groundwater quality data. The last column is my DV (1 = #contaminated, 0 = not contaminated)

# Thick:   Unsat Zone Thickness   0 (favorable) to 9 (unfavorable)
#Yields:   Est. Aquifer Yield     0 (poor)      to 6 (good)
#GW Qual:   Pristine GW Qual       0 (poor)      to 5 (excellent)
#Hazard:   Hazard of Waste Matl   1 (low)       to 9 (high)
#Contam:   Contamination Rating   0 = uncontaminated
#                                 1 = contaminated

Y = test[,5]     #Dependent Variable
X = test[,2:4]  # Independent variable.


#### Fit the model on the first 100 points and evaluate on the last 24 points..
X1 = X[1:100,]
X1 = as.data.frame(X1)
Y1 = Y[1:100]

Xp = X[101:124,]
Yp = Y[101:124]


# fit a GLM model with all the variables
zz=glm(Y1 ~ X1, data=X1, family=binomial(link="logit"))

# predict at the points of interest..
zp = predict.glm(zz, data.frame(Xp), se.fit=T, type="response")

########

#### Local Polynomial - LOCFIT GLM...
### GLM model requires a higher mininum alpha..
### You can get the minimum alpha by trials. If you provide a low minimum alpha
### you will get some warning messages..

min_alph=0.75
alpha_grid=seq(min_alph,1.0,by=0.05)
n=length(alpha_grid)
min_alph=0.99
alpha1_grid=seq(min_alph,1.0,by=0.05)
alpha2_grid=c(alpha_grid,alpha1_grid)

gcv_deg1=gcvplot(Y1 ~ ., data=X1, maxk = 100000, alpha=alpha_grid,deg=1,kern="bisq", scale = T, family="binomial",ev=dat())
gcv_deg2=gcvplot(Y1 ~ ., data=X1, maxk = 100000, alpha=alpha1_grid,deg=2,kern="bisq", scale = T, family="binomial",ev=dat())
# pick the best alpha and the degree of the polynomial that
# gives the least GCV
z2=order(c(gcv_deg1$values,gcv_deg2$values))
deg1=1
if(z2[1] > n)deg1=2
best_alpha = alpha2_grid[z2[1]]
best_gcv = c(gcv_deg1$values,gcv_deg2$values)[z2[1]]
output=c(deg1, best_alpha, best_gcv)    #the best parameter set

#zz = locfit(Y1 ~ ., data=X1, alpha=best_alpha, maxk = 10000, deg=deg1,kern="bisq", scale = T, ev=dat(),family="binomial")
zz = locfit(Y1 ~ ., data=X1, alpha=best_alpha, maxk = 10000, deg=deg1,kern="bisq", scale = T, family="binomial")

zpl = predict.locfit(zz, data.frame(Xp), se.fit=T )

### You can plot the zpl$fit and zpl$se   ..
