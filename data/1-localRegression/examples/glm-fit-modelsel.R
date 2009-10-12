#

Commands to perform GLM

library(MASS)

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
                                 1 = contaminated

Y = test[,5]     #Dependent Variable
X = test[,2:4]  # Independent variable.

#2. It is advisable to convert the IDV matrix as a data frame for manipulating in #stepwise and also in model fitting.
#The columns are now referred as V1, V2, etc. (as you can see below).

#Also it is best to use the function stepAIC than step.
#help(step) recommends using stepAIC. Below is the implementation for this data set.

X = as.data.frame(X)

# fit a GLM model with all the variables
zz=glm(Y ~ ., data=X, family=binomial(link="logit"))


# remove one variable at a time and do the stepwise
z1=stepAIC(zz, trace=FALSE)

z1$anova
#gives you the initial and the final model and the AIC at each step


Stepwise Model Path
Analysis of Deviance Table

Initial Model:
y ~ V1 + V2 + V3 + V4

Final Model:
y ~ V1 + V2 + V4


 Step Df    Deviance Resid. Df Resid. Dev      AIC
1                           119   49.70439 59.70439
2 - V3  1 0.008756627       120   49.71315 57.71315

summary(z1)  # gives the details of the best fit model)
Call:
glm(formula = y ~ V1 + V2 + V4, family = binomial(link = "logit"),
   data = xx)

Deviance Residuals:
    Min        1Q    Median        3Q       Max -2.08079  -0.33644  -0.08457  -0.04780   2.14428
Coefficients:
           Estimate Std. Error z value Pr(>|z|)   (Intercept) -13.0457     3.0983  -4.211 2.55e-05 ***
V1            0.5161     0.1508   3.422 0.000622 ***
V2            0.4257     0.2712   1.569 0.116541   V4            1.0859     0.2952   3.679 0.000234 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

   Null deviance: 91.474  on 123  degrees of freedom
Residual deviance: 49.713  on 120  degrees of freedom
AIC: 57.713

Number of Fisher Scoring iterations: 7
#######################################
You can get the same result by
zz=stepAIC(glm(Y ~ ., data=X, family=binomial(link="logit")), scope=list(upper = ~., lower = ~1), trace=5)

#arguement trace =5 (or a high number spits out the details on the screen at each step)

Start:  AIC=59.7
y ~ V1 + V2 + V3 + V4

trying -V1
trying -V2
trying -V3
trying -V4
      Df Deviance    AIC
- V3    1   49.713 57.713
<none>      49.704 59.704
- V2    1   52.543 60.543
- V1    1   69.162 77.162
- V4    1   74.755 82.755

Step:  AIC=57.71
y ~ V1 + V2 + V4

trying -V1
trying -V2
trying -V4
trying +V3
      Df Deviance    AIC
<none>      49.713 57.713
- V2    1   52.545 58.545
+ V3    1   49.704 59.704
- V1    1   69.305 75.305
- V4    1   74.769 80.769

summary(zz) and anova(zz) gives the same result as before.

------
