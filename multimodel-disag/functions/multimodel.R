multimodel=function(locfitcalib,trainingmodel,trainingresponse,allpredictors,psets,gcvs,alphas,degs,npredpts,nsims=1,outputfile='simdata.out'){

	#The process goes:
	#				1.store the fit objects of each model
	#				2.generate nsims predictions for every point
	#				3.return the predictions
	
  library(locfit)
  library(lattice)
  library(akima)
	
		simdata=matrix(0,nrow=npredpts,ncol=nsims)
		
		W=1/gcvs	    # Least GCV combination gets more weight
		W=W/sum(W)
		W=cumsum(W)		#W is the weight function
		
		#for(i in 1:nrow(psets)){
		#	fit=locfit(trainingresponse~trainingmodel[,psets[i,]],alpha=alphas[i],
		#					deg=degs[i],kern="bisq",scale=T)
		#	if(i==1){
		#		locfitcalib=list(fit)
		#	}else{
		#		locfitcalib=c(locfitcalib,list(fit))
		#	}
		#}
		
		print('Making Predicitons...')
		for(j in 1:npredpts){
					#for every point in prediction range, make a prediction
			for(i in 1:nsims){
					#choose model
				randnum=runif(1,0,1)		#draw a U(0,1) RV
				xyzran=c(randnum,W)			#attaches randomnumber to the weight function
				modelnumber=rank(xyzran)[1]	#the first number of the ranked vector is the 
											#index of the randnum in the ordered vecor 
		
				selectedpset=as.logical(psets[modelnumber,])    #pull out combination for this iteration
				modeldata=allpredictors[,selectedpset]			#pull out predictors corresponding to this combo
				
				if(sum(psets[modelnumber,])==1){
					#then there is only one predictor
					predictat=modeldata[j]				#The current state of the system
				}else{
					#there is more than one predictor
					predictat=modeldata[j,]
					predictat=rbind(predictat)			#so that the data is as.matrix
				}

				
				locfitpredicted=predict.locfit(locfitcalib[[modelnumber]],predictat,se.fit=T,band="global")
		
				#if(j%%1==0){print(paste('model',modelnumber,'on point',j,'out of',npredpts),quote=F)}
		
				resid=rnorm(1,0,locfitpredicted$se.fit[1])
				simdata[j,i]=locfitpredicted$fit[1]+resid
			
				if(is.nan(simdata[j,i])){simdata[j,i]=0}
				if(simdata[j,i]<0){simdata[j,i]=0}
		
			}
		}
		return(simdata)
}
