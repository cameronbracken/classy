combinationfilter=function(AllPredictors,selpredsetcombi){
  #This function will discard combinations which are highly
  #correlated among themselves (ie. multicolinear)
	predpos=0	
	combinationkept=vector(length(selpredsetcombi),mode='numeric')	# Vector to store combinations
	print("Correlation at 95% level:::",quote=F)
	sigcor=sqrt(0.8)
	#sigcor=round(sqrt(2/sqrt(nrow(AllPredictors))),3)
	print(sigcor,quote=F)
	print('',quote=F)
	
	for(selpredset1 in 1:nrow(selpredsetcombi) ) {

		selpredset=as.logical(selpredsetcombi[selpredset1,])
		preddata=data.frame(AllPredictors[,selpredset])    
		corpos=0
		corvalues=1:( ncol(preddata) * ncol(preddata) )
    
		if(ncol(preddata)>1){
			cortable=matrix(-9999,nrow=ncol(preddata),ncol=ncol(preddata))
			for( i in 1:(ncol(preddata)-1) ) {
				for(j in (i+1):ncol(preddata) ) {
					corpos=corpos+1
					corvalues[corpos]=round(cor(preddata[,i],preddata[,j]),3)
				}
			}
			corvalues=corvalues[1:corpos]

			if((max(corvalues)>sigcor) || (min(corvalues)<(-1*sigcor))){
				decision=0;print(paste("Combination",selpredset1, "NOT Selected",sep=" "),quote=F)
			}else{decision=1;print(paste("Combination",selpredset1,"Selected"),quote=F)}
		}else{decision=1;print(paste("Combination",selpredset1,"Selected"),quote=F)}
		
		if(decision==1){predpos=predpos+1;combinationkept[predpos]=selpredset1}                 
	}
		combinationkept=combinationkept[1:predpos]
		combinationkept
}
