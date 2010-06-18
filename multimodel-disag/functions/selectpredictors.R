selectpredictors=function(rawpredictors,response,outputfile){

  file.remove(outputfile)
	
	library(lattice)
	library(akima)
	library(leaps) 
	library(locfit)
	#options(warn=-1) #ignores all the annoying warnings
        
	#basepath = '/home/resu/public_html/windbag/R/multimodel/'
	basepath = './'
	source(paste(basepath,"functions/leaps_modified.r",sep=''))	# modified leaps commands loaded
	source(paste(basepath,"functions/Combination_filter.r",sep=''))
	
	lps=leaps(rawpredictors,response,nbest=10,method="Cp",int=F,strictly.compatible=F)
    # leaps uses the Cp statistic to compare subsets and chooses a set of the best 
    # subsets of predictors and stores them in lps$which
    # nbest-max number of subsets of each size to choose
	
	write("------GCV-Results---------",file=outputfile,append=TRUE)
	#print(lps)
	
	allsubsets=lps$which
	nsubsets=length(allsubsets[,1])
	degs=vector(nsubsets,mode='numeric')
	alphas=vector(nsubsets,mode='numeric')
	gcvs=vector(nsubsets,mode='numeric')
	
	alpha1=seq(.4,1,by=0.05)  #for 1st deg locfit .2	
	alpha2=seq(.5,1,by=0.05)  #for 2nd degree locfit .3
	deg1gcvs=1:length(alpha1);deg2gcvs=1:length(alpha2)
	fitobj=list()
  
	for(i in 1:nsubsets){ 
          # this loop determines the smoothing parameter and the 
          # corresponding locfit degree which minimizes the GCV statistic  
		options(warn=-1)
          
		predset=rawpredictors[,allsubsets[i,]]     
          #pulls predictors out for particular subset
		
		for(j in 1:length(alpha1)){
			fit=locfit.raw(x=predset,y=response,alpha=alpha1[j],deg=1,kern="bisq")
			
			z <- fit$dp[c("lk", "df1", "df2")]
			n <- fit$mi["n"]
			deg1gcvs[j] = (-2 * n * z[1])/(n - z[2])^2
			
			if(j==1){
				deg1fits=list(fit)
			}else{
				deg1fits=c(deg1fits,list(fit))
			}
		}
		for(j in 1:length(alpha2)){
			fit=locfit.raw(x=predset,y=response,alpha=alpha2[j],deg=2,kern="bisq")
			
			z <- fit$dp[c("lk", "df1", "df2")]
			n <- fit$mi["n"]
			deg2gcvs[j] = (-2 * n * z[1])/(n - z[2])^2
			
			if(j==1){
				deg2fits=list(fit)
			}else{
				deg2fits=c(deg2fits,list(fit))
			}
		}
		#deg1gcvs=gcvplot(response~predset,alpha=alpha1,deg=1,kern="bisq",scale=T)
		#deg2gcvs=gcvplot(response~predset,alpha=alpha2,deg=2,kern="bisq",scale=T)
          #calculate GCV for all alphas in sequence above 
          #repeat for a deg 1 and 2 local fit
          
        #print(paste(round(min(c(deg1gcvs$values,deg2gcvs$values)),2),' ',i,'/',nsubsets,sep=''))
		if(is.null(ncol(predset))){
			print(paste('set',i,'out of',nsubsets,'with',1,'predictors'),quote=F)
		}else{
			print(paste('set',i,'out of',nsubsets,'with',ncol(predset),'predictors'),quote=F)
		}
        
        #determine where the lowest GCV value is and what are the alpha and deg
        if(min(deg2gcvs)<min(deg1gcvs)){  #min GCV has degree 2 locfit
          degs[i]=2
          hold=order(deg2gcvs)
          gcvs[i]=deg2gcvs[hold[1]]
          alphas[i]=alpha2[hold[1]] 
		  fitobj=c(fitobj,list( deg2fits[[hold[1]]] ))
		}else{                                           #min GCV has deg 1 locfit
          degs[i]=1
          hold=order(deg1gcvs)
          gcvs[i]=deg1gcvs[hold[1]]
          alphas[i]=alpha1[hold[1]]
		  fitobj=c(fitobj,list( deg1fits[[hold[1]]] ))
		}
	}
	
	#print(fitobj[[1]])
    
	print('',quote=F)
	for(i in 1:nsubsets){
		print(paste('for subset ',as.integer(i),', GCV=',round(gcvs[i],2),
				'; alpha=',alphas[i],sep=''),quote=F)
	}
	print('',quote=F)
  
	a=1
	for(n in 1:max(lps$size)){ 
			#loop through set w/ same number of predictors
			#this whole loop is really just book keeping for la large
			#number of predictors and not really necessary for most cases
			#It will discard predictor sets if there are too many 
		print(paste('there are',length(lps$size[lps$size==n]),'sets with',n,'predictor(s)'),quote=F)
		
		b=a+length(lps$size[lps$size==n])-1  #number of sets w/ n predictors
		
			#luckily leaps already orders subsets by # predictors
		gcvswithnpredictors=gcvs[a:b] 
		setswithnpredictors=allsubsets[a:b,]
		
		gcvsordered=order(gcvswithnpredictors)  #order just subsets with same n predictors
		gcvssorted=sort(gcvswithnpredictors)   #sort just subsets with same n predictors
		
		
			#determine how many subsets to use from set with n predictors
			#probably not very useful if there are less than 5 predictors
			#the value we are determining (keepsets) is the number of sets 
			#to keep from the subset that has the same number of predictors
		nsets=length(gcvssorted)
		base=5
		if((range(gcvssorted)[2]/gcvssorted[1]) >1.1 ){
			keepsets=0
			while((gcvssorted[keepsets+1])<=(1.1*gcvssorted[1])){keepsets=keepsets+1}
			if(keepsets<base){keepsets=base}
			if(nsets<base){keepsets=nsets}
		}
		else{
			keepsets=base
				if(nsets<base){keepsets=nsets}
		}
		
		
		print(paste('keeping',keepsets,'out of',nsets,'sets;','indicies are',a,b),quote=F)
		print('',quote=F)
		
		
		#these conditions are necessary because if there is only one member of a set then
		#it is represented as a vector and not an array
		if(length(lps$size[lps$size==n])==1){  
			keptpredictors=setswithnpredictors
			keptpredictorset=c(keptpredictors,gcvssorted)
			write(n,file=outputfile,append=TRUE,ncol=1)
			write(t(keptpredictorset),file=outputfile,append=TRUE,ncol=length(keptpredictorset))
		}else{
			keptpredictors=setswithnpredictors[gcvsordered[1:keepsets],]
			keptpredictorset=cbind(keptpredictors,gcvssorted[1:keepsets])
			write(n,file=outputfile,append=TRUE,ncol=1)
			write(t(keptpredictorset),file=outputfile,append=TRUE,ncol=ncol(keptpredictorset))
		}
			#print(length(lps$size[lps$size==i]))
		
		a=b+1
	}
	
	#Now finally the last step 
	allgcvsordered=order(gcvs)
	
		#Filters the combinations based on correlation between predictor varibles(multicolinearity)
		#returns an array that is the same size or smaller than all subsets
		#where rows corresponding to multicolinear sets have been discarded
	combinationskept=combinationfilter(rawpredictors,allsubsets[allgcvsordered,])
	#print(nrow(allsubsets[allgcvsordered,]))
	
	write("----------------------------------------",file=outputfile,append=TRUE)
	write("Best Combinations that are not correlated among themselves",file=outputfile,append=TRUE)
	write(t(combinationskept),file=outputfile,append=TRUE,ncol=length(combinationskept))
	write("----------------------------------------",file=outputfile,append=TRUE)
	
	
	finalselection=allgcvsordered[combinationskept]		
	print(finalselection)
	finalpredictors=allsubsets[finalselection,]
	
	
	gcvratio=1:length(gcvs[finalselection])
	for(igcv in 1:length(gcvratio)){
		gcvratio[igcv]=round((gcvs[finalselection][igcv]/gcvs[finalselection][1]),3)
	}
  
	predset=cbind(lps$size[finalselection],finalpredictors,gcvs[finalselection],
				degs[finalselection],alphas[finalselection],gcvratio)
				
	hold=fitobj
	fitobj=list()
	for(i in 1:length(finalselection)){
		fitobj=c(fitobj,list(hold[[finalselection[i]]]))

	}
	
	write("Finally selcted Combinations",file=outputfile,append=TRUE)
	write("npredictors, predictors selected, GCV vals, deg, alpha,GCV ratio",file=outputfile,append=TRUE)
	write(t(predset),file=outputfile,append=TRUE,ncol=ncol(predset))
	return(list(p=predset,fitobj=fitobj))
	
	
	
}
