#call_multimodel=function(datamatrix,trainingindices,nsims=1,outputfile='pred.out'){

nsims <- 20

args=c('data/P000000043.txt', '1', '20000','pred.out')

datamatrix = as.matrix(read.table(args[1],header=FALSE,sep="\t"))

trainingindices = as.integer(args[2])
trainingindices[2] = as.integer(args[3])
outputfile = args[4]


	source('functions/selectpredictors.r')
	source('functions/multimodel.r')
	
	allpredictors=datamatrix[,2:ncol(datamatrix)]                               #predictors 
	#allpredictors=datamatrix[1:400,2:ncol(datamatrix)]
	response=datamatrix[,1]
	#response=datamatrix[1:400,1]														#dependent variable
	npts=nrow(datamatrix)
	
	
	trainingrange=trainingindices[1]:trainingindices[2]
	if(trainingindices[1]==1){
		if(trainingindices[2]>=npts){
			stop("Error, training set size is >= the entire data set, stopping.")
		}else{
			predictionrange=(trainingindices[2]+1):npts
		}
	}else{
		if(trainingindices[2]==npts){ 
			predictionrange=1:(trainingindices[1]-1)
		}else{
			predictionrange=c(1:(trainingindices[1]-1),(trainingindices[2]+1):npts)
		}
	}
	#predictionrange=201:400
	#trainingrange=1:200
	#npts=400
	
	#psets=selectpredictors(allpredictors[trainingrange,],response[trainingrange],paste(basepath,'../../data/multimodelinfo.out',sep=''))
	psets=selectpredictors(allpredictors[trainingrange,],response[trainingrange],'multimodelinfo.out')

	gcvs=as.vector(psets$p[,(ncol(psets$p)-3)])      # Takes only GCV information 
	alphas=as.vector(psets$p[,(ncol(psets$p)-1)])
	degs=as.vector(psets$p[,(ncol(psets$p)-2)])
	fitobj=psets$fitobj
	psets=psets$p[,2:(ncol(psets$p)-4)]   # Grabs only predictors information 
	
	#stop('i am the king')
	
	pred=multimodel(fitobj,allpredictors[trainingrange,],response[trainingrange],
					allpredictors[predictionrange,],psets,gcvs,alphas,degs,
					npts-length(trainingrange),nsims=nsims,outputfile)

	for(i in 1:(npts-(trainingindices[2]-trainingindices[1])-1)){if(pred[i]==0){pred[i]=.0001}}

	write(t(pred),file=outputfile,ncolumns=nsims)
