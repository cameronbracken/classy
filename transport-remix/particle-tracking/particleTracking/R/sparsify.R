sparsify <- function(mat,np=nrow(mat)){
	#d <- sqrt(diff(positions[,1])^2 + diff(positions[,2])^2)
	#sparse <- integer(np)
	#s <- n <- 0
	#for(p in 1:(np-1)){
	#	s <- s + d[p]
	#	#if(length(s > tol) == 0) browser()
	#	if(s > tol){
	#		n <- n + 1
	#		sparse[n] <- p
	#		s <- 0
	#	}
	#}
	#sparse <- sparse[!(sparse == 0)]
	##sparse <- sparse[d[sparse] < tol]
	
	pp <- mat
	#pp <- mat[sparse,]
	n <- nrow(pp)
	#dpp <- sqrt(diff(pp[,1])^2 + diff(pp[,2])^2)
	#xnew <- ynew <- integer(n)
	#pos <- 1:n
	p <- nnew <- 0
	while((p+1)<nrow(pp)){
		p <- p+1
		q <- if((p+1)==nrow(pp))
			1
		else
			p+1
			
		dpp <- sqrt((pp[q,1] - pp[p,1])^2 + (pp[q,2] - pp[p,2])^2)

		if(dpp >= tol){
			nnew <- nnew + 1
			xmid <- (pp[p,1] + pp[q,1])/2
			ymid <- (pp[p,2] + pp[q,2])/2
			#xnew[nnew] <- xmid
			#ynew[nnew] <- ymid
			pp <- if((p+1)==nrow(pp)){
				#pos[n+nnew] <- pos[q]
				rbind(pp,c(xmid,ymid))
			}else{
				#if(p==1)print(c(xmid,ymid))
				#pos[n+nnew] <- pos[q]
				#pos[q:n] <- pos[q:n] + 1
				rbind(pp[1:p,],c(xmid,ymid),pp[(p+1):nrow(pp),]) 
			}
		}
	}
	#browser()
	#pp <- rbind(pp,cbind(xnew,ynew))[pos,]
	#pp[d[sparse]>tol,1] <- NA
	pp
}