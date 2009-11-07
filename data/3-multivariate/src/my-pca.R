my.pca <- function(x){
	s <- svd(var(x))
	
	#do an Eigen decomposition..
	eigv <- s$u
	eig <- s$d

	#Principal Components...
	pcs <- t(t(eigv) %*% t(x))

	#Eigen Values.. - fraction variance 
	eigf <- (eig/sum(eig))
	
	return(list(eigv=eigv,eigf=eigf,eig=eig,pc=pcs))
	
}