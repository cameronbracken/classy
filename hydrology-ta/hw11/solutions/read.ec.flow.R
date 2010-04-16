read.ec.flow <- function(file){
	x <- read.csv(file)
	mon <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
	dat <- matrix(NA,1,4)
	for(i in unique(x$Year)){
		for(m in 1:length(mon)){
			y <- x$Year
			this.data <- x[y == i,][[mon[m]]]
			n <- length(this.data)
			
			dat <- rbind(dat,cbind(rep(i,n),rep(m,n),1:n,this.data))
		}
	}
	dat <- dat[!is.na(dat[,4]),]
	
	dat <- as.data.frame(dat)
	names(dat) <- c('y','m','d','flow')
	
	return(dat)
}