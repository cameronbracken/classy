data <- read.table('colo_precip.dat',header=TRUE)
y <- data$precip
x <- as.matrix(data[c('lat','lon','elev')])

a <- c()