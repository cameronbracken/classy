require('knnflex')

leesf <- ts(read.table('data/lf-annual.txt')[,2],1906)/10^6
k <- 10
pred <- numeric(length(leesf))
d <- knn.dist(leesf)

for(i in 1:length(leesf)){
	
	train <- (1:length(leesf))[-i]
	test <- i
	pred[i] <- knn.predict(train,test,leesf[-i],d,k=k)
	
	#this.point <- leesf[i]
	#this.train <- leesf[-i]
	
	#d <- sqrt((this.point - this.train)^2)
	#neighbors <- rank(d)[1:k]
	
}
pred <- ts(pred,1906)
plot(leesf)
lines(pred,col='red')