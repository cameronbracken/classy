source('read.ec.flow.R')

camp <- read.ec.flow('flowDaily_Camp.csv')
greata <- read.ec.flow('flowDaily_Greata.csv')

times.c <- camp$y + camp$m/12 + camp$d/365
times.g <- greata$y + greata$m/12 + greata$d/365

plot(times.c,camp$flow,col='blue',xlab='Time',ylab='Flow [cms]',type='l')
lines(times.g,greata$flow,col='red')

y.c <- unique(camp$y)
y.g <- unique(greata$y)
peak.c <- apr.c <- numeric(length(y.c))
peak.g <- apr.g <- numeric(length(y.g))

for(y in 1:length(y.c)){
	peak.c[y] <- max(camp$flow[camp$y == y.c[y]])
	apr.c[y] <- mean(camp$flow[camp$y == y.c[y] & camp$m == 4])
}
for(y in 1:length(y.g)){
	peak.g[y] <- max(greata$flow[greata$y == y.g[y]])
	apr.g[y] <- mean(greata$flow[greata$y == y.g[y] & greata$m == 4])
}
	
peak.c <- peak.c[y.c %in% y.g]
apr.c <- apr.c[y.c %in% y.g]
apr.c.pre <- apr.c[y.g<1978]
apr.g.pre <- apr.g[y.g<1978]
apr.c.post <- apr.c[y.g>=1978]
apr.g.post <- apr.g[y.g>=1978]

summary(lm(apr.c.pre~apr.g.pre))

dpm <- c(31,29,31,30,31,30,31,31,30,31,30,31)

am.c.pre <- am.g.pre <- am.c.post <- am.g.post <- numeric(366)
day <- 0
for(m in 1:12){
	for(d in 1:dpm[m]){
		day <- day + 1
		this.day.c <- camp$m == m & camp$d == d
		this.day.g <- greata$m == m & greata$d == d
		am.c.pre[day] <- mean(camp$flow[this.day.c & camp$y < 1978])
		am.c.post[day] <- mean(camp$flow[this.day.c & camp$y >= 1978])
		am.g.pre[day] <- mean(greata$flow[this.day.g & greata$y < 1978])
		am.g.post[day] <- mean(greata$flow[this.day.g & greata$y >= 1978])
	}
}

plot(am.c.pre,type='l')
lines(am.g.pre,type='l')
polygon(c(1:366,366:1),c(am.c.pre,rev(am.g.pre)),col='blue')

plot(am.c.post,type='l')
lines(am.g.post,type='l')
polygon(c(1:366,366:1),c(am.c.post,rev(am.g.post)),col='blue')