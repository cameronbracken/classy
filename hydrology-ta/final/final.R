library(fields)
library(topmodel)
data(huagrahuma)
data(huagrahuma.dem)
h <- huagrahuma
ind <- 2300
Qobs <- h$Qobs[1:ind]

    ## Run once
Qsim <- topmodel(h$parameters,h$topidx,h$delay,h$rain,h$ET0)[1:ind]
times <- seq(0,by=15/1440,length.out=length(Qobs))

    #plot simulated and observed
pdf('routed.pdf')
plot(times, Qobs,cex=.5, xlab='Time (days)', ylab='Flow (mm / 15 min)',
    ylim=c(0,max(Qobs,na.rm=T)))
lines(times, Qsim, col='blue')
legend('topleft',c('Observed','Predicted'),pch=c(1,-1),lty=c(0,1),col=c(1,4))
dev.off()

    #plot observed versus predicted
pdf('ovp.pdf')
plot(Qobs,Qsim, main = 'Observed v. Predicted Flow (mm / 15 min)', 
	xlab='Observed', ylab= 'Predicted')
abline(0,1)
dev.off()

NSeff(Qobs,Qsim)

pdf('regress.pdf')
x <- rnorm(ind)/100000
fit <- lm(x~Qsim)
plot(Qsim,x,main = 'Paired Effect Model', xlab='Basin Aye [mm / 15 min]', 
    ylab='Basin Bee [mm / 15 min]')
abline(fit,col='blue')
legend('topright',c('Linear Fit'),pch=c(-1),lty=c(1),col=c(4))
dev.off()

print(summary(fit))

pdf('volcano.pdf')
x <- 10*1:nrow(volcano)
y <- 10*1:ncol(volcano)
filled.contour(x, y, volcano+5000, color = terrain.colors,
    plot.title = title(main = "Digital Elevation Map",
    xlab = "Meters North", ylab = "Meters West"),
    plot.axes = { axis(1, seq(100, 800, by = 100))
                  axis(2, seq(100, 600, by = 100)) },
    key.title = title(main="Height\n(meters)"),
    key.axes = axis(4, seq(90, 190, by = 10)))# maybe also asp=1
dev.off()

pdf('lm.pdf')
xy <- expand.grid(x,y)
r.volcano <- volcano/10 + 20 + rnorm(length(volcano),sd=2)
v.dat <- data.frame(x=xy[,1],y=xy[,2],z=as.vector(volcano),p=as.vector(r.volcano))
filled.contour(x,y,r.volcano, color=tim.colors,
	plot.title = title(main = "Average Total Annual Precipitation [cm]",
    xlab = "Meters North", ylab = "Meters West"),
    plot.axes = { axis(1, seq(100, 800,100))
                  axis(2, seq(100, 600,100)) },
    key.title = title(main="Precip\n[cm]"),
    key.axes = axis(4, pretty(r.volcano)))
dev.off()

p.fit <- lm(p~x+y+z,data=v.dat)
print(summary(p.fit))