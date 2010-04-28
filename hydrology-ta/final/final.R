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