#!/usr/bin/env Rscript

imgdir <- 'img'
pdfdir <- 'pdf'
mname <- 'simple_advection.mp4'

if(file.exists(imgdir))
	unlink(imgdir,T)

dir.create(imgdir)

if(file.exists(pdfdir))
	unlink(pdfdir,T)

dir.create(pdfdir)

n <- length(scan('h.out',nlines=1,qui=T))
x <- data.matrix(read.fwf('h.out',w=rep(12,n),na.strings='************'))
time <- x[,1]
x <- x[,-1]

png(file.path(imgdir,'%03d.png'),res=100,width=600,height=600)
pb <- txtProgressBar(1,nrow(x),style=3)
ylim <- c(min(x),max(x))
if(any(is.na(x))) ylim <- extendrange(r=c(min(x[1,],na.rm=T),max(x[1,],na.rm=T)),f=.3)
for(i in 1:nrow(x)){
    setTxtProgressBar(pb, i)
    plot(x[i,],type='l',ylim=ylim, ylab = 'Fluid Height (m)', xlab='Grid-point Number')
    text(ncol(x)*.9,ylim[2],paste("Time =",sprintf("%4.1f",time[i]/60/60),"h"),adj=c(1,.5))
}
close(pb)
dev.off()

pdf(file.path(pdfdir,'%03d.pdf'),onefile=F,width=6,height=6)
for(i in 1:nrow(x)){
    plot(x[i,],type='l',ylim=ylim, ylab = 'Fluid Height (m)', xlab='Grid-point Number')
    text(ncol(x)*.9,ylim[2],paste("Time =",sprintf("%4.1f",time[i]/60/60),"h"),adj=c(1,.5))
}
dev.off()

system(paste('ffmpeg -r 20 -sameq -y -i img/%03d.png',mname))
