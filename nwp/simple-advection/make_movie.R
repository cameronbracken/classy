#!/usr/bin/env Rscript

imgdir <- 'img'
mname <- 'simple_advection.mp4'
suppressMessages(require(Cairo))

if(file.exists(imgdir))
	unlink(imgdir,T)

dir.create(imgdir)

x <- as.matrix(read.table('h.out'))

#CairoJPEG(file.path(imgdir,'%03d.jpg'),quality=100)
png(file.path(imgdir,'%03d.png'),res=100,width=600,height=600)
pb <- txtProgressBar(1,nrow(x),style=3)
for(i in 1:nrow(x)){
    setTxtProgressBar(pb, i)
    plot(x[i,],type='l',ylim=c(min(x),max(x)), ylab = 'Height', xlab='')
}
close(pb)
dev.off()

system(paste('ffmpeg -r 20 -sameq -y -i img/%03d.png',mname))
