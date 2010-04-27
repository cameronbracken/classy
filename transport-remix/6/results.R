require(cairoDevice)

x <- scan('output/xt',nlines=1)
t <- scan('output/xt',nlines=1,skip=1)
C_an <- as.matrix(read.table('output/analytical'))
C_fd <- as.matrix(read.table('output/numerical'))

xlim <- range(x)
U0 <- 1
P <- max(t)
ul <- 400
ll <- 300
C0 <- max(C_fd)

imgdir <- "img"
if (file.exists(imgdir)) unlink(file.path(imgdir,'*'),T) else dir.create(imgdir)
	
for (i in 1:length(t)) {
	Cairo_png(file.path(imgdir, sprintf("%03d.png",i)), width = 6, height = 6)
	
		par(font = 1, family = "Palatino")
	
		u <- P/2/pi * U0 * (cos(2 * pi * t[i]/P) - 1)
	
		plot(x - u, C_an[i, ], ylim = c(0, max(C_fd)), type = "l", xlim = xlim, 
			xlab = "Distance downstream", 
			ylab = "Concentration [kg/m]", font = 1)
		points(x,C_fd[i,],cex=.5)
		
		if(i == 1)lines(c(ul,ll), c(C0,C0),type='h')
		
		text(xlim[1],C0,paste('t =',round(t[i],0),'min'),pos=4)
		legend('topright',c('Analytical','Numerical'),lty=c(1,0),pch=c(-1,1))
	
	dev.off()
}
	
	
mname <- '1dAD.mp4'
system(paste('ffmpeg -r 20 -sameq -y -i img/%03d.png',mname))