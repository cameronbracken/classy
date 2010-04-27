require(Cairo)
suppressPackageStartupMessages(require(VGAM))
source("analytical_f.R")
source("numerical_f.R")

tmax <- 720
t <- scseq(0, tmax, , 50)
xlim <- c(0,1000)
x <- -1000:2000
ll <- 300
ul <- 400
D <- 2
C0 <- 1
U0 <- 1
P <- 720

C_an <- analytical(x, t, ll, ul, D, C0, U0, P)
C_fd <- numerical(999,xlim[1],xlim[2],ll,ul,C0,tmax,D,U0,P)

imgdir <- "img"
if (file.exists(imgdir)) unlink(file.path(imgdir,'*'),T) else dir.create(imgdir)

#CairoFonts(regular = "Palatino:Italic", bold = "Palatino", 
#	italic = "Palatino", bolditalic = "Palatino")

#CairoJPEG(file.path(imgdir, "%03d.jpg"), width = 600, height = 600, 
#	pointsize = 14, quality = 100)
	
pb <- txtProgressBar(1, length(t), style = 3)

	for (i in 1:length(t)) {
		Cairo_png(file.path(imgdir, sprintf("%03d.jpg",i)), width = 6, height = 6)
			par(font = 1, family = "Palatino")
		
			u <- P/2/pi * U0 * (cos(2 * pi * t[i]/P) - 1)
		
			plot(x - u, C_an[i, ], ylim = c(0, C0), type = "l", xlim = xlim, 
				xlab = "Distance downstream", 
				ylab = "Concentration [kg/m]", font = 1)
			if(i == 1)lines(c(ul,ll), c(C0,C0),type='h')
			text(xlim[1],C0,paste('t =',round(t[i],0),'min'),pos=4)
			setTxtProgressBar(pb, i)
		dev.off()
	}
	
close(pb)
