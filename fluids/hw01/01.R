V <- readMat('V.mat')$V

x <- seq(-20,20,,length.out=ncol(V))
y <- seq(0,10,,length.out=nrow(V))

#contour(x=x,y=y,t(flipud(V)))

Vbar <- mean(V)
beta <- mean(V^2)/Vbar^2
alpha <- mean(V^3)/Vbar^3
