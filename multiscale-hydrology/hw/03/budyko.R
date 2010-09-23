le <- 2260 #kK/kg
alpha1 <- 2.6
alpha2 <- 1.8
RnP <- seq(0,4*le,,100)
phi <- RnP/le
EP <- sqrt(phi*tanh(1/phi)*(1-cosh(phi)+sinh(phi)))
#EP[EP < .00001] <- NA

ETP1 <- 1/(1+phi^-alpha1)^(1/alpha1)
ETP2 <- 1/(1+phi^-alpha2)^(1/alpha2)

p.diff <- function(x,y)
    200*(x-y)/(x+y)


#x <- data.frame(phi=phi,EP=EP,ETP=ETP)
#p <- ggplot(data=x) + 
#    theme_bw() + 
#    geom_line(aes(phi,EP, color='red')) +
#    geom_line(aes(phi,ETP,color='blue')) + 
#    scale_x_continuous('$\\frac{R_n}{P}$') + 
#    scale_y_continuous('$\\frac{E}{P}$') +
#    opts(legend.position="bottom") 
#
#print(p)