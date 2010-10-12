B0 <- 5
s <- 1
n  <- 0.013
Q <- 50
S0 <- 0.0004
g <- 9.81
alpha <- 1
d0 <- 6
dd <- -.01
maxit <- 1000

ydc <- function(x)abs(B0*x^(3/2)+s*x-Q/sqrt(g))
dc <- optimize(ydc,c(0,4))$minimum
ydn <- function(x)abs((x*(B0+s*x))^(5/3)/(B0+2*x*sqrt(s^2+1))^(2/3)-n*Q/sqrt(S0))
dn <- optimize(ydn,c(0,4))$minimum

d <- seq(d0,dn,by=dd)
A <- P <- Rh <- V <- Sf <- E <- x <- numeric(length(d))

Af <-  function(d) d*(B0+s*d)
Pf <-  function(d) B0+2*d*sqrt(s^2+1)
Rhf <- function(d) Af(d)/Pf(d)
Vf <-  function(d) Q/Af(d)
Sff <- function(d) (n*Vf(d))^2/Rhf(d)^(4/3)
Ef <-  function(d) d+alpha*Vf(d)^2/(2*g)

Sf[1] <- Sff(d[1])
E[1] <- Ef(d[1])

for(i in 2:length(d)){
    
    Sf[i] <- Sff(d[i])
    E[i] <- Ef(d[i])
    x[i] <- x[i-1]+(E[i]-E[i-1])/(S0-.5*(Sf[i]+Sf[i-1]))
    
    if( 200*(d[i]-dn)/(d[i]+dn) < 1 )
        break

    
}
x <- x[1:i] 
d <- d[1:i]