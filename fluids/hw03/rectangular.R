B <- 2
n  <- 0.010
Q <- 10
S0 <- 0.005
g <- 9.81
alpha <- 1
d0 <- 0.5
dd <- .01

dc <- ((Q/B)^2/g)^(1/3)
ydn <- function(x)abs((x*B)^(5/3)/(2*x+B)^(2/3)-n*Q/sqrt(S0))
dn <- optimize(ydn,c(0,4))$minimum

d <- seq(d0,dn,by=dd)
A <- P <- Rh <- V <- Sf <- E <- x <- numeric(length(d))

Af <-  function(d) d*B
Pf <-  function(d) 2*d+B
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
    
    if( 200*(d[i]-dn)/(d[i]+dn) > 1 )
        break

    
}
x <- x[1:i] 
d <- d[1:i]