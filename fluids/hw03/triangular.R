n <- 0.015
Q <- 20
S0 <- 0.001
y <- n*Q/sqrt(S0)

sf <- function(dn)(1/8)^(1/3)*dn^(8/3)
f <- function(x)abs(sf(x)-y)
dn <- optimize(f,c(2,4))$minimum

dc <- (Q^2/2)^(1/5)