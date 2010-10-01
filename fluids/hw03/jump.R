q <- 1
g <- 9.81
d1 <- .5
V1 <- q/d1
Fr1 <- q/sqrt(g*d1^3)

y <- function(x)abs(x^3-x^2*(d1+q^2/(2*g*d1^2))+q^2/(2*g))
d2 <- optimize(y,c(0,1))$minimum
V2 <- q/d2
Fr2 <- q/sqrt(g*d2^3)


d3 <- d2/2*(sqrt(1+8*Fr2^2)-1)
Fr3 <- q/sqrt(g*d3^3)
V3 <- q/d3

deltaEp <- 100/16*(sqrt(1+8*Fr2^2)-3)^3/(sqrt(1+8*Fr2^2)-1)
