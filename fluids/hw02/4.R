B1 = 3 
B2 = 2
dz = 0.3
g <- 9.81

d1 <- c(0.4, 0.6, 0.8, 1.0, 1.2, 1.4, 1.6, 1.8, 2.0)
d2 <- seq(0,1.8,,100)
Qc <- function(d2) sqrt(d2^3*B2^2*g)
Q <- function(d1,d2) sqrt((2*g*(d2-d1+dz))/(1/(B1*d1)^2-1/(B2*d2)^2))

plot(d2,Qc(d2),type='l',ylab='$Q$',xlab='$d_2$')

min_d2 <- 0

for(i in seq_along(d1)){    

    d2 <- seq(0,2,,10000)
    Qp <- Q(d1[i],d2)
    
    filter <- Qp < Qc(d2) 
    
    #print(Q(d1[i],d2[j]))
    #print(j)
    stopp <- FALSE
    for(j in seq_along(Qp)){
        if(is.na(Qp[j]) & !stopp)
            stopp <- TRUE
        if(stopp)
            Qp[j] <- NA
    }
    lines(d2[filter],Qp[filter])
    min_d2[i] <- min(d2[filter],na.rm=T)

}
text(min_d2,Qc(min_d2),paste('\\small $d_1$ = ',d1),srt=-65,pos=2)