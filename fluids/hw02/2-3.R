source('constriction.R')
L <- 1
np <- 1000
x <- seq(0,L,,np)
p <- as.integer(c(1,np/4,np/2,3*np/4,np))
xp <- c(0,L/4,L/2,3*L/4,L)


B_0 <- 1.25
Q <- 0.125
g <- 9.81
d0_sub <- 0.25
d0_sup <- 0.06
delta0_sub <- 0.3
delta0_sup <- 0.2
delta0c_sub <- delta_crit(d0_sub)
delta0c_sup <- delta_crit(d0_sup)
q_0 <- Q/B_0
E_sub <- d0_sub + q_0^2/(2*g*d0_sub^2)
E_sup <- d0_sup + q_0^2/(2*g*d0_sup^2)

sub <-      constriction(x, delta0_sub,  d0_sub)
sub_crit <- constriction(x, delta0c_sub, d0_sub, crit=T)
sup <-      constriction(x, delta0_sup,  d0_sup, sub=F)
sup_crit <- constriction(x, delta0c_sup, d0_sup, sub=F, crit=T)

#quartz(height=3.8,width=10)
#AwesomePlots(sub,      delta0_sub, xp, p, E_sub, 'Subcritical')
#quartz(height=3.8,width=10)
#AwesomePlots(sub_crit, delta0c_sub, xp, p, E_sub, 'Subcritical Transition')
#quartz(height=3.8,width=10)
#AwesomePlots(sup,      delta0_sup, xp, p, E_sup, 'Supercritical')
#quartz(height=3.8,width=10)
#AwesomePlots(sup_crit, delta0c_sup, xp, p, E_sup, 'Supercritical Transition')