function [c,f,s] = pdefun(x,t,u,DuDx)
	global D
    c = 1;
    f = D * DuDx;
    s = -slosh(t)*DuDx;