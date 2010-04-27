function [u] = icfun(x)
	global U0 P D ll ul C0 D
    u =  analytical(x,0,ll,ul,D,C0,U0,P);
    u = u(1,:)';
    u(isnan(u)) = 0;