function [speed] = slosh(t) 
	global U0 P
    speed = U0 * sin(2*pi*t/P);