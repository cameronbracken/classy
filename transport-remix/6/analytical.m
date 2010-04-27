function C = analytical(x,t,ll,ul,D,C0,U0,P)
	n = length(t);

    C = zeros(n,length(x));
	for i = 1:n
    	C(i,:) = -C0/2*(erf(((x-ul)-U0*sin(2*pi*t(i)/P))./sqrt(4*D*t(i)))-...
				erf(((x-ll)+U0*sin(2*pi*t(i)/P))./sqrt(4*D*t(i))));

end
