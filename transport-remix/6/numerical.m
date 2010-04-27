function C = numerical(nip,xl,xr,ll,ul,C0,tmax,D)
	
	Bl = 0;
	Br = 0;
	global dx dt U0 P
	dx = (xr - xl)/(nip + 1);
	dt = .5*dx/U0%min([dx^2*.25/(2*(D*2)),1])
	skip = 1/dt 
	
    function x = u(t) 
        x = U0 * sin(2*pi*t/P);
    end
	
		%#number of interior points
	nip = (xr - xl)/dx - 1;
	
	alpha = 1/6/dt;
    
    function x = beta(t)
        x = u(t)/4/dx;
    end
        
	gamma = D/2/dx^2;
	
    function x = p(t) 
        x = alpha - beta(t) - gamma;
    end
    function x = q(t)
       x = 4*alpha + 2*beta(t);
    end
    function x = r(t) 
        x = alpha + beta(t) + gamma;
    end
    function x = L(t) 
        x = alpha + beta(t) - gamma;
    end
    function x = M(t)
       x = 4*alpha - 2*beta(t);
    end
    function x = N(t) 
        x = alpha - beta(t) + gamma;
    end
	
	t = 0:dt:tmax;
	x = dx:dx:((xr-xl)-dx);
	nt = length(t)
	
		%#Initial conditions
	C = zeros(nt,nip);
	h =  analytical(x,2,ll,ul,D,C0,U0,P);
	C(1,:) = h(1,:);
    Cnew = C(1,:);
	
	f = zeros(1,nip);
	
	s = 0;
    for n = 1:(nt-1)
		%if(mod(n,skip)==0)plot(Cnew,type='l')

		qn = q(t(n)); 
		Mn = M(t(n));
		pn = p(t(n));
		rn = r(t(n));
		Ln = L(t(n)); 
		Nn = N(t(n));
		
		A = diag(repmat(qn,nip,1)) + diag(repmat(pn,nip-1,1),-1) + diag(repmat(rn,nip-1,1),1);
		B = diag(repmat(Mn,nip,1)) + diag(repmat(Ln,nip-1,1),-1) + diag(repmat(Nn,nip-1,1),1);
		f(:) = 0;
		
			%#junk
		f(1) = Bl*(Ln-pn);
		f(nip) = Br*(Nn-rn);
		
		Cold = Cnew; 
		Cnew = ((Cold * B) + f)/A;
        C(n,:) = Cnew;
        sprintf('\b\b\b\b\b\b\b\b\b %d',n)
    end
end