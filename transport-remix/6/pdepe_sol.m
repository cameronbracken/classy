function pdepe_sol

    tmax = 50;
    t = linspace(0, tmax, 50);
    xlim = [0,1000];
    x = xlim(1):1:xlim(2);
    
    global U0 P D ll ul C0
    ll = 300;
    ul = 400;
    C0 = 1;
    D = 2;
    U0 = 1;
    P = 720;

    sol = pdepe(0,@pdefun,@icfun,@bcfun,x,t);
    C = sol(:,:,1);
    surf(x,t,C)
    

    %------------------------
    function [c,f,s] = pdefun(x,t,u,DuDx)
        c = 1;
        f = D * DuDx;
        s = -slosh(t)*DuDx;
    end

    %------------------------
    function [u] = icfun(x)
        u =  analytical(x,0,ll,ul,D,C0,U0,P);
        u = u(1,:)';
        u(isnan(u)) = 0;
    end

    %------------------------
    function [speed] = slosh(t) 
        speed = U0 * sin(2*pi*t/P);
    end

    %------------------------
    function [pl,ql,pr,qr] = bcfun(xl,ul,xr,ur,t)
        pl = 0;
        ql = 1;
        pr = 0;
        qr = 1;
    end
end