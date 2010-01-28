function C = twoDC(x,y,t,M,D,x0,y0)
    
    [X Y] = meshgrid(x,y);
    C = X;
    for i=1:length(x)
        for j=1:length(y)
            C(j,i) = M/(4*pi*D*t(1)).*...
                     (exp(-(X(j,i)-x0).^2/(4*D*t(1))-(Y(j,i)-y0).^2/(4*D*t(1))));
        end
    end

end