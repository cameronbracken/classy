tmax = 720;
t = linspace(0, tmax,tmax+1);
xlim = [0,1000];
x = xlim(1):1:xlim(2);

global U0 P D ll ul C0 D
ll = 300;
ul = 400;
D = 2;
C0 = 1;
U0 = 1;
P = 720;

C_an = analytical(x, t, ll, ul, D, C0, U0, P);
%C_fd = numerical(999,xlim(1),xlim(2),ll,ul,C0,tmax,D)

sol = pdepe(0,@pdefun,@icfun,@bcfun,x,t);
C_fd = sol(:,:,1);

for i=1:length(t)


        u = P/2/pi * U0 * (cos(2 * pi * t(i)/P) - 1);

        plot(x - u, C_an(i, :))
        hold on
        plot(x,C_fd(i,:),'k-')
        
        xlabel('Distance downstream') 
        ylabel('Concentration [kg/m]')
        axis([xlim(1) xlim(2) 0 1])
        text((xlim(2)-xlim(1))/2,.9*C0,...
            ['t =',num2str(round(t(i))),' min'])
        pause(.05)
		hold off

end

save output/analytical C_an -ASCII
save output/numerical C_fd -ASCII
save output/xt x t -ASCII