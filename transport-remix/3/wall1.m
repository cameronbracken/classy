M = 1;                                           % Initial mass of polutant
L = 1;
x = linspace(-2*L,10,1000);				  % range of x values for plot data
C0 = M/2/L;
D = 1;										  % Diffusion coefficient
t = [0,.1,1,5];
Cul = 1.1;
C = zeros(4,length(x));

for i=1:length(t)
	C(i,:) = -C0*(erf((x-L)/sqrt(4*D*t(i)))-erf((x+L)/sqrt(4*D*t(i))))...
			-C0*(erf((x+3*L)/sqrt(4*D*t(i)))-erf((x+5*L)/sqrt(4*D*t(i))));
end

plot(x,C(1,:),'k',x,C(2,:),'k:',x,C(3,:),'k--',x,C(4,:),'k-.');			  % plot data
axis([min(x) max(x) 0 Cul]);		 % set axis limits
xlabel('Distance along tube [Length]','FontSize',12);
ylabel('Concentration [M/L]','FontSize',12);
%title('Concentration profile over time','FontSize',14);
legend('t = 0','t = .1','t = 1','t = 5')

print('-r600','-depsc','wall1.eps')