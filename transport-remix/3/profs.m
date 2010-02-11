x = linspace(-5,5,1000);				  % range of x values for plot data
M = 1;                                           % Initial mass of polutant
L = 1; 
C0 = M/2/L;
D = 1;										  % Diffusion coefficient
t = L^2/(pi*D);
nframes = length(t);						% number of frames in the movie
Cul = 1;

Cg = M/sqrt(4*pi*D*t)*exp(-x.^2*4*D*t);	% Concentration profile
C = -C0/2*(erf((x-L)/sqrt(4*D*t))-erf((x+L)/sqrt(4*D*t)));
    
plot(x,Cg,'k',x,C,'k--');			  % plot data
axis([min(x) max(x) 0 Cul]);		 % set axis limits
xlabel('Distance along tube [L]','FontSize',12);
ylabel('Concentration [M/L]','FontSize',12);
%title('Concentration profile over time','FontSize',14) ;
legend('Point Mass','Distributed Mass')

% prints slope value in upper left corner of movie frame:
text(.9*min(x),.9*Cul,['time = L^2/{\pi D} =' num2str(t, '%4.2f')], ...
	  'Margin', 5, 'EdgeColor', 'k'); 
	
print('-r600','-depsc','profs.eps')