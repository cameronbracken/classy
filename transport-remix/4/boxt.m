T = 4;                       % Tidal period (hours)
U0 = 0.1;                    % Mean velocity (miles/hour)
U1 = 1;                      % Fluxuating velocity amplitude (miles/hour)
M = 1;                       % Initial mass of polutant (lbs)
D = 0.03;                    % Diffusion coefficient (miles^2/hour)
t = linspace(0,48,1000);     % Range of time values for plot data

x = U0*t + U1 * T / (2 * pi) * sin(2 * pi * t / T);

plot(t,x)

ylabel('Distance Downstream [miles]');
xlabel('Time [hours]','FontSize',12); 
	
print('-r600','-depsc','boxt.eps')