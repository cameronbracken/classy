T = 4;                       % Tidal period (hours)
U0 = 0.1;                    % Mean velocity (miles/hour)
U1 = 1;                      % Fluxuating velocity amplitude (miles/hour)
M = 1;                       % Initial mass of polutant (lbs)
D = 0.03;                    % Diffusion coefficient (miles^2/hour)
t = linspace(.2,48,1000);

Cx0 = linspace(0,0,length(t));

for i=1:length(t)
	%xp = x - (U0 + U1*cos(2*pi*t(i)/T))*t(i);
	xp = - (U0*t(i) + U1 * T / (2 * pi) * sin(2 * pi * t(i) / T));
	Cx0(i) = M / sqrt(4*pi*D*t(i)) .* exp(-xp.^2/(4*D*t(i)));
end
plot(t,Cx0)

xlabel('Time [hours]');
ylabel('Concentration at x = 0 [lbs/mile]'); 
	
print('-r600','-depsc','dub-hump.eps')