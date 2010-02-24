T = 4;                       % Tidal period (hours)
U0 = 0.1;                    % Mean velocity (miles/hour)
U1 = 1;                      % Fluxuating velocity amplitude (miles/hour)
M = 1;                       % Initial mass of polutant (lbs)
D = 0.03;                    % Diffusion coefficient (miles^2/hour)
x = linspace(-5,10,1000);    % Range of time values for plot data
t = [5 10:10:50];
%t = [1:.1:50];

C = zeros(length(t),length(x));
xpeak = linspace(0,0,length(t));
ypeak = xpeak;

for i=1:length(t)
	%xp = x - (U0 + U1*cos(2*pi*t(i)/T))*t(i);
	xp = x - (U0*t(i) + U1 * T / (2 * pi) * sin(2 * pi * t(i) / T));
	C(i,:) = M / sqrt(4*pi*D*t(i)) .* exp(-xp.^2/(4*D*t(i)));
	plot(x,C(i,:))
	xpeak(i) = x(C(i,:) == max(C(i,:)));
	ypeak(i) = .95*max(C(i,:));
	text(xpeak(i)+.05*range(x),...
		ypeak(i),['t = ', num2str(t(i))])
	hold all
end
%plot(xpeak,ypeak)

axis([min(x) max(x) 0 max(max(C))])

xlabel('Distance Downstream [miles]');
ylabel('Concentration [lbs/mile]'); 
	
print('-r600','-depsc','lprofs.eps')